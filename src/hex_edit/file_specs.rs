use std::str::FromStr;
use std::str::CharIndices;

use bitutils2::{BitIndex, BitField, BinRegex};
use xmlparser::{Tokenizer, Token, ElementEnd, StrSpan};
use log::*;

fn get_index_from_row_col(input: &str, row: usize, col: usize) -> usize {
    input.split_inclusive("\n").take(row - 1).map(|line| line.len()).sum::<usize>() + col - 1
}

// TODO: This is duplicated in expr
fn get_line_char(input: &str, index: usize) -> (usize, usize) {
    if let Some((n, line)) = input[..index].lines().enumerate().last() {
        (n, line.len())
    } else if index == 0 {
        (0, 0)
    } else {
        panic!("Empty input!: {}", input)
    }
}

const MAX_LINE_LENGTH: usize = 100;
const MIN_LINE_CONTEXT: usize = 5;
const LEFT_ELLIPSIS: &str = "... ";
const RIGHT_ELLIPSIS: &str = " ...";
const MID_ELLIPSES: &str = " ... ";

fn sample_line(line: &str, min_index: usize, max_index: usize) -> (String, usize, usize) {
    if line.len() < MAX_LINE_LENGTH {
        (line.to_string(), min_index, max_index)
    } else if MAX_LINE_LENGTH > max_index + MIN_LINE_CONTEXT {
        let elided = line[..MAX_LINE_LENGTH].to_string();
        (elided + "\x1b[0;96m" + RIGHT_ELLIPSIS + "\x1b[0m", min_index, max_index)
    } else if line.len() - MAX_LINE_LENGTH + MIN_LINE_CONTEXT < min_index {
        let offset = line.len() - MAX_LINE_LENGTH;
        let ellipsis = LEFT_ELLIPSIS.len();
        let elided = &line[offset..];
        ("\x1b[0;96m".to_string() + LEFT_ELLIPSIS + "\x1b[0m" + elided, min_index - offset + ellipsis, max_index - offset + ellipsis)
    } else if max_index - min_index + MIN_LINE_CONTEXT * 2 < MAX_LINE_LENGTH {
        let offset = (min_index + max_index - MAX_LINE_LENGTH) / 2 - MIN_LINE_CONTEXT;
        let ellipsis = LEFT_ELLIPSIS.len();
        info!("min: {} max: {} Offset: {}", min_index, max_index, offset);
        let elided = &line[offset..offset+MAX_LINE_LENGTH];
        ("\x1b[0;96m".to_string() + LEFT_ELLIPSIS + "\x1b[0m" + elided + "\x1b[0;96m" + RIGHT_ELLIPSIS + "\x1b[0m", min_index - offset + ellipsis, max_index - offset + ellipsis)
    } else {
        todo!()
    }
}

pub struct BinaryField {
    pub name: String,
    pub value: Option<String>,
    pub start: usize,
    pub span: usize
}

use crate::expr::{MyStrSpan, IndexRemap, ExprValue, Expr, ExprEvalError, ParseExprError};

#[derive(Clone)]
struct ExprWrapper {
    inner: Expr,
    remap: IndexRemap,
    default: bool
}

impl ExprWrapper {
    fn from_xml_str_span(input: &MyStrSpan) -> Result<ExprWrapper, ParseXmlError> {
        let mut new: String = input.as_str().to_string();
        let mut remap = IndexRemap::from_str_span(input);
        for (from, to) in vec![("&lt;", "<"), ("&gt;", ">"), ("&apos;", "'"), ("&quot;", "\""), ("&amp;", "&")] {
            let new_remap: IndexRemap;
            (new, new_remap) = IndexRemap::from_replacement(&new, from, to);
            remap.extend(new_remap);
        }
        remap = remap.inverted();
        match Expr::from_str_span(MyStrSpan::new(0, &new)) {
            Ok(expr) => {
                Ok(ExprWrapper {
                    inner: expr,
                    remap,
                    default: false
                })
            },
            Err(err) => {
                let mut new_annotations = vec![];
                for (s, rng) in err.annotations {
                    new_annotations.push((s.clone(), remap.apply_range(rng)));
                }
                Err(ParseXmlError{
                    details: err.details,
                    annotations: new_annotations,
                    help: err.help,
                    fname: None,
                    message: None
                })
            }
        }
    }

    pub fn from_default(expr: Expr) -> ExprWrapper {
        ExprWrapper {
            inner: expr,
            remap: IndexRemap::new(),
            default: true
        }
    }

    pub fn span(&self) -> Option<std::ops::Range<usize>> {
        if let Some(span) = &self.inner.span {
            Some(self.remap.apply_range(span.clone()))
        } else {
            None
        }
        
    }

    pub fn evaluate<'a>(&self, fname: &Option<Rc<String>>, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<ExprValue>, ExprEvalError> {
        self.evaluate_with_args(fname, &vec![], lookup)
    }

    pub fn evaluate_with_args<'a>(&self, fname: &Option<Rc<String>>, arguments: &Vec<ExprValue>, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<ExprValue>, ExprEvalError> {
        match self.inner.evaluate_with_args(arguments, lookup) {
            Ok(ev) => Ok(Some(ev)),
            Err(err) if err.is_lookup_error() => Ok(None),
            Err(mut err) => {
                if let Some(fname) = fname {
                    let text = std::fs::read_to_string(fname.to_string()).unwrap();
                    err.remap(&self.remap);
                    err.set_fname(fname.to_string());
                    err.set_context(&text);
                }
                Err(err)
                    
            }
        }
    }

    pub fn evaluate_expect_position<'a>(&self, fname: &Option<Rc<String>>, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<BitIndex>, ExprEvalError> {
        match self.evaluate(fname, lookup)? {
            Some(ev) => match ev {
                ExprValue::Position(bp) => Ok(Some(bp)),
                other => {
                    let mut err = ExprEvalError::data_type_mismatch(other.datatype_as_string(), "Position".to_string(), self.inner.span.clone());
                    if let Some(fname) = fname {
                        let text = std::fs::read_to_string(fname.to_string()).unwrap();
                        err.remap(&self.remap);
                        err.set_fname(fname.to_string());
                        err.set_context(&text);
                    }
                    Err(err)
                }
            },
            None => Ok(None)
        }
    }

    pub fn evaluate_expect_integer<'a>(&self, fname: &Option<Rc<String>>, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<i64>, ExprEvalError> {
        match self.evaluate(fname, lookup)? {
            Some(ev) => match ev {
                ExprValue::Integer(i) => Ok(Some(i)),
                other => {
                    let mut err = ExprEvalError::data_type_mismatch(other.datatype_as_string(), "Integer".to_string(), self.inner.span.clone());
                    if let Some(fname) = fname {
                        let text = std::fs::read_to_string(fname.to_string()).unwrap();
                        err.remap(&self.remap);
                        err.set_fname(fname.to_string());
                        err.set_context(&text);
                    }
                    Err(err)
                }
            },
            None => Ok(None)
        }
    }

    pub fn evaluate_expect_bool<'a>(&self, fname: &Option<Rc<String>>, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<bool>, ExprEvalError> {
        match self.evaluate(fname, lookup)? {
            Some(ev) => match ev {
                ExprValue::Bool(b) => Ok(Some(b)),
                other => {
                    let mut err = ExprEvalError::data_type_mismatch(other.datatype_as_string(), "Boolean".to_string(), self.inner.span.clone());
                    if let Some(fname) = fname {
                        let text = std::fs::read_to_string(fname.to_string()).unwrap();
                        err.remap(&self.remap);
                        err.set_fname(fname.to_string());
                        err.set_context(&text);
                    }
                    Err(err)
                }
            },
            None => Ok(None)
        }
    }

    pub fn vars(&self) -> HashSet<String> {
        self.inner.vars()
    }
}

// impl From<ParseAbstractIdentError> for ExprEvalError {
//     fn from(err: ParseAbstractIdentError) -> Self {
//         ExprEvalError::ParseError{
//             details: err.details
//         }
//     }
// }

#[derive(Debug)]
pub enum FileSpecErrorKind {
    IOError(std::io::Error),
    ExprEvalError(ExprEvalError),
    IdentNotFound(AbstractIdent),
    IdentNotValid(AbstractIdent),
    IdentParseError(ParseAbstractIdentError),
    ValueError(String, Option<std::ops::Range<usize>>)
}

#[derive(Debug)]
pub struct FileSpecError {
    kind: FileSpecErrorKind,
    fname: Option<String>,
    message: Option<String>,
    help: Option<String>
}

impl FileSpecError {
    pub fn new(kind: FileSpecErrorKind) -> FileSpecError {
        FileSpecError {
            kind,
            fname: None,
            message: None,
            help: None
        }
    }

    pub fn ident_not_found(ident: AbstractIdent) -> FileSpecError {
        FileSpecError::new(FileSpecErrorKind::IdentNotFound(ident))
    }

    pub fn ident_not_valid(ident: AbstractIdent) -> FileSpecError {
        FileSpecError::new(FileSpecErrorKind::IdentNotValid(ident))
    }

    pub fn expr_eval_error(err: ExprEvalError) -> FileSpecError {
        FileSpecError::new(FileSpecErrorKind::ExprEvalError(err))
    }

    pub fn value_error(message: String, rng: Option<std::ops::Range<usize>>) -> FileSpecError {
        FileSpecError::new(FileSpecErrorKind::ValueError(message, rng))
    }

    pub fn set_fname(&mut self, fname: String) {
        // if let FileSpecErrorKind::ExprEvalError(ref mut err) = self.kind {
        //     err.fname = Some(fname.clone());
        // }
        self.fname = Some(fname);
        
    }

    pub fn load_context(&mut self, fname: String) -> Result<(), std::io::Error> {
        let text = std::fs::read_to_string(&fname.as_str())?;
        self.set_fname(fname);
        self.set_context(&text);
        Ok(())
    }
    
                                

    fn details(&self) -> String {
        match &self.kind {
            FileSpecErrorKind::IOError(err) => format!("I/O Error: {}", err),
            FileSpecErrorKind::IdentNotFound(err) => format!("Identifier not found: {}", err),
            FileSpecErrorKind::IdentNotValid(err) => format!("Identifier not valid: {}", err),
            FileSpecErrorKind::IdentParseError(err) => format!("Identifier parse error: {}", err),
            FileSpecErrorKind::ValueError(message, rng) => format!("Value Error: {}", message),
            FileSpecErrorKind::ExprEvalError(err) => err.details()
        }
    }

    fn annotations(&self) -> Vec<(Option<String>, std::ops::Range<usize>)> {
        match &self.kind {
            FileSpecErrorKind::ExprEvalError(err) => err.annotations(),
            FileSpecErrorKind::ValueError(_, Some(rng)) => {
                vec![
                    (None, rng.clone())
                ]
            }
            _ => todo!()
        }
    }

    pub fn set_context(&mut self, context: &str) {
        if self.message.is_some() {
            return;
        }
        let mut msg = format!("\x1b[0;31mERROR:\x1b[0m {}", self.details()).to_string();
        if let Some(fname) = &self.fname {
            let s = format!("\n\x1b[0;96m -->\x1b[0m {}", fname).to_string();
            msg.push_str(&s)
        }
        let mut max_line_num = 0;
        for (_, r) in &self.annotations() {
            let (line, _) = get_line_char(context, r.start);
            if line > max_line_num {
                max_line_num = line;
            }
            let (line, _) = get_line_char(context, r.end);
            if line > max_line_num {
                max_line_num = line;
            }
        }
        let line_num_length = format!("{}", max_line_num).len();
        for (info, r) in &self.annotations() {
            let (line1, col1) = get_line_char(context, r.start);
            let (line2, col2) = get_line_char(context, r.end);
            let line_text = context.lines().nth(line1).unwrap();
            if let Some(info_string) = info {
                msg.push_str("\n");
                msg.push_str(&info_string);
            }
            let (line_text, col1, col2) = sample_line(line_text, col1, col2);
            let s = format!("\n\x1b[0;96m{:2$} | \x1b[0m{}", line1 + 1, line_text, line_num_length).to_string();
            msg.push_str(&s);
            let s = format!("\n{}\x1b[0;31m{}\x1b[0m", " ".repeat(col1 + line_num_length + 3), "~".repeat(col2 - col1)).to_string();
            msg.push_str(&s);
        }
        if let Some(help) = &self.help {
            msg.push_str(format!("\n\x1b[0;32mHELP:\x1b[0m {}", help).as_str());
        }
        self.message = Some(msg)
    }

    // pub fn try_get_message(&self) -> Option<String> {
    //     self.message.clone()
    // }

    pub fn get_message(&mut self, context: &str) -> String {
        if self.message.is_none() {
            self.set_context(context);
        }
        self.message.as_ref().unwrap().clone()
    }

    pub fn try_get_message(&self) -> Option<String> {
        match &self.kind {
            FileSpecErrorKind::ExprEvalError(err) => {
                err.try_get_message()
            },
            _ => {
                self.message.clone()
            }
        }
    }
}

impl From<std::io::Error> for FileSpecError {
    fn from(err: std::io::Error) -> Self {
        FileSpecError::new(FileSpecErrorKind::IOError(err))
    }
}

impl From<ExprEvalError> for FileSpecError {
    fn from(err: ExprEvalError) -> Self {
        FileSpecError::new(FileSpecErrorKind::ExprEvalError(err))
    }
}

impl From<ParseAbstractIdentError> for FileSpecError {
    fn from(err: ParseAbstractIdentError) -> Self {
        FileSpecError::new(FileSpecErrorKind::IdentParseError(err))
    }
}

impl std::fmt::Display for FileSpecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            FileSpecErrorKind::IOError(err) => write!(f, "I/O Error: {}", err),
            FileSpecErrorKind::ExprEvalError(err) => write!(f, "Expression Eval Error: {}", err),
            FileSpecErrorKind::IdentNotFound(err) => write!(f, "Identifier not found: {}", err),
            FileSpecErrorKind::IdentNotValid(err) => write!(f, "Identifier not valid: {}", err),
            FileSpecErrorKind::IdentParseError(err) => write!(f, "Identifier parse error: {}", err),
            FileSpecErrorKind::ValueError(message, _) => write!(f, "Value Error: {}", message)
        }
    }    
}

#[derive(Debug)]
pub struct FileParseError {
    details: String
}

impl FileParseError {
    pub fn new(msg: String) -> FileParseError {
        FileParseError{details: msg}
    }
}

impl std::fmt::Display for FileParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.details)
    }    
}

// #[derive(Debug)]
// pub struct ParseXmlError {
//     details: String
// }

#[derive(Debug)]
pub struct ParseXmlError {
    details: String,
    annotations: Vec<(Option<String>, std::ops::Range<usize>)>,
    help: Option<String>,
    fname: Option<String>,
    message: Option<String>
}

impl ParseXmlError {
    pub fn new(details: String) -> ParseXmlError {
        ParseXmlError{
            details, 
            annotations: Vec::new(), 
            help: None, 
            fname: None,
            message: None
        }
    }

    pub fn missing_required_attribute(element: &MyStrSpan, attr_name: String) -> ParseXmlError {
        let message = format!("'{}' attribute is required for '{}' element, but was not specified", attr_name, element.as_str()).to_string();
        let mut err = ParseXmlError::new(message);
        err.push_annotation(element.range(), Some(format!("'{}' attribute was not specified for this element", attr_name).to_string()));
        err.set_help(format!("Try adding a '{}' attribute or removing the element", attr_name).to_string());
        err
    }

    pub fn push_annotation(&mut self, range: std::ops::Range<usize>, message: Option<String>) {
        self.annotations.push((message, range));
    }

    pub fn set_help(&mut self, text: String) {
        self.help = Some(text);
    }

    pub fn set_fname(&mut self, fname: String) {
        self.fname = Some(fname);
    }

    pub fn get_message(&mut self, context: &str) -> String {
        if self.message.is_none() {
            self.set_context(context);
        }
        self.message.as_ref().unwrap().clone()
    }

    pub fn set_context(&mut self, context: &str) {
        if self.message.is_some() {
            return;
        }
        let mut msg = format!("\x1b[0;31mERROR:\x1b[0m {}", self.details).to_string();
        if let Some(fname) = &self.fname {
            let s = format!("\n\x1b[0;96m -->\x1b[0m {}", fname).to_string();
            msg.push_str(&s)
        }
        let mut max_line_num = 0;
        for (_, r) in &self.annotations {
            let (line, _) = get_line_char(context, r.start);
            if line > max_line_num {
                max_line_num = line;
            }
            let (line, _) = get_line_char(context, r.end);
            if line > max_line_num {
                max_line_num = line;
            }
        }
        let line_num_length = format!("{}", max_line_num).len();
        for (info, r) in &self.annotations {
            let (line1, col1) = get_line_char(context, r.start);
            let (line2, col2) = get_line_char(context, r.end);
            let line_text = context.lines().nth(line1).unwrap();
            if let Some(info_string) = info {
                msg.push_str("\n");
                msg.push_str(&info_string);
            }
            let (line_text, col1, col2) = sample_line(line_text, col1, col2);
            let s = format!("\n\x1b[0;96m{:2$} | \x1b[0m{}", line1 + 1, line_text, line_num_length).to_string();
            msg.push_str(&s);
            let s = format!("\n{}\x1b[0;31m{}\x1b[0m", " ".repeat(col1 + line_num_length + 3), "~".repeat(col2 - col1)).to_string();
            msg.push_str(&s);
        }
        if let Some(help) = &self.help {
            msg.push_str(format!("\n\x1b[0;32mHELP:\x1b[0m {}", help).as_str());
        }
        self.message = Some(msg)
    }

}

impl std::fmt::Display for ParseXmlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.details)
    }    
}

impl From<ParseExprError> for ParseXmlError {
    fn from(err: ParseExprError) -> Self {
        ParseXmlError{
            details: err.details,
            annotations: err.annotations,
            help: err.help,
            fname: None,
            message: None
        }
    }
}

fn convert_xml_symbols(s: &str) -> String {
    let out = s.replace("&lt;", "<");
    out
}

use crate::Endianness;
use crate::{BinaryFormat, UIntFormat};

enum BitFormat {
    Standard(BinaryFormat),
    UInt(usize),
    IInt(usize)
}

pub struct UnparsedBinaryField {
    pub name: String,
    pub start: BitIndex,
    pub span: BitIndex,
    pub dtype: String,
    pub endian: Endianness
}

impl UnparsedBinaryField {
    pub fn parse(&self, fm: &mut crate::hex_edit::FileManager) -> std::io::Result<ExprValue> {
        // assert_eq!(self.start.bit(), 0);
        // assert_eq!(self.span.bit(), 0);
        let mut buffer = vec![0; self.end().ceil().byte() - self.start.byte()];
        fm.get_bytes(self.start.byte(), &mut buffer)?;
        let mut bf = BitField::from_vec(buffer) << self.start.bit() as usize;
        // match self.endian {
        //     Endianness::Little => {
        //         bf = (bf << self.start.bit() as usize).truncate(&self.span);
        //     },
        //     Endianness::Big | Endianness::Network => {
        //         bf = (bf << self.start.bit() as usize).truncate(&self.span);
        //     }
        // }
        bf.truncate(&self.span);

        match self.dtype.as_str() {
           "S" | "s" => {
                assert_eq!(bf.len().bit(), 0);
                let buffer = bf.into_boxed_slice().unwrap().into_vec();
                let s = std::str::from_utf8(&buffer).unwrap().to_string();
                return Ok(ExprValue::String(s))
            },
            "U" | "u" => {
                assert!(bf.len() < BitIndex::bytes(8)); // Integers larger than 64 bit not yet supported
                let uint = match self.endian {
                    Endianness::Little => {
                        bf.pad_unsigned_le(BitIndex::bytes(8));
                        u64::from_le_bytes(bf.into_slice().unwrap())
                    },
                    Endianness::Big | Endianness::Network => {
                        bf.pad_unsigned_be(BitIndex::bytes(8));
                        u64::from_be_bytes(bf.into_slice().unwrap())
                    }
                };
                return Ok(ExprValue::Integer(uint as i64))
            },
            "B" => {
                Ok(ExprValue::Bytes(bf))
                // Ok(ExprValue::Integer(0))
            }
            _ => todo!("Datatype not supported: {}", self.dtype)
        }


        // let format = match self.dtype.as_str() {
        //     "U" => BinaryFormat::UInt(match self.span.byte() {
        //         1 => UIntFormat::U8,
        //         2 => UIntFormat::U16,
        //         4 => UIntFormat::U32,
        //         8 => UIntFormat::U64,
        //         _ => todo!()
        //     }),
        //     _ => todo!()
        // };

        // match format {
        //     BinaryFormat::UInt(UIntFormat::U32) => {
        //         let v = u32::from_be_bytes([buffer[0], buffer[1], buffer[2], buffer[3]]);
        //         //return Ok(ExprValue::Integer(v as i64))
        //         return Ok(ExprValue::Integer(v as i64))
        //     },
        //     _ => todo!()
        // }
    }

    pub fn end(&self) -> BitIndex {
        self.start + self.span
    }
}

#[derive(Clone)]
struct IntegerEnum {
    cases: Vec<(i64, String)>,
    default: Option<String>
}

impl IntegerEnum {
    fn lookup(&self, value: i64) -> Option<String> {
        for (i, s) in &self.cases {
            if value == *i {
                return Some(s.clone())
            }
        }
        return self.default.clone()
    }

    fn from_xml_object(mut obj: XmlObject) -> Result<IntegerEnum, ParseXmlError> {
        if obj.element.as_str() != "enum" {
            panic!("Trying to parse enum from non-enum element")
        }
        if !obj.attrs.is_empty() {
            panic!("'enum' does not take any attributes")
        }
        let mut cases = vec![];
        let mut default = None;
        for mut child in obj.children {
            match child.element.as_str() {
                "enum-case" => {
                    let s = child.attrs.remove("index").unwrap().as_str();
                    let i: i64;
                    if let Some(s) = s.strip_prefix("0x") {
                        i = i64::from_str_radix(s, 16).unwrap()
                    } else {
                        i = i64::from_str(s).unwrap();
                    }
                    let value = child.attrs.remove("value").unwrap().as_str().to_string();
                    cases.push((i, value));
                },
                "enum-else" => {
                    if default.is_none() {
                        let value = child.attrs.remove("value").unwrap().as_str().to_string();
                        default = Some(value)
                    } else {
                        panic!("Only one enum-else allowed per enum")
                    }
                },
                e => panic!("Invalid enum child: {}", e)
            }
        }
        Ok(IntegerEnum{cases, default})
    }
}

#[derive(Clone)]
struct FieldSpec {
    id: String,
    name: String,
    offset: ExprWrapper,
    length: ExprWrapper,
    alignment: ExprWrapper,
    alignment_base: ExprWrapper,
    valid: Option<ExprWrapper>,
    dtype: String,
    endian: Endianness,
    enumeration: Option<IntegerEnum>,
    units: Option<String>
}

impl FieldSpec {

    fn format_data(&self, value: ExprValue) -> String {
        match value {
            ExprValue::String(s) => s,
            ExprValue::Integer(i) => {
                if let Some(e) = &self.enumeration {
                    if let Some(s) = e.lookup(i) {
                        format!("{} ({})", i, s).to_string()
                    } else {
                        format!("{} (Invalid)", i).to_string()
                    }
                } else if let Some(u) = &self.units {
                    format!("{} {}", i, u).to_string()
                } else {
                    format!("{}", i).to_string()
                }
            },
            ExprValue::Position(bi) => format!("{}B{}b", bi.byte(), bi.bit()).to_string(),
            ExprValue::Bool(b) => {
                if b {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            },
            ExprValue::Bytes(bf) => format!("{:?}", bf).to_string() // TODO
        }
    }

    fn from_xml_object(mut obj: XmlObject) -> Result<FieldSpec, ParseXmlError> {
        if obj.element.as_str() != "field" {
            panic!("Trying to parse field from non-field element")
        }
        let length = ExprWrapper::from_xml_str_span(&obj.remove_required_attr("length")?)?;
        let id = convert_xml_symbols(obj.attrs.remove("id").unwrap().as_str());
        let name = convert_xml_symbols(obj.attrs.remove("name").unwrap().as_str());
        let dtype = convert_xml_symbols(obj.attrs.remove("dtype").unwrap().as_str());
        let endian = match obj.attrs.remove("endian") {
            None => Endianness::Network,
            Some(s) => match convert_xml_symbols(s.as_str()).as_str() {
                "big" => Endianness::Big,
                "little" => Endianness::Little,
                _ => panic!("Unrecognized endianness: '{}'", s.as_str())
            }
        };
        let offset = match obj.attrs.remove("offset") {
            Some(s) => ExprWrapper::from_xml_str_span(&s)?,
            None => ExprWrapper::from_default(Expr::value(ExprValue::Position(BitIndex::zero())))
        };
        let alignment = match obj.attrs.remove("align") {
            Some(s) => ExprWrapper::from_xml_str_span(&s)?,
            None => ExprWrapper::from_default(Expr::value(ExprValue::Position(BitIndex::bytes(1))))
        };
        let alignment_base = match obj.attrs.remove("align-base") {
            Some(s) => ExprWrapper::from_xml_str_span(&s)?,
            None => ExprWrapper::from_default(Expr::value(ExprValue::Position(BitIndex::zero())))
        };
        let valid = match obj.attrs.remove("valid") {
            Some(s) => Some(ExprWrapper::from_xml_str_span(&s)?),
            None => None
        };
        let units = match obj.attrs.remove("units") {
            Some(s) => Some(convert_xml_symbols(s.as_str()).to_string()),
            None => None
        };
        let mut enumeration = None;
        for child in obj.children {
            match child.element.as_str() {
                "enum" => {
                    if enumeration.is_none() {
                        enumeration = Some(IntegerEnum::from_xml_object(child)?);
                    } else {
                        panic!("Only one enum child allowed per field")
                    }
                },
                c => panic!("Invalid field child: {}", c)
            }
        }
        Ok(FieldSpec {
            id,
            name,
            offset,
            length,
            alignment,
            alignment_base,
            valid,
            dtype,
            endian,
            enumeration,
            units
        })
    }
}

#[derive(Clone)]
enum SectionType {
    Header,
    Body,
    Footer
}


#[derive(Clone)]
struct SectionSpec {
    length: LengthPolicy,
    section_type: SectionType,
    fields: Vec<FieldSpec>,
    id_map: HashMap<String, usize>, // Map of ID to index in fields array
}

impl SectionSpec {


    fn get_field_info(&self, field_id: String) -> Result<(String, String, Endianness), ()> {
        match self.id_map.get(&field_id) {
            Some(index) => Ok((self.fields[*index].name.clone(), self.fields[*index].dtype.clone(), self.fields[*index].endian)),
            None => Err(()) 
        }
    }

    fn format_field_data(&self, field_id: String, value: ExprValue) -> String {
        match self.id_map.get(&field_id) {
            Some(index) => self.fields[*index].format_data(value),
            None => panic!("Field does not exist")
        }
    }

}

use std::rc::Rc;
use std::collections::{HashSet, HashMap};

#[derive(Debug)]
enum DepResult<T> {
    Success(T),
    Incomplete(HashSet<AbstractIdent>),
    MightExist(HashSet<AbstractIdent>),
    DoesNotExist
}

#[derive(Debug)]
struct DependencyReport<T> {
    parents_children: Vec<(HashSet<AbstractIdent>, HashSet<AbstractIdent>)>,
    result: DepResult<T>
}

impl<T> DependencyReport<T> {

    fn success(result: T) -> DependencyReport<T> {
        DependencyReport {
            parents_children: Vec::new(),
            result: DepResult::Success(result)
        }
    }

    fn incomplete(requirements: HashSet<AbstractIdent>) -> DependencyReport<T> {
        DependencyReport {
            parents_children: Vec::new(),
            result: DepResult::Incomplete(requirements)
        }
    }

    fn might_exist(requirements: HashSet<AbstractIdent>) -> DependencyReport<T> {
        DependencyReport {
            parents_children: Vec::new(),
            result: DepResult::MightExist(requirements)
        }
    }

    fn does_not_exist() -> DependencyReport<T> {
        DependencyReport {
            parents_children: Vec::new(),
            result: DepResult::DoesNotExist
        }
    }

    fn add_pc_pairs(&mut self, parents: HashSet<AbstractIdent>, children: HashSet<AbstractIdent>) {
        self.parents_children.push((parents, children))
    }
}

use lazy_static::lazy_static;
lazy_static! {
    static ref KEY_RE: regex::Regex = regex::Regex::new(r"^(?<structure>\w+)\.(?<attribute>\w+)(?<index>[\d+])?$").unwrap();
    static ref FIELD_KEY_RE: regex::Regex = regex::Regex::new(r"^(?<structure>\w+):(?<field>\w+)(\.(?<attribute>\w+))?$").unwrap();

    static ref STRUCT_ATTR_RE: regex::Regex = regex::Regex::new(r"^(?<structure>\w+)\.(?<attribute>\w+)(?<index>[\d+])?$").unwrap();
    static ref FIELD_ATTR_RE: regex::Regex = regex::Regex::new(r"^(?<structure>\w+):(?<field>\w+)(\.(?<attribute>\w+))?$").unwrap();
}

#[derive(Clone)]
enum LengthPolicy {
    FitContents,
    Expand,
    Expr(ExprWrapper)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructureIdent {
    id: String,
    index: Vec<usize>
}

impl std::fmt::Display for StructureIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl StructureIdent {
    fn new(id: String) -> StructureIdent {
        StructureIdent{id, index: vec![]}
    }

    fn new_indexed(id: String, index: Vec<usize>) -> StructureIdent {
        StructureIdent{id, index}
    }

    fn unindexed(&self) -> StructureIdent {
        StructureIdent::new(self.id.clone())
    }

    fn get_child_ident(&self, child_id: String, rel_index: Vec<usize>) -> StructureIdent {
        let mut child_index = self.index.clone();
        child_index.extend(rel_index);
        StructureIdent::new_indexed(child_id, child_index)
    }

    fn get_field_ident(&self, field_id: String) -> FieldIdent {
        FieldIdent {structure: self.clone(), id: field_id}
    }

    fn get_attr_ident(&self, attr: StructureAttrType) -> StructureAttrIdent {
        StructureAttrIdent::new(self.clone(), attr)
    }

    fn is_root(&self) -> bool {
        self.index.is_empty()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FieldIdent {
    pub structure: StructureIdent,
    pub id: String
}

impl std::fmt::Display for FieldIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.structure, self.id)
    }
}

impl FieldIdent {
    fn new(structure: StructureIdent, id: String) -> FieldIdent {
        FieldIdent{ structure, id }
    }

    fn get_attr_ident(&self, attr: FieldAttrType) -> FieldAttrIdent {
        FieldAttrIdent::new(self.clone(), attr)
    }

    fn to_abstract(self) -> AbstractIdent {
        AbstractIdent::Field(self)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum StructureAttrType {
    Align,
    AlignBase,
    Position,
    StartPad,
    Start,
    ContentsLength,
    SpareLength,
    Length,
    End,
    Repetitions,
    SwitchValue,
    SwitchIndex,
    SwitchCase(usize),
    Break(usize)
}

impl std::fmt::Display for StructureAttrType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            StructureAttrType::Align => write!(f, "align"),
            StructureAttrType::AlignBase => write!(f, "align_base"),
            StructureAttrType::Position => write!(f, "position"),
            StructureAttrType::StartPad => write!(f, "start_pad"),
            StructureAttrType::Start => write!(f, "start"),
            StructureAttrType::ContentsLength => write!(f, "contents_length"),
            StructureAttrType::SpareLength => write!(f, "spare_length"),
            StructureAttrType::Length => write!(f, "length"),
            StructureAttrType::End => write!(f, "end"),
            StructureAttrType::Repetitions => write!(f, "repetitions"),
            StructureAttrType::SwitchValue => write!(f, "switch_value"),
            StructureAttrType::SwitchIndex => write!(f, "switch_index"),
            StructureAttrType::SwitchCase(n) => write!(f, "switch_case[{}]", n),
            StructureAttrType::Break(n) => write!(f, "break[{}]", n)
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct StructureAttrIdent {
    structure: StructureIdent,
    attr: StructureAttrType
}

impl StructureAttrIdent {
    fn new(structure: StructureIdent, attr: StructureAttrType) -> StructureAttrIdent {
        StructureAttrIdent{structure, attr}
    }

    fn to_abstract(self) -> AbstractIdent {
        AbstractIdent::StructureAttr(self)
    }
}

impl std::fmt::Display for StructureAttrIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}", self.structure, self.attr)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum FieldAttrType {
    Align,
    AlignBase,
    Position,
    Offset,
    StartPad,
    Start,
    Length,
    End,
    Valid,
    SearchStart,
    SearchN,
    AnchorOffset
}

impl std::fmt::Display for FieldAttrType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            FieldAttrType::Align => write!(f, "align"),
            FieldAttrType::AlignBase => write!(f, "align_base"),
            FieldAttrType::Position => write!(f, "position"),
            FieldAttrType::Offset => write!(f, "offset"),
            FieldAttrType::StartPad => write!(f, "start_pad"),
            FieldAttrType::Start => write!(f, "start"),
            FieldAttrType::Length => write!(f, "length"),
            FieldAttrType::End => write!(f, "end"),
            FieldAttrType::Valid => write!(f, "valid"),
            FieldAttrType::SearchStart => write!(f, "search_start"),
            FieldAttrType::SearchN => write!(f, "search_n"),
            FieldAttrType::AnchorOffset => write!(f, "anchor_offset"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct FieldAttrIdent {
    field: FieldIdent,
    attr: FieldAttrType
}

impl std::fmt::Display for FieldAttrIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}.{}", self.field.structure, self.field.id, self.attr)
    }
}

impl FieldAttrIdent {
    fn new(field: FieldIdent, attr: FieldAttrType) -> FieldAttrIdent {
        FieldAttrIdent{ field, attr }
    }

    fn to_abstract(self) -> AbstractIdent {
        AbstractIdent::FieldAttr(self)
    }
}

#[derive(Debug)]
pub struct ParseAbstractIdentError {
    details: String
}

impl ParseAbstractIdentError {
    fn new(details: String) -> ParseAbstractIdentError {
        ParseAbstractIdentError{details}
    }
}

impl std::fmt::Display for ParseAbstractIdentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.details)
    }    
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum AbstractIdent {
    StructureAttr(StructureAttrIdent),
    Field(FieldIdent),
    FieldAttr(FieldAttrIdent)
}

impl std::fmt::Display for AbstractIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AbstractIdent::StructureAttr(i) => write!(f, "{}", i),
            AbstractIdent::Field(i) => write!(f, "{}", i),
            AbstractIdent::FieldAttr(i) => write!(f, "{}", i),
        }
    }
}

impl FromStr for AbstractIdent {
    type Err = ParseAbstractIdentError;

    fn from_str(s: &str) -> Result<AbstractIdent, ParseAbstractIdentError> {
        if let Some(caps) = STRUCT_ATTR_RE.captures(&s) {
            if let (Some(structure_match), Some(attr_match)) = (caps.name("structure"), caps.name("attribute")) {
                let structure = StructureIdent::new(structure_match.as_str().to_string());
                let attr = match attr_match.as_str() {
                    "alignment" => StructureAttrType::Align,
                    "alignment_base" => StructureAttrType::AlignBase,
                    "position" => StructureAttrType::Position,
                    "start_pad" => StructureAttrType::StartPad,
                    "start" => StructureAttrType::Start,
                    "contents_length" => StructureAttrType::ContentsLength,
                    "spare_length" => StructureAttrType::SpareLength,
                    "length" => StructureAttrType::Length,
                    "end" => StructureAttrType::End,
                    "repetitions" => StructureAttrType::Repetitions,
                    "switch_value" => StructureAttrType::SwitchValue,
                    "switch_index" => StructureAttrType::SwitchIndex,
                    "switch_case" => {
                        if let Some(index_match) = caps.name("index") {
                            let index = index_match.as_str().parse::<usize>().unwrap();
                            StructureAttrType::SwitchCase(index)
                        } else {
                            return Err(ParseAbstractIdentError::new("Switch case without index".to_string()))
                        }
                    },
                    _ => return Err(ParseAbstractIdentError::new(format!("Unrecognized structure attribute: '{}'", attr_match.as_str()).to_string()))
                };
                Ok(structure.get_attr_ident(attr).to_abstract())
                
            } else {
                unreachable!()
            }
        } else if let Some(caps) = FIELD_ATTR_RE.captures(&s) {
            if let (Some(structure_match), Some(field_match)) = (caps.name("structure"), caps.name("field")) {
                let structure = StructureIdent::new(structure_match.as_str().to_string());
                let field = FieldIdent::new(structure, field_match.as_str().to_string());
                if let Some(attr_match) = caps.name("attribute") {
                    let attr = match attr_match.as_str() {
                        "alignment" => FieldAttrType::Align,
                        "alignment_base" => FieldAttrType::AlignBase,
                        "position" => FieldAttrType::Position,
                        "start_pad" => FieldAttrType::StartPad,
                        "start" => FieldAttrType::Start,
                        "length" => FieldAttrType::Length,
                        "end" => FieldAttrType::End,
                        "valid" => FieldAttrType::Valid,
                        _ => return Err(ParseAbstractIdentError::new(format!("Unrecognized field attribute: '{}'", attr_match.as_str()).to_string()))
                    };
                    Ok(field.get_attr_ident(attr).to_abstract())
                } else {
                    Ok(field.to_abstract())
                }
            } else {
                unreachable!()
            }
        } else {
            Err(ParseAbstractIdentError::new(format!("Unrecognized ident: '{}'", s).to_string()))
        }
    }
}

enum DataSource {
    Field(UnparsedBinaryField),
    Search{start: BitIndex, n: usize, find: String},
    Given(ExprValue)
}

enum PartiallyResolvedStructureType<'a> {
    UnresolvedSection{initial: Rc<SectionSpec>},
    Segment,
    Addressed{position: Rc<ExprWrapper>, pss: Rc<RefCell<PartiallyResolvedStructure<'a>>>},
    Sequence(Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>),
    Switch{value: Rc<ExprWrapper>, cases: Vec<(Rc<ExprWrapper>, Rc<Structure>)>, default: Rc<Structure>, next_case: usize, prs: Option<Rc<RefCell<PartiallyResolvedStructure<'a>>>>},
    Repeat{n: Rc<ExprWrapper>, structure: Rc<Structure>, seq: Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>, finalized: bool}, // seq is used to store the actual structure instances once "n" is determined
    RepeatUntil{end: Rc<ExprWrapper>, structure: Rc<Structure>, seq: Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>, finalized: bool},
    Chain{next: Rc<ExprWrapper>, structure: Rc<Structure>, prs: Rc<RefCell<PartiallyResolvedStructure<'a>>>, seq: Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>, finalized: bool},
    Assembly{segments: Vec<String>, structure: Rc<Structure>, prs: Rc<RefCell<PartiallyResolvedStructure<'a>>>}
}

impl<'a> PartiallyResolvedStructureType<'a> {
    /// Returns a string representing `self`'s structure type for use in error/debug messages
    fn name(&self) -> String {
        match self {
            PartiallyResolvedStructureType::UnresolvedSection{..} => "section".to_string(),
            PartiallyResolvedStructureType::Segment{..} => "segment".to_string(),
            PartiallyResolvedStructureType::Addressed{..} => "addressed".to_string(),
            PartiallyResolvedStructureType::Sequence{..} => "sequence".to_string(),
            PartiallyResolvedStructureType::Switch{..} => "switch".to_string(),
            PartiallyResolvedStructureType::Repeat{..} => "repeat".to_string(),
            PartiallyResolvedStructureType::RepeatUntil{..} => "repeat-until".to_string(),
            PartiallyResolvedStructureType::Chain{..} => "chain".to_string(),
            PartiallyResolvedStructureType::Assembly{..} => "assembly".to_string(),
        }
    }
}

struct PartiallyResolvedStructure<'a> {
    id: StructureIdent,
    original: Rc<Structure>,
    parent_id: StructureIdent,
    alignment: ExprWrapper,
    alignment_base: ExprWrapper,
    // start: Option<BitIndex>,
    length: LengthPolicy,
    // end: Option<BitIndex>,
    stype: Box<PartiallyResolvedStructureType<'a>>,
    import_monikers: HashMap<String, Option<StructureIdent>>,
    exports: HashSet<String>,
    inherited_monikers: HashMap<String, FieldIdent>,
    index_path: Vec<usize>,
    fname: Option<Rc<String>>
}

type PrsMap<'a> = HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>;

impl<'a> PartiallyResolvedStructure<'a> {

    fn new(original: Rc<Structure>, parent_id: StructureIdent, 
        prs_map: &mut PrsMap<'a>) -> Rc<RefCell<PartiallyResolvedStructure<'a>>> {
        PartiallyResolvedStructure::new_indexed(original, parent_id, vec![], prs_map)
    }

    fn new_indexed(original: Rc<Structure>, parent_id: StructureIdent, index: Vec<usize>, prs_map: &mut PrsMap<'a>) -> Rc<RefCell<PartiallyResolvedStructure<'a>>> {
        // let mut id_string = original.id.clone();
        // for i in &index {
        //     id_string.push_str(&format!("_{}", i));
        // }

        let id = StructureIdent::new_indexed(original.id.clone(), index.clone());

        let mut import_monikers = HashMap::new();
        let stype = match &original.stype {
            StructureType::Section(section) => {
                PartiallyResolvedStructureType::UnresolvedSection{initial: section.clone()}
            },
            StructureType::Segment => {
                PartiallyResolvedStructureType::Segment
            },
            StructureType::Addressed{position, content} => {
                for name in content.exports.keys() {
                    import_monikers.insert(name.clone(), None);
                }
                let mut child_index = index.clone();
                child_index.push(0);
                let pss = PartiallyResolvedStructure::new_indexed(content.clone(), id.clone(), child_index, prs_map);
                PartiallyResolvedStructureType::Addressed{position: position.clone(), pss}
            },
            StructureType::Sequence(seq) => {
                for structure in seq {
                    for name in structure.exports.keys() {
                        import_monikers.insert(name.clone(), None);
                    }
                }
                
                PartiallyResolvedStructureType::Sequence(seq.iter().enumerate().map(
                    |(i, s)| {
                        let mut child_index = index.clone();
                        child_index.push(i);
                        PartiallyResolvedStructure::new_indexed(s.clone(), id.clone(), child_index, prs_map)
                    }).collect())
            },
            StructureType::Switch{value, cases, default} => {
                for (_, structure) in cases {
                    for name in structure.exports.keys() {
                        import_monikers.insert(name.clone(), None);
                    }
                }
                for name in default.exports.keys() {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::Switch{value: value.clone(), cases: cases.iter().map(|(e, c)| (e.clone(), c.clone())).collect(), default: default.clone(), next_case: 0, prs: None}
            },
            StructureType::Repeat{n, structure} => {
                for name in structure.exports.keys() {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::Repeat{n: n.clone(), structure: structure.clone(), seq: Vec::new(), finalized: false}
            },
            StructureType::RepeatUntil{end, structure} => {
                for name in structure.exports.keys() {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::RepeatUntil{end: end.clone(), structure: structure.clone(), seq: Vec::new(), finalized: false}
            },
            StructureType::Chain{next, structure} => {
                for name in structure.exports.keys() {
                    import_monikers.insert(name.clone(), None);
                }
                let mut prs_index = index.clone();
                prs_index.push(0);
                let prs = PartiallyResolvedStructure::new_indexed(structure.clone(), id.clone(), prs_index, prs_map);
                let structure = Structure::wrap_addressed(structure.clone(), Expr::value(ExprValue::Bool(false))); // The "address" put in here 
                // must never be used, hence why it's a boolean
                PartiallyResolvedStructureType::Chain{next: next.clone(), structure: Rc::new(structure), prs, seq: Vec::new(), finalized: false}
            },
            StructureType::Assembly{segments, structure} => {
                for name in structure.exports.keys() {
                    import_monikers.insert(name.clone(), None);
                }
                let mut child_index = index.clone();
                child_index.push(0);
                let prs = PartiallyResolvedStructure::new_indexed(structure.clone(), id.clone(), child_index, prs_map);
                PartiallyResolvedStructureType::Assembly{segments: segments.clone(), structure: structure.clone(), prs}
            }
        };

        let prs = Rc::new(RefCell::new(PartiallyResolvedStructure {
            id: id.clone(),
            original: original.clone(),
            parent_id,
            alignment: original.alignment.clone(),
            alignment_base: original.alignment_base.clone(),
            length: original.length.clone(),
            stype: Box::new(stype),
            import_monikers,
            exports: original.exports.keys().map(|v| v.to_string()).collect(),
            inherited_monikers: HashMap::new(),
            index_path: index,
            fname: original.filename.clone()
        }));

        prs_map.insert(id.clone(), prs.clone());
        prs
    }

    fn monikers_for_child(&self) -> HashMap<String, FieldIdent> {
        let mut child_monikers = self.inherited_monikers.clone();
        for key in self.original.def_fields.keys() {
            child_monikers.insert(key.clone(), self.id.clone().get_field_ident(key.clone()));
        }
        for key in self.import_monikers.keys().clone() {
            if !child_monikers.contains_key(key) {
                child_monikers.insert(key.to_string(), self.id.get_field_ident(key.clone()));
            }
        }
        child_monikers
    }

    fn initialize_inherited_monikers(&mut self, prs_map: &PrsMap<'a>,
            monikers: HashMap<String, FieldIdent>) {
        
        self.inherited_monikers = monikers;
        // for key in self.original.def_fields.keys() {
        //     self.inherited_monikers.insert(key.clone(), self.id.clone().get_field_ident(key.clone()));
        // }
        let child_monikers = self.monikers_for_child();
        
        match self.stype.as_mut() {
            PartiallyResolvedStructureType::Sequence(ref mut seq) => {
                for structure in seq.iter_mut() {
                    structure.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
                }
            },
            PartiallyResolvedStructureType::Addressed{ref mut pss, ..} => {
                pss.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
            },
            PartiallyResolvedStructureType::Chain{ref mut prs, ..} => {
                prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
            },
            _ => {
                // Do nothing
            }
        }
    }

    fn convert_expr_vars(&self, vars: HashSet<String>) -> Result<HashSet<AbstractIdent>, ParseAbstractIdentError> {
        let mut new = HashSet::new();
        for s in vars {
            match self.inherited_monikers.get(&s) {
                Some(fi) => {new.insert(fi.clone().to_abstract());},
                None => {
                    if self.import_monikers.contains_key(&s) || self.original.def_fields.contains_key(&s) {
                        new.insert(self.id.get_field_ident(s).to_abstract());
                    } else {
                        error!("Couldn't convert {} in {}", s, self.id);
                        // new.insert(AbstractIdent::from_str(&s)?);
                        return Err(ParseAbstractIdentError::new(format!("Could not interpret '{}' in context of '{:?}'", s, self.id).to_string()))
                    }
                }
            }
        }
        Ok(new)
    }

    fn get_fname(&self, prs_map: &PrsMap<'a>) -> Option<String> {
        match &self.original.filename {
            Some(fname) => Some(fname.to_string()),
            None => None
        } 
    }

    /// Returns a boolean that indicates whether or not this structure type is "inline", where inline means that
    /// it's contents is located in the location where the structure is defined. This is distinguished from 
    /// structure types such as "addressed", where the structure's content is not actually in the location where
    /// the structure appears in the file spec. This is used in determining the offset to whatever structure is
    /// defined after this one in a repeat or sequence.
    fn is_inline(&self) -> bool {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::Addressed{..} | PartiallyResolvedStructureType::Assembly{..} => {
                false
            },
            _ => true
        }
    }

    /// Attempts to find the actual field that field_name refers to in the context of `self`. This 
    /// requires recursively following inheritance/import sources. Returns Ok(Some(...)) if the 
    /// field name provided is associated with an actual field ID somewhere. Returns Ok(None) if the
    /// field name is valid but it refers to a derived field as opposed to a real field in the file.
    /// Returns Err(IdentNotValid) if the field doesn't exist or if the inheritance/import source 
    /// points to a structure that isn't in prs_map. Returns Err(IdentNotFound) if the source may
    /// exist but hasn't been fully determined yet.
    fn get_source_field(&self, field_name: String, prs_map: &PrsMap<'a>) -> Result<Option<FieldIdent>, FileSpecError> {
        info!("Entering get_source_field for {:?}", self.id);
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                if initial.id_map.contains_key(&field_name) {
                    return Ok(Some(self.id.get_field_ident(field_name)))
                } else {
                    error!("initial did not conain key");
                    return Err(FileSpecError::ident_not_valid(self.id.get_field_ident(field_name).to_abstract()))
                }
            },
            _ => {
                match self.inherited_monikers.get(&field_name) {
                    Some(fi) => {
                        if let Some(prs) = prs_map.get(&fi.structure) {
                            info!("Looking for source field in inherited monikers");
                            return prs.borrow().get_source_field(field_name, prs_map)
                        } else if fi.structure.is_root() {
                            // If the source's structure is root, then the inherited moniker must have been injected and has no
                            // "original" source. Return None
                            info!("Field {} wsa injected into the initial inheritance monikers", field_name);
                            return Ok(None)
                        } else {
                            todo!("inherited monikers contains PRS that wasn't in prs_map")
                        }
                    },
                    None => {
                        match self.import_monikers.get(&field_name) {
                            Some(Some(sid)) => {
                                if let Some(prs) = prs_map.get(&sid) {
                                    info!("Looking for source field in import monikers");
                                    return prs.borrow().get_source_field(field_name, prs_map)
                                } else {
                                    todo!("import monikers contains PRS that wasn't in prs_map")
                                }
                            },
                            Some(None) => {
                                error!("Couldn't find {} in {}", field_name, self.id);
                                return Err(FileSpecError::ident_not_found(self.id.get_field_ident(field_name).to_abstract()))
                            },
                            None => {
                                if self.original.def_fields.contains_key(&field_name) {
                                    return Ok(None)
                                } else {
                                    return Err(FileSpecError::ident_not_valid(self.id.get_field_ident(field_name).to_abstract()))
                                }
                                
                            }
                        }
                    }
                }
            }
        }

    }

    fn try_get_region_at(&self, vd: &ValueDictionary, location: BitIndex) -> Result<DependencyReport<FileRegion>, FileSpecError> {
        info!("Trying to get region for {:?}", self.id);
        let position_id = self.id.get_attr_ident(StructureAttrType::Position);
        let start_id = self.id.get_attr_ident(StructureAttrType::Start);
        let end_id = self.id.get_attr_ident(StructureAttrType::End);
        let mut parents = HashSet::from([position_id.clone().to_abstract(), start_id.clone().to_abstract(), end_id.clone().to_abstract()]);

        let position = match vd.lookup_struct_attr(&position_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(parents))
        };

        if position > location {
            // location is before position
            return Ok(DependencyReport::does_not_exist())
        }

        let start = match vd.lookup_struct_attr(&start_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(parents))
        };

        if start > location {
            // location is between position and start
            return Ok(DependencyReport::success(FileRegion::StructurePad(self.id.clone())))
        }

        let end = match vd.lookup_struct_attr(&end_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(parents))
        };

        if location >= end {
            // location is after end
            return Ok(DependencyReport::does_not_exist())
        }

        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                let mut prev_end = start;
                for field in &initial.fields {
                    let field_id = self.id.get_field_ident(field.id.clone());
                    let field_pos_id = field_id.get_attr_ident(FieldAttrType::Position);
                    let field_start_id = field_id.get_attr_ident(FieldAttrType::Start);
                    let field_end_id = field_id.get_attr_ident(FieldAttrType::End);
                    parents.insert(field_pos_id.clone().to_abstract());
                    parents.insert(field_start_id.clone().to_abstract());
                    parents.insert(field_end_id.clone().to_abstract());

                    let field_pos = match vd.lookup_field_attr(&field_pos_id) {
                        Some(ev) => ev.expect_position()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };
                    if field_pos > location {
                        return Ok(DependencyReport::success(FileRegion::Spare(self.id.clone(), prev_end..field_pos)));
                    }
                    let field_start = match vd.lookup_field_attr(&field_start_id) {
                        Some(ev) => ev.expect_position()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };
                    if location < field_start {
                        return Ok(DependencyReport::success(FileRegion::FieldPad(field_id.clone())))
                    }
                    let field_end = match vd.lookup_field_attr(&field_end_id) {
                        Some(ev) => ev.expect_position()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };
                    if location < field_end {
                        return Ok(DependencyReport::success(FileRegion::Field(field_id)))
                    }
                }
                Ok(DependencyReport::success(FileRegion::Spare(self.id.clone(), prev_end..end)))
            },
            PartiallyResolvedStructureType::Addressed{position, pss, ..} => {
                return pss.borrow().try_get_region_at(vd, location)
            },
            PartiallyResolvedStructureType::Sequence(seq) => {
                for prs in seq {
                    let dr = prs.borrow().try_get_region_at(vd, location)?;
                    match dr.result {
                        DepResult::DoesNotExist => {},
                        DepResult::MightExist{..} | DepResult::Incomplete{..} | DepResult::Success(_) => return Ok(dr)
                    }
                }
                Ok(DependencyReport::does_not_exist())
            },
            PartiallyResolvedStructureType::Switch{prs, ..} => {
                if let Some(prs) = prs {
                    let dr = prs.borrow().try_get_region_at(vd, location)?;
                    match dr.result {
                        DepResult::DoesNotExist => {
                            // Location is between the end of the switch contents and the end of the switch... It's a spare.
                            let prs_end_id = prs.borrow().id.clone().get_attr_ident(StructureAttrType::End);
                            parents.insert(prs_end_id.clone().to_abstract());
                            let prs_end = match vd.lookup_struct_attr(&prs_end_id) {
                                Some(ev) => ev.expect_position()?,
                                None => return Ok(DependencyReport::incomplete(parents))
                            };
                            return Ok(DependencyReport::success(FileRegion::Spare(self.id.clone(), prs_end..end)));
                        },
                        DepResult::MightExist{..} | DepResult::Incomplete{..} | DepResult::Success(_) => return Ok(dr)
                    }
                } else {
                    let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex);
                    parents.insert(switch_index_id.to_abstract());
                    return Ok(DependencyReport::incomplete(parents))
                }
            },
            PartiallyResolvedStructureType::Chain{prs, ..} => {
                // Only the first member in the chain is actually positioned... The other are all addressed structures.
                let dr = prs.borrow().try_get_region_at(vd, location)?;
                match dr.result {
                    DepResult::DoesNotExist => {
                        // Location is between the end of the switch contents and the end of the switch... It's a spare.
                        let prs_end_id = prs.borrow().id.clone().get_attr_ident(StructureAttrType::End);
                        parents.insert(prs_end_id.clone().to_abstract());
                        let prs_end = match vd.lookup_struct_attr(&prs_end_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                        return Ok(DependencyReport::success(FileRegion::Spare(self.id.clone(), prs_end..end)));
                    },
                    DepResult::MightExist{..} | DepResult::Incomplete{..} | DepResult::Success(_) => return Ok(dr)
                }
            },
            PartiallyResolvedStructureType::Repeat{seq, finalized, ..} 
                | PartiallyResolvedStructureType::RepeatUntil{seq, finalized, ..} => {
                for prs in seq {
                    let dr = prs.borrow().try_get_region_at(vd, location)?;
                    match dr.result {
                        DepResult::DoesNotExist => {},
                        DepResult::MightExist{..} | DepResult::Incomplete{..} | DepResult::Success(_) => return Ok(dr)
                    }
                }
                if *finalized {
                    // If the repeat strucutre is finalized and none of the repetitions contain the position, then
                    // there must be a spare block between the last repetition and the end of the repeat structure
                    if seq.is_empty() {
                        return Ok(DependencyReport::success(FileRegion::Spare(self.id.clone(), start..end)));
                    }
                    let last_index = seq.len() - 1;
                    let seq_end_id = seq[last_index].borrow().id.clone().get_attr_ident(StructureAttrType::End);
                    parents.insert(seq_end_id.clone().to_abstract());
                    let seq_end = match vd.lookup_struct_attr(&seq_end_id) {
                        Some(ev) => ev.expect_position()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };
                    return Ok(DependencyReport::success(FileRegion::Spare(self.id.clone(), seq_end..end)));
                } else {
                    let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions);
                    parents.insert(repetitions_id.to_abstract());
                    return Ok(DependencyReport::incomplete(parents))
                }
            },
            PartiallyResolvedStructureType::Segment => {
                Ok(DependencyReport::success(FileRegion::Segment(self.id.clone(), start..end)))
            },
            PartiallyResolvedStructureType::Assembly{..} => {
                todo!()
            }
        }   
    }

    fn try_get_field(&mut self, field_id: FieldIdent, vd: &ValueDictionary,
            prs_map: &PrsMap<'a>) -> Result<DependencyReport<DataSource>, FileSpecError> {
        info!("Import monikers for {:?}: {:?}", self.id, self.import_monikers);
        if self.import_monikers.contains_key(&field_id.id) {
            match &self.import_monikers[&field_id.id] {
                Some(source_id) => {
                    // Grab value from the import source
                    info!("Trying to import {} from {:?}", field_id, source_id);
                    let source = prs_map.get(source_id).unwrap();
                    let new_field_id = source_id.get_field_ident(field_id.id);
                    source.borrow_mut().try_get_field(new_field_id, vd, prs_map)
                },
                None => {
                    let dr = self.try_resolve_import(field_id.id.clone())?;
                    match dr.result {
                        DepResult::Success(source_id) => {
                            info!("Successfully resolved import");
                            self.import_monikers.insert(field_id.id.clone(), Some(source_id.clone()));
                            let source = prs_map.get(&source_id).unwrap();
                            let new_field_id = source_id.get_field_ident(field_id.id);
                            source.borrow_mut().try_get_field(new_field_id, vd, prs_map)
                        },
                        DepResult::Incomplete(vars) => {
                            info!("Import incomplete");
                            let mut new_dr = DependencyReport::incomplete(vars);
                            new_dr.parents_children.extend(dr.parents_children);
                            Ok(new_dr)
                        },
                        DepResult::MightExist(vars) => {
                            info!("Import might exist: {:?}", vars);
                            let mut new_dr = DependencyReport::might_exist(vars);
                            new_dr.parents_children.extend(dr.parents_children);
                            Ok(new_dr)
                        },
                        DepResult::DoesNotExist => {
                            // Import source doesn't exist, use default value if available
                            if let Some(default) = self.original.exports.get(&field_id.id).expect("PRS has export that isn't in original structure?") {
                                // panic!("DEFAULT!!!");
                                // let lookup = self.get_lookup_fn(vd);
                                match default.evaluate(&self.fname, &|alias| {self.lookup_str(&vd, alias)}) {
                                    Ok(Some(value)) => {
                                        let mut dr = DependencyReport::success(DataSource::Given(value));
                                        dr.add_pc_pairs(self.convert_expr_vars(default.vars())?, HashSet::from([field_id.to_abstract()]));
                                        Ok(dr)
                                    },
                                    Ok(None) => Ok(DependencyReport::incomplete(self.convert_expr_vars(default.vars())?)),
                                    Err(err) => Err(FileSpecError::from(err))
                                }
                            } else {
                                Ok(DependencyReport::does_not_exist())
                            }
                            
                        }
                    }

                }
            }

        } else if field_id.structure == self.id {
            if self.original.def_fields.contains_key(&field_id.id) {
                match &self.original.def_fields[&field_id.id] {
                    DefField::Search{find, ..} => {
                        let search_start_id = field_id.clone().get_attr_ident(FieldAttrType::SearchStart);
                        let search_n_id = field_id.clone().get_attr_ident(FieldAttrType::SearchN);
                        let parents = HashSet::from([search_start_id.clone().to_abstract(), search_n_id.clone().to_abstract()]);
                        let search_start = match vd.lookup_field_attr(&search_start_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                        let search_n = match vd.lookup_field_attr(&search_n_id) {
                            Some(ev) => ev.expect_integer()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                        let ds = DataSource::Search{start: search_start, n: search_n as usize, find: find.to_string()};
                        let mut dr = DependencyReport::success(ds);
                        dr.add_pc_pairs(parents, HashSet::from([field_id.to_abstract()]));
                        return Ok(dr)
                    },
                    DefField::Anchor{offset} => {
                        let position_id = field_id.structure.clone().get_attr_ident(StructureAttrType::Position);
                        let anchor_offset_id = field_id.clone().get_attr_ident(FieldAttrType::AnchorOffset);
                        let parents = HashSet::from([position_id.clone().to_abstract(), anchor_offset_id.clone().to_abstract()]);
                        let position = match vd.lookup_struct_attr(&position_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                        let anchor_offset = match vd.lookup_field_attr(&anchor_offset_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                        let ds = DataSource::Given(ExprValue::Position(position + anchor_offset));
                        let mut dr = DependencyReport::success(ds);
                        dr.add_pc_pairs(parents, HashSet::from([field_id.to_abstract()]));
                        return Ok(dr)
                    }
                    _ => todo!()
                }
            } else {
                let start_id = field_id.clone().get_attr_ident(FieldAttrType::Start);
                let length_id = field_id.clone().get_attr_ident(FieldAttrType::Length);
                let parents = HashSet::from([start_id.clone().to_abstract(), length_id.clone().to_abstract()]);
                let start = match vd.lookup_field_attr(&start_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
                let span = match vd.lookup_field_attr(&length_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                match self.stype.as_ref() {
                    PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                        match initial.get_field_info(field_id.id.clone()) {
                            Ok((name, dtype, endian)) => {
                                let field = UnparsedBinaryField {
                                    name, start, span, dtype, endian
                                };

                                let mut dr = DependencyReport::success(DataSource::Field(field));
                                dr.add_pc_pairs(parents, HashSet::from([field_id.to_abstract()]));
                                Ok(dr)
                            },
                            Err(()) => {
                                error!("Structure did not contain the field!!!");
                                return Err(FileSpecError::ident_not_valid(field_id.to_abstract()))
                            }
                        }
                        
        
                    },
                    _ => {
                        error!("Cannot get a field for a non-section structure!");
                        return Err(FileSpecError::ident_not_valid(field_id.to_abstract()))
                    }
                }
            }
        } else {
            error!("Field {} does not exist!", field_id);
            return Err(FileSpecError::ident_not_found(field_id.to_abstract()))

        }
    }

    fn lookup_str(&self, vd: &ValueDictionary, alias: &str) -> Option<ExprValue> {
        match self.inherited_monikers.get(alias) {
            Some(fi) => vd.lookup_field(fi),
            None => {
                if self.import_monikers.contains_key(alias)  || self.original.def_fields.contains_key(alias) {
                    let fi = self.id.clone().get_field_ident(alias.to_string());
                    vd.lookup_field(&fi)
                } else {
                    vd.lookup_str(alias).unwrap_or(None)
                }
                
            }
        }
    }

    fn try_lookup(&mut self, key: AbstractIdent, vd: &ValueDictionary, prs_map: &mut PrsMap<'a>) -> Result<DependencyReport<ExprValue>, FileSpecError> {
        info!("Looking for {} in {}", key, self.id);
        let mut result = match key {
            AbstractIdent::StructureAttr(ref sai) => {
                if sai.structure == self.id {
                    match sai.attr {
                        StructureAttrType::Position => self.try_get_position(vd),
                        StructureAttrType::Align => self.try_get_alignment(vd),
                        StructureAttrType::AlignBase => self.try_get_alignment_base(vd),
                        StructureAttrType::StartPad => self.try_get_start_pad(vd),
                        StructureAttrType::Start => self.try_get_start(vd),
                        StructureAttrType::Length => self.try_get_length(vd),
                        StructureAttrType::SpareLength => self.try_get_spare_length(vd),
                        StructureAttrType::ContentsLength => self.try_get_contents_length(vd),
                        StructureAttrType::End => self.try_get_end(vd),
                        StructureAttrType::SwitchValue => self.try_get_switch_value(vd),
                        StructureAttrType::SwitchIndex => self.try_get_switch_index(vd, prs_map),
                        StructureAttrType::Repetitions => self.try_get_repetitions(vd, prs_map),
                        StructureAttrType::SwitchCase(i) => self.try_get_switch_case(i, vd),
                        StructureAttrType::Break(i) => self.try_get_break(i, vd)
                    }
                } else {
                    info!("Searching in children");
                    self.try_lookup_in_children(key, vd, prs_map)
                }
            },
            AbstractIdent::FieldAttr(ref fai) => {
                if fai.field.structure == self.id {
                    if self.import_monikers.contains_key(&fai.field.id) {
                        match &self.import_monikers[&fai.field.id] {
                            Some(source_id) => {
                                // Grab value from the import source
                                info!("Trying to import {} from {:?}", fai.field.id, source_id);
                                let source = prs_map.get(source_id).unwrap();
                                let new_key = FieldIdent::new(source.borrow().id.clone(), fai.field.id.clone()).get_attr_ident(fai.attr.clone());
                                // let new_key = source.borrow().field_attribute_id(field_match.as_str().to_string(), attr_match.as_str().to_string());
                                match vd.lookup_field_attr(&new_key) {
                                    Some(ev) => {
                                        let mut dr = DependencyReport::success(ev);
                                        dr.add_pc_pairs(HashSet::from([new_key.to_abstract()]), HashSet::from([key]));
                                        Ok(dr)
                                    },
                                    None => Ok(DependencyReport::incomplete(HashSet::from([new_key.to_abstract()])))
                                }
                            },
                            None => {
                                let dr = self.try_resolve_import(fai.field.id.clone())?;
                                match dr.result {
                                    DepResult::Success(source_id) => {
                                        self.import_monikers.insert(fai.field.id.clone(), Some(source_id.clone()));
                                        // Grab value from the import source
                                        info!("Trying to import {} from {:?}", fai.field.id, source_id);
                                        let source = prs_map.get(&source_id).unwrap();
                                        let new_key = FieldIdent::new(source.borrow().id.clone(), fai.field.id.clone()).get_attr_ident(fai.attr.clone());
                                        // let new_key = source.borrow().field_attribute_id(field_match.as_str().to_string(), attr_match.as_str().to_string());
                                        match vd.lookup_field_attr(&new_key) {
                                            Some(ev) => {
                                                let mut dr = DependencyReport::success(ev);
                                                dr.add_pc_pairs(HashSet::from([new_key.to_abstract()]), HashSet::from([key]));
                                                Ok(dr)
                                            },
                                            None => Ok(DependencyReport::incomplete(HashSet::from([new_key.to_abstract()])))
                                        }
                                    },
                                    DepResult::Incomplete(vars) => {
                                        let mut new_dr = DependencyReport::incomplete(vars);
                                        new_dr.parents_children.extend(dr.parents_children);
                                        Ok(new_dr)
                                    },
                                    DepResult::MightExist(vars) => {
                                        let mut new_dr = DependencyReport::might_exist(vars);
                                        new_dr.parents_children.extend(dr.parents_children);
                                        Ok(new_dr)
                                    }
                                    _ => todo!()
                                }

                            }
                        }
                    } else {
                        self.try_get_field_attr(fai.clone(), vd)
                    }
                } else {
                    info!("\t Searching in children");
                    self.try_lookup_in_children(key, vd, prs_map)
                }
            },
            AbstractIdent::Field(_) => panic!("Field lookups not supported")
        }?;

        for (parents, children) in result.parents_children.iter_mut() {
            // TODO: Make sure this logic works with def_fields
            *parents = parents.iter().map(
                |var| match var {
                    AbstractIdent::Field(fi) => {
                        if fi.structure == self.id {
                            match self.inherited_monikers.get(&fi.id) {
                                Some(new) => AbstractIdent::Field(new.clone()),
                                None => var.clone()
                            }
                        } else {
                            var.clone()
                        }
                    },
                    _ => var.clone()
                }
            ).collect();
            
            // TODO: Make sure this logic works with def_fields
            *children = children.iter().map(
                |var| match var {
                    AbstractIdent::Field(fi) => {
                        if fi.structure == self.id {
                            match self.inherited_monikers.get(&fi.id) {
                                Some(new) => AbstractIdent::Field(new.clone()),
                                None => var.clone()
                            }
                        } else {
                            var.clone()
                        }
                    },
                    _ => var.clone()
                }
            ).collect();
        }

        match result.result {
            DepResult::Success(_) => {},
            DepResult::DoesNotExist => {},
            DepResult::Incomplete(ref mut vars) | DepResult::MightExist(ref mut vars) => {
                let mut new_vars = HashSet::new();
                for var in vars.clone().into_iter() {
                    if let AbstractIdent::Field(ref fi) = var {
                        if fi.structure == self.id {
                            // TODO: Make sure this logic works with def_fields
                            match self.inherited_monikers.get(&fi.id) {
                                Some(new) => {new_vars.insert(AbstractIdent::Field(new.clone()));},
                                None => {new_vars.insert(var);}
                            }
                        } else {
                            new_vars.insert(var);
                        }
                    } else {
                        new_vars.insert(var);
                    }
                }

                *vars = new_vars
                
            }
        }

        Ok(result)

    }

    /// Attempts to evaluate the given expression wit any output type and return the result as a dependency 
    /// report. target_ident will be used as the "child" in the report and any variables in the expression 
    /// will be used as the "parents". Returns incomplete if there are any variables in the expression 
    /// that aren't yet in the provided value dictionary. Returns an error if there are any errors 
    /// evaluating the expression or resolving the variables in the expression
    fn try_evaluate_any_expression(&self, expr: &ExprWrapper, target_ident: AbstractIdent, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        // Convert the expression's variables to AbstractIdents and keep track of them
        // to return as dependencies upon success or failure
        let parents = self.convert_expr_vars(expr.vars())?;

        // Attempt to evaluate the expression using self's lookup_str routine
        match expr.evaluate(&self.fname, &|alias| {self.lookup_str(&vd, alias)}) {

            Ok(Some(value)) => {
                // If the evaluation is successful, construct a successful dependency report 
                // with the result, add the dependecies found previously, and return it.
                let mut dr = DependencyReport::success(value);
                dr.add_pc_pairs(parents, HashSet::from([target_ident]));
                Ok(dr)
            },
            Ok(None) => {
                // If the evaluation failed due to a lookup error, then one of the dependencies
                // must not be resolved yet. Return incomplete with the dependency list found
                // previously
                Ok(DependencyReport::incomplete(parents))
            }
            Err(err) => {
                // Any other errors are actual problems with the expression. Return the error.
                Err(FileSpecError::from(err))
            }
        }
    }

    /// Attempts to evaluate the given expression as a position and return the result as a dependency 
    /// report. target_ident will be used as the "child" in the report and any variables in the expression 
    /// will be used as the "parents". Returns incomplete if there are any variables in the expression 
    /// that aren't yet in the provided value dictionary. Returns an error if there are any errors 
    /// evaluating the expression or resolving the variables in the expression or if the expression 
    /// evaluates to anything other than a position.
    fn try_evaluate_position_expression(&self, expr: &ExprWrapper, target_ident: AbstractIdent, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        // Convert the expression's variables to AbstractIdents and keep track of them
        // to return as dependencies upon success or failure
        let parents = self.convert_expr_vars(expr.vars())?;

        // Attempt to evaluate the expression using self's lookup_str routine
        match expr.evaluate_expect_position(&self.fname, &|alias| {self.lookup_str(&vd, alias)}) {

            Ok(Some(pos)) => {
                // If the evaluation is successful, construct a successful dependency report 
                // with the result, add the dependecies found previously, and return it.
                let mut dr = DependencyReport::success(ExprValue::Position(pos));
                dr.add_pc_pairs(parents, HashSet::from([target_ident]));
                Ok(dr)
            },
            Ok(None) => {
                // If the evaluation failed due to a lookup error, then one of the dependencies
                // must not be resolved yet. Return incomplete with the dependency list found
                // previously
                Ok(DependencyReport::incomplete(parents))
            }
            Err(err) => {
                // Any other errors are actual problems with the expression. Return the error.
                Err(FileSpecError::from(err))
            }
        }
    }

    /// Attempts to evaluate the given expression as a integer and return the result as a dependency 
    /// report. target_ident will be used as the "child" in the report and any variables in the expression 
    /// will be used as the "parents". Returns incomplete if there are any variables in the expression 
    /// that aren't yet in the provided value dictionary. Returns an error if there are any errors 
    /// evaluating the expression or resolving the variables in the expression or if the expression 
    /// evaluates to anything other than a integer.
    fn try_evaluate_integer_expression(&self, expr: &ExprWrapper, target_ident: AbstractIdent, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        // Convert the expression's variables to AbstractIdents and keep track of them
        // to return as dependencies upon success or failure
        let parents = self.convert_expr_vars(expr.vars())?;

        // Attempt to evaluate the expression using self's lookup_str routine
        match expr.evaluate_expect_integer(&self.fname, &|alias| {self.lookup_str(&vd, alias)}) {

            Ok(Some(i)) => {
                // If the evaluation is successful, construct a successful dependency report 
                // with the result, add the dependecies found previously, and return it.
                let mut dr = DependencyReport::success(ExprValue::Integer(i));
                dr.add_pc_pairs(parents, HashSet::from([target_ident]));
                Ok(dr)
            },
            Ok(None) => {
                // If the evaluation failed due to a lookup error, then one of the dependencies
                // must not be resolved yet. Return incomplete with the dependency list found
                // previously
                Ok(DependencyReport::incomplete(parents))
            }
            Err(err) => {
                // Any other errors are actual problems with the expression. Return the error.
                Err(FileSpecError::from(err))
            }
        }
    }

    // Attempts to determine the value of a field attribute without looking it up. This is used 
    // for the initial determination of the value. The value dictionary is needed to look up 
    // dependencies as needed. Returns the result as a DependencyReport with all dependencies
    // needed to determine the value, regardless of whether the value was successfully determined.
    //
    // Returns an InvalidIdentError if the field attribute is related to a real field (not a derived
    // field) and self is not an UnresovledSection or self does not contain the field ID. Also returns
    // InvalidIdentError if self is related to a derived field and the derived field is either not in 
    // self or the type of the derived field isn't associated with  the attribute. May return other 
    // errors if a problem is encountered in any expression evaluations.
    fn try_get_field_attr(&self, target_ident: FieldAttrIdent, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        match target_ident.attr {

            // These first three are defined or derived fields, they aren't associated with actual 
            // field specs, they're associated with a DefField structure
            FieldAttrType::SearchStart | FieldAttrType::SearchN | FieldAttrType::AnchorOffset => {

                // Try to access the field from the def_fields list
                if let Some(def_field) = self.original.def_fields.get(&target_ident.field.id) {
                    match target_ident.attr {
                        FieldAttrType::SearchStart => {
                            // If the attribute is search start, then it should be related to a Search field. Try
                            // to evaluate the start expression of a Search field
                            if let DefField::Search{start, ..} = def_field {
                                return self.try_evaluate_position_expression(&start, target_ident.to_abstract(), vd)
                            } 
                        },
                        FieldAttrType::SearchN => {
                            // If the attribute is search n, then it should be related to a Search field. Try
                            // to evaluate the n expression of a Search field
                            if let DefField::Search{n, ..} = def_field {
                                return self.try_evaluate_integer_expression(&n, target_ident.to_abstract(), vd)
                            } 
                        },
                        FieldAttrType::AnchorOffset => {
                            // If the attribute is anchor offset, then it should be related to a Anchor field. Try
                            // to evaluate the offset expression of a Anchor field
                            if let DefField::Anchor{offset} = def_field {
                                return self.try_evaluate_position_expression(&offset, target_ident.to_abstract(), vd)
                            } 
                        },
                        _ => unreachable!()
                    }
                } 

                // If the field isn't in the def_field's list, or the def_field it's associated 
                // with isn't related to the attribute, then it must be invalid.
                Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
            },

            // Fields that require accessing data from the FieldSpec in initial
            FieldAttrType::Valid | FieldAttrType::Length | FieldAttrType::Offset | FieldAttrType::Align | FieldAttrType::AlignBase | FieldAttrType::Position => {

                #[cfg(test)] {
                    // Panic during tests if the field does not belong to self.
                    if self.id != target_ident.field.structure {
                        panic!("try_get_field_attr called with field that does not belong to self")
                    }
                }

                if let PartiallyResolvedStructureType::UnresolvedSection{initial} = self.stype.as_ref() {
        
                    #[cfg(test)] {
                        // Panic during tests if the field does not exist in the initial SectionSpec
                        if !initial.id_map.contains_key(&target_ident.field.id) {
                            panic!("try_get_field_attr called with field that belongs to self but is not in self's field list")
                        }
                    }

                    // Get the index of the field by looking up its name in the initial SectionSpec's
                    // id map. If it's not in the id map, then the field either does not exist or does
                    // not belong to `self`. In that case, return an error
                    let field_index = match initial.id_map.get(&target_ident.field.id) {
                        Some(i) => i,
                        None => return Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
                    };

                    // Get the field by its index
                    let field = &initial.fields[*field_index];

                    match target_ident.attr {
                        FieldAttrType::Valid => {

                            // Check to see if the field has a validity check
                            if let Some(field_valid) = &field.valid {

                                // If there is a validity check, then it's dependencies will be whatever
                                // variables are in the validity check expression and also the field's value
                                let mut parents = self.convert_expr_vars(field_valid.vars())?;
                                parents.insert(target_ident.field.clone().to_abstract());
            
                                // Attempt to lookup the field's value to supply to the validity check. If it's
                                // not available, return an incomplete dependency report.
                                let field_value = match vd.lookup_field(&target_ident.field) {
                                    Some(ev) => ev,
                                    None => return Ok(DependencyReport::incomplete(parents))
                                };
                
                                // Evaluate the field's validity check with the field's value as the only argument
                                match field_valid.evaluate_with_args(&self.fname, &vec![field_value], &|alias| {self.lookup_str(&vd, alias)}) {
                                    Ok(Some(valid)) => {
                                        // If the evaluation was successful, ensure it's a Boolean and return it in
                                        // a successful dependency report with the dependencies found earlier
                                        let mut dr = DependencyReport::success(ExprValue::Bool(valid.expect_bool()?));
                                        dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
                                        Ok(dr)
                                    },
                                    Ok(None) => {
                                        // If there was a lookup error, it means that one of the dependencies has not
                                        // yet been resolved. Return an incomplete dependency report
                                        Ok(DependencyReport::incomplete(parents))
                                    }
                                    Err(err) => {
                                        // Any other error is a problem
                                        Err(FileSpecError::from(err))
                                    }
                                }
                            } else {
                                // Field has no validity check, always return true with no dependencies
                                let mut dr = DependencyReport::success(ExprValue::Bool(true));
                                dr.add_pc_pairs(HashSet::new(), HashSet::from([target_ident.to_abstract()]));
                                Ok(dr)
                                // Ok(DependencyReport::success(ExprValue::Bool(true)))
                            }
                        }, 
                        FieldAttrType::Length => {
                            // Attempt to evaluate the field's length expression
                            self.try_evaluate_position_expression(&field.length, target_ident.to_abstract(), vd)
                        },
                        FieldAttrType::Offset => {
                            // Attempt to evaluate the field's offset expression
                            self.try_evaluate_position_expression(&field.offset, target_ident.to_abstract(), vd)
                        },
                        FieldAttrType::Align => {
                            // Attempt to evaluate the field's alignment expression
                            self.try_evaluate_position_expression(&field.alignment, target_ident.to_abstract(), vd)
                        },
                        FieldAttrType::AlignBase => {
                            // Attempt to evaluate the field's alignment-base expression
                            self.try_evaluate_position_expression(&field.alignment_base, target_ident.to_abstract(), vd)
                        },
                        FieldAttrType::Position => {
                            // The position of the field is going to be the field's offset plus either the structure's
                            // start location or the previous field's end location depending on whether this is the
                            // first field in the structure.

                            // Get the ID of the field's offset and add it as a dependency. Don't try to look it up
                            // yet because not all of the dependencies have been documented.
                            let offset_id = target_ident.field.clone().get_attr_ident(FieldAttrType::Offset);
                            let mut parents = HashSet::from([offset_id.clone().to_abstract()]);
            
                            // Get the initial location from either the structure's start or the previous field's
                            // end position
                            let pre_offset = if *field_index == 0 {
                                // First field in the structure, must be positioned starting from the structure start.
                                // Get the structure's start ID, add it to dependencies, and attempt to look it up. If the
                                // lookup fails, return an incomplete report
                                let structure_start_id = self.id.get_attr_ident(StructureAttrType::Start);
                                parents.insert(structure_start_id.clone().to_abstract());
                                
                                match vd.lookup_struct_attr(&structure_start_id) {
                                    Some(ev) => ev.expect_position()?,
                                    None => return Ok(DependencyReport::incomplete(parents))
                                }

                            } else {
                                // Field is positioned starting from previous field's end. Get the previous field's end ID, 
                                // add it to dependencies, and attempt to look it up. If the lookup fails, return an 
                                // incomplete report
                                let prev_field = self.id.clone().get_field_ident(initial.fields[*field_index - 1].id.clone());
                                let previous_end_id = prev_field.clone().get_attr_ident(FieldAttrType::End);
                                parents.insert(previous_end_id.clone().to_abstract());

                                match vd.lookup_field_attr(&previous_end_id) {
                                    Some(ev) => ev.expect_position()?,
                                    None => return Ok(DependencyReport::incomplete(parents))
                                }
                            };
            
                            // Now that the parents list is complete, try to look up the offset. Return an incomplete report if
                            // the lookup fails
                            let offset = match vd.lookup_field_attr(&offset_id) {
                                Some(ev) => ev.expect_position()?,
                                None => return Ok(DependencyReport::incomplete(parents))
                            };
            
                            // Calculate the field's position by adding the offset to the pre-offset position. Add the 
                            // dependencies and return it as a successful dependency report
                            let mut dr = DependencyReport::success(ExprValue::Position(pre_offset + offset));
                            dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
                            Ok(dr)
                        },
                        _ => unreachable!()
                    }

                } else {
                    // If self isn't a UnresolvedSection, then it can't have fields. The field id 
                    // must be invalid.
                    Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
                }
            },

            // Attributes that are associated with actual fields that must be contained in self, but
            // that don't need any data about the FieldSpec itself.
            FieldAttrType::Start | FieldAttrType::StartPad | FieldAttrType::End => {

                #[cfg(test)] {
                    // Panic during tests if the field does not belong to self.
                    if self.id != target_ident.field.structure {
                        panic!("try_get_field_attr called with field that does not belong to self")
                    }

                }

                // Pre-check to ensure that self is an UnresolvedSection. self doesn't actually need to
                // be an UnresolvedSection to do this processing, but this is here because finding the
                // value of attributes that shouldn't exist can compromise other algorithms in use.
                if let PartiallyResolvedStructureType::UnresolvedSection{initial} = self.stype.as_ref() {
                    #[cfg(test)] {
                        // Panic during tests if the field does not exist in the initial SectionSpec
                        if !initial.id_map.contains_key(&target_ident.field.id) {
                            panic!("try_get_field_attr called with field that belongs to self but is not in self's field list")
                        }
                    }
                } else {
                    // If self isn't a UnresolvedSection, then it can't have fields. The field id 
                    // must be invalid.
                    return Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
                }

                // Declare a hash set for the result's dependencies. This will be initialized in 
                // the following logic depending on the attribute.
                let parents: HashSet<AbstractIdent>;

                // Calculation of the value depends on the attribute
                let value = match target_ident.attr {
                    FieldAttrType::Start => {
                        // Get the start pad and position ids to add to the parents list and also 
                        // for the lookup
                        let start_pad_id = target_ident.field.clone().get_attr_ident(FieldAttrType::StartPad);
                        let position_id = target_ident.field.clone().get_attr_ident(FieldAttrType::Position);
        
                        parents = HashSet::from([start_pad_id.clone().to_abstract(), position_id.clone().to_abstract()]);
        
                        // Attempt to lookup the field's start pad. If it fails, return an incomplete
                        // dependency report
                        let start_pad = match vd.lookup_field_attr(&start_pad_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                
                        // Attempt to lookup the field's position. If it fails, return an incomplete
                        // dependency report
                        let position = match vd.lookup_field_attr(&position_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };

                        // Calculate the start by adding the start pad to the position
                        ExprValue::Position(position + start_pad)

                    },
                    FieldAttrType::StartPad => {
                        // Get the alignment, alignment-base, and position ids to add to the parents list 
                        // and also for the lookup
                        let alignment_id = target_ident.field.clone().get_attr_ident(FieldAttrType::Align);
                        let alignment_base_id = target_ident.field.clone().get_attr_ident(FieldAttrType::AlignBase);
                        let position_id = target_ident.field.clone().get_attr_ident(FieldAttrType::Position);

                        parents = HashSet::from([
                            alignment_id.clone().to_abstract(), 
                            alignment_base_id.clone().to_abstract(),
                            position_id.clone().to_abstract()
                        ]);

                        // Attempt to lookup the field's alignment. If it fails, return an incomplete
                        // dependency report
                        let alignment = match vd.lookup_field_attr(&alignment_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                
                        // Attempt to lookup the field's alignment-base. If it fails, return an incomplete
                        // dependency report
                        let alignment_base = match vd.lookup_field_attr(&alignment_base_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                
                        // Attempt to lookup the field's position. If it fails, return an incomplete
                        // dependency report
                        let position = match vd.lookup_field_attr(&position_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
                
                        // The `remainder` here is the offset from the position to the largest aligned location
                        // less than the position. But we want the offset to the smallest aligned location 
                        // that is greater than the position
                        let remainder = (&position - &alignment_base).rem_euclid(&alignment);
                
                        // If the remainder is zero, then the position is already aligned and the start pad is 
                        // zero. Otherwise, it's the compliment of the remainder.
                        let start_pad = if remainder.is_zero() {
                            BitIndex::zero()
                        } else {
                            alignment - remainder
                        };

                        // Convert the start pad position to an ExprValue
                        ExprValue::Position(start_pad)
                    },
                    FieldAttrType::End => {
                        // Get the start position and length ids to add to the parents list and also 
                        // for the lookup
                        let start_id = target_ident.field.clone().get_attr_ident(FieldAttrType::Start);
                        let length_id = target_ident.field.clone().get_attr_ident(FieldAttrType::Length);
                        parents = HashSet::from([start_id.clone().to_abstract(), length_id.clone().to_abstract()]);

                        // Attempt to lookup the field's start position. If it fails, return an incomplete
                        // dependency report
                        let start = match vd.lookup_field_attr(&start_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };
            
                        // Attempt to lookup the field's length. If it fails, return an incomplete
                        // dependency report
                        let length = match vd.lookup_field_attr(&length_id) {
                            Some(ev) => ev.expect_position()?,
                            None => return Ok(DependencyReport::incomplete(parents))
                        };

                        // Calculate the end position by adding the length to the start position
                        ExprValue::Position(start + length)
                    }
                    _ => unreachable!()
                };

                // Create a successful dependency report with the result and dependencies determined above
                let mut dr = DependencyReport::success(value);
                dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
                Ok(dr)
        
            }
        }
    }


    fn try_lookup_in_children(&mut self, key: AbstractIdent, vd: &ValueDictionary, prs_map: &mut PrsMap<'a>) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        let mut current_position_key = self.id.get_attr_ident(StructureAttrType::Position);

        match self.stype.as_mut() {
            PartiallyResolvedStructureType::UnresolvedSection{..} | PartiallyResolvedStructureType::Segment => {
                
                Ok(DependencyReport::does_not_exist())

            },
            PartiallyResolvedStructureType::Addressed{position, ref mut pss, ..} => {

                let position = position.clone(); // Need this for the borrow checker

                let dr = pss.borrow_mut().try_lookup(key.clone(), vd, prs_map)?;

                // This block is to account for children not knowing their own position. This
                // catches that case and populates the dependency report itself.
                if matches!(dr.result, DepResult::DoesNotExist) && key == pss.borrow().id.get_attr_ident(StructureAttrType::Position).to_abstract() {
                    match vd.lookup_struct_attr(&current_position_key) {
                        Some(pos) => {
                            let mut dr = DependencyReport::success(pos);
                            dr.add_pc_pairs(HashSet::from([current_position_key.to_abstract()]), HashSet::from([key]));
                            Ok(dr)
                        },
                        None => Ok(DependencyReport::incomplete(HashSet::from([current_position_key.to_abstract()])))
                    }
                } else {
                    Ok(dr)
                }
                
            },
            PartiallyResolvedStructureType::Sequence(ref mut structures) => {
                let mut current_best = DependencyReport::does_not_exist();
                for structure in structures.iter_mut() {
                    let dr_result = structure.borrow_mut().try_lookup(key.clone(), vd, prs_map);
                    match dr_result {
                        Ok(dr) => match dr.result {
                            DepResult::Success(ref ev) => return Ok(dr),
                            DepResult::Incomplete(_) => return Ok(dr),
                            DepResult::MightExist(ref vars) => {
                                current_best = dr;
                            },
                            DepResult::DoesNotExist => {
                                // This block is to account for children not knowing their own position. This
                                // catches that case and populates the dependency report itself.
                                if key == structure.borrow().id.get_attr_ident(StructureAttrType::Position).to_abstract() {
                                    match vd.lookup_struct_attr(&current_position_key) {
                                        Some(pos) => {
                                            let mut dr = DependencyReport::success(pos);
                                            dr.add_pc_pairs(HashSet::from([current_position_key.to_abstract()]), HashSet::from([key]));
                                            return Ok(dr)
                                        },
                                        None => return Ok(DependencyReport::incomplete(HashSet::from([current_position_key.to_abstract()])))
                                    }
                                } else {
                                    // Do nothing
                                }
                            }
                        },
                        // Err(FileSpecError::ident_not_found(_)) => {todo!("Not sure if this is right...")},
                        Err(err) => return Err(err)
                    }
                    if structure.borrow().is_inline() {
                        current_position_key = structure.borrow().id.get_attr_ident(StructureAttrType::End);
                    }
                }

                Ok(current_best)
            },
            PartiallyResolvedStructureType::Switch{value, ref mut prs, ..} => {
                if let Some(ref mut prs) = prs {
                    // If the swich case is known, then this should be populated. It is all we need
                    let dr = prs.borrow_mut().try_lookup(key.clone(), vd, prs_map)?;

                    // This block is to account for children not knowing their own position. This
                    // catches that case and populates the dependency report itself.
                    if matches!(dr.result, DepResult::DoesNotExist) && key == prs.borrow().id.get_attr_ident(StructureAttrType::Position).to_abstract() {
                        match vd.lookup_struct_attr(&current_position_key) {
                            Some(pos) => {
                                let mut dr = DependencyReport::success(pos);
                                dr.add_pc_pairs(HashSet::from([current_position_key.to_abstract()]), HashSet::from([key]));
                                Ok(dr)
                            },
                            None => Ok(DependencyReport::incomplete(HashSet::from([current_position_key.to_abstract()])))
                        }
                    } else {
                        Ok(dr)
                    }

                } else {
                    // We don't know the switch case so can't perform the lookup
                    let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex).to_abstract();
                    Ok(DependencyReport::might_exist(HashSet::from([switch_index_id])))

                }
            },
            PartiallyResolvedStructureType::Repeat{ref mut seq, ..} | PartiallyResolvedStructureType::RepeatUntil{ref mut seq, ..} => {
                let mut current_best = DependencyReport::does_not_exist();
                for structure in seq.iter_mut() {
                    let dr_result = structure.borrow_mut().try_lookup(key.clone(), vd, prs_map);
                    match dr_result {
                        Ok(dr) => match dr.result {
                            DepResult::Success(ref ev) => return Ok(dr),
                            DepResult::Incomplete(_) => return Ok(dr),
                            DepResult::MightExist(ref vars) => {
                                current_best = dr;
                            },
                            DepResult::DoesNotExist => {
                                // This block is to account for children not knowing their own position. This
                                // catches that case and populates the dependency report itself.
                                if key == structure.borrow().id.get_attr_ident(StructureAttrType::Position).to_abstract() {
                                    match vd.lookup_struct_attr(&current_position_key) {
                                        Some(pos) => {
                                            let mut dr = DependencyReport::success(pos);
                                            dr.add_pc_pairs(HashSet::from([current_position_key.to_abstract()]), HashSet::from([key]));
                                            return Ok(dr)
                                        },
                                        None => return Ok(DependencyReport::incomplete(HashSet::from([current_position_key.to_abstract()])))
                                    }
                                } else {
                                    // Do nothing
                                }
                            }
                        },
                        // Err(FileSpecError::ident_not_found(_)) => {todo!("Not sure if this is right...")},
                        Err(err) => return Err(err)
                    }
                    if structure.borrow().is_inline() {
                        current_position_key = structure.borrow().id.get_attr_ident(StructureAttrType::End);
                    }
                } 

                Ok(current_best)

            },
            PartiallyResolvedStructureType::Chain{ref mut prs, ref mut seq, ..} => {
                let mut current_best = DependencyReport::does_not_exist();

                // Check prs
                let structure = &prs;
                let dr_result = structure.borrow_mut().try_lookup(key.clone(), vd, prs_map);
                match dr_result {
                    Ok(dr) => match dr.result {
                        DepResult::Success(ref ev) => return Ok(dr),
                        DepResult::Incomplete(_) => return Ok(dr),
                        DepResult::MightExist(ref vars) => {
                            current_best = dr;
                        },
                        DepResult::DoesNotExist => {
                            // This block is to account for children not knowing their own position. This
                            // catches that case and populates the dependency report itself.
                            if key == structure.borrow().id.get_attr_ident(StructureAttrType::Position).to_abstract() {
                                match vd.lookup_struct_attr(&current_position_key) {
                                    Some(pos) => {
                                        let mut dr = DependencyReport::success(pos);
                                        dr.add_pc_pairs(HashSet::from([current_position_key.to_abstract()]), HashSet::from([key]));
                                        return Ok(dr)
                                    },
                                    None => return Ok(DependencyReport::incomplete(HashSet::from([current_position_key.to_abstract()])))
                                }
                            } else {
                                // Do nothing
                            }
                        }
                    },
                    // Err(FileSpecError::ident_not_found(_)) => {todo!("Not sure if this is right...")},
                    Err(err) => return Err(err)
                }
                current_position_key = structure.borrow().id.get_attr_ident(StructureAttrType::End);

                // check everything in sequence
                for structure in seq.iter_mut() {
                    let dr_result = structure.borrow_mut().try_lookup(key.clone(), vd, prs_map);
                    match dr_result {
                        Ok(dr) => match dr.result {
                            DepResult::Success(ref ev) => return Ok(dr),
                            DepResult::Incomplete(_) => return Ok(dr),
                            DepResult::MightExist(ref vars) => {
                                current_best = dr;
                            },
                            DepResult::DoesNotExist => {
                                // This block is to account for children not knowing their own position. This
                                // catches that case and populates the dependency report itself.
                                if key == structure.borrow().id.get_attr_ident(StructureAttrType::Position).to_abstract() {
                                    match vd.lookup_struct_attr(&current_position_key) {
                                        Some(pos) => {
                                            let mut dr = DependencyReport::success(pos);
                                            dr.add_pc_pairs(HashSet::from([current_position_key.to_abstract()]), HashSet::from([key]));
                                            return Ok(dr)
                                        },
                                        None => return Ok(DependencyReport::incomplete(HashSet::from([current_position_key.to_abstract()])))
                                    }
                                } else {
                                    // Do nothing
                                }
                            }
                        },
                        // Err(FileSpecError::ident_not_found(_)) => {todo!("Not sure if this is right...")},
                        Err(err) => return Err(err)
                    }
                    current_position_key = structure.borrow().id.get_attr_ident(StructureAttrType::End);
                } 

                Ok(current_best)

            },
            PartiallyResolvedStructureType::Assembly{ref mut prs, ..} => {
                // prs.borrow_mut().try_lookup(key.clone(), vd, prs_map)

                let dr = prs.borrow_mut().try_lookup(key.clone(), vd, prs_map)?;

                // This block is to account for children not knowing their own position. This
                // catches that case and populates the dependency report itself.
                if matches!(dr.result, DepResult::DoesNotExist) && key == prs.borrow().id.get_attr_ident(StructureAttrType::Position).to_abstract() {
                    match vd.lookup_struct_attr(&current_position_key) {
                        Some(pos) => {
                            let mut dr = DependencyReport::success(pos);
                            dr.add_pc_pairs(HashSet::from([current_position_key.to_abstract()]), HashSet::from([key]));
                            Ok(dr)
                        },
                        None => Ok(DependencyReport::incomplete(HashSet::from([current_position_key.to_abstract()])))
                    }
                } else {
                    Ok(dr)
                }
            }
        }
    }

    /// Attempts to determine the value used in a switch statement and returns the result as a 
    /// dependency report. The value is determined by evaluating the value expression, and any
    /// variables referenced in the value expression are listed in the dependency report as 
    /// dependencies. 
    /// 
    /// If self is not a Switch, then Err(IdentNotValid) is returned.
    fn try_get_switch_value(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        let target_ident = self.id.get_attr_ident(StructureAttrType::SwitchValue);
        if let PartiallyResolvedStructureType::Switch{value, ..} = self.stype.as_ref() {
            self.try_evaluate_any_expression(&value, target_ident.to_abstract(), vd)

        } else {
            Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
        }
    }

    /// Attempts to determine the boolean value of a given case of a switch statement given its index
    /// (i.e. the first case for which this evaluates as "true" will be the Switch Index) and returns
    /// the result as a dependency report. The value is determined by evaluating the case expression
    /// with the value from the switch object as a singular argument. The switch value is listed as a
    /// depenceny along with any variables referenced by the case expression.
    ///
    /// If self is not a Swtich or the case index exceeds the number of cases in self, then 
    /// Err(IdentNotValid) is returned.
    fn try_get_switch_case(&self, i: usize, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {
        
        let target_ident = self.id.get_attr_ident(StructureAttrType::SwitchCase(i));

        if let PartiallyResolvedStructureType::Switch{cases, ..} = self.stype.as_ref() {

            // Check in the index is within the accepted bounds before attempting to access the case
            if i < cases.len() {

                // Get the case expression and add its variables to the parents list
                let expr = &cases[i].0;
                let mut parents = self.convert_expr_vars(expr.vars())?;

                // Get the id for the switch value and add it to the parents list
                let switch_value_id = self.id.get_attr_ident(StructureAttrType::SwitchValue);
                parents.insert(switch_value_id.clone().to_abstract());

                // Attempt to look up the switch value. If the lookup fails, return an incomplete result
                let arg = match vd.lookup_struct_attr(&switch_value_id) {
                    Some(ev) => ev,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                // Attempt to evaluate the expression using self's lookup_str routine
                match expr.evaluate_with_args(&self.fname, &vec![arg], &|alias| {self.lookup_str(&vd, alias)}) {
                    Ok(Some(v)) => {
                        // If the evaluation is successful, construct a successful dependency report 
                        // with the result, add the dependecies found previously, and return it.
                        let mut dr = DependencyReport::success(ExprValue::Bool(v.expect_bool()?));
                        dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
                        Ok(dr)
                    },
                    Ok(None) => {
                        // If the evaluation failed due to a lookup error, then one of the dependencies
                        // must not be resolved yet. Return incomplete with the dependency list found
                        // previously
                        Ok(DependencyReport::incomplete(parents))
                    },
                    Err(err) => {
                        // Any other errors are actual problems with the expression. Return the error.
                        Err(FileSpecError::from(err))
                    }
                }
            } else {
                // If the case index exceeds the number of cases in the switch, then it is not a valid ident
                error!("Switch case out of bounds: {}", i);
                Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
            }

        } else {
            // If self is not a Switch, then it is not a valid ident
            Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
        }
    }

    /// Attempts to determine the index of the correct switch case and returns the result as a 
    /// `DependencyReport`. If the index can be determined, then calling this function will create
    /// a new `PartiallyResolvedStructure` to represent it. The resulting structure will be added
    /// to `self` as a member and to `prs_map`. Note that if this returns an incomplete dependency
    /// report, then the dependencies list included may not be complete. The reason for that is that
    /// the extent of the dependencies list will depend on the case index itself. This function will
    /// also only return any new dependencies (that haven't been previously reported) each time it's 
    /// called.
    ///
    /// Returns Err(IdentNotValid) if `self` is not a Switch type.
    fn try_get_switch_index(&mut self, vd: &ValueDictionary, prs_map: &mut PrsMap<'a>) -> Result<DependencyReport<ExprValue>, FileSpecError> {
        
        #[cfg(test)] {
            // Panic during tests if self's PRS already exists (since this method should
            // be finding it for the first time) or if self is not a Switch.
            if let PartiallyResolvedStructureType::Switch{prs, ..} = self.stype.as_ref() {
                if prs.is_some() {
                    panic!("try_get_switch_index called for switch with existing PRS")
                }
            } else {
                panic!("try_get_switch_index called for non-switch structure '{}'", self.id)
            }
        }

        let target_ident = self.id.get_attr_ident(StructureAttrType::SwitchIndex);

        // Set up variables for the dependencies, case index, and corresponding Structure outside of 
        // the following if statement since they need to be used in a separate if statement to satisfy
        // the borrow rules (otherwise, a mutable and immutable borrow would be required)
        let mut parents = HashSet::new();
        let mut case_index: usize;
        let mut case_struct: Rc<Structure>;


        if let PartiallyResolvedStructureType::Switch{cases, default, ref mut next_case, ..} = self.stype.as_mut() {
            // If self is a Switch, iterate through the case indices to find the first one that 
            // evaluates to false.

            // Get the bound of the case indices
            let n_cases = cases.len();

            // Initialize these to n_cases and default so that if none of the checks work out 
            // it'll point to the default case.
            case_index = n_cases;
            case_struct = default.clone();
        
            // Iterate through the case indices and look up each case. Once a case evaluates to
            // true, overwrite case_index and case_struct with that case and break. If none evaluate
            // to true, then the loop will exit and case_index and case_struct will point to the 
            // default since they'll be unchanged. The loop can start at next_case since the previous
            // cases would have been already checked.
            for i in *next_case..n_cases {

                // Look up the case ident for this index and add it to the dependencies list
                let case_id = self.id.get_attr_ident(StructureAttrType::SwitchCase(i));
                parents.insert(case_id.clone().to_abstract());

                // Attempt to look up the case ident as a boolean. If the lookup fails, update next_case
                // with the progress made and return incomplete with the dependencies found so far. Note 
                // that this list may be incomplete, but we can't know the full dependency list until we 
                // know what case we're dealing with.
                let check = match vd.lookup_struct_attr(&case_id) {
                    Some(ev) => ev.expect_bool()?,
                    None => {
                        *next_case = i;
                        return Ok(DependencyReport::incomplete(parents))
                    }
                };

                // If the case is true, overwrite case_index and case_struct and exit the loop. Also 
                // update next_case for completeness, even though it shouldn't need to be accessed 
                // again.
                if check {
                    *next_case = i;
                    case_index = i;
                    case_struct = cases[i].1.clone();
                    break;
                }
            }

        } else {
            // If self is not a Switch, then it is not a valid ident
            return Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
        }

        // Now that the case ID has been found along with it's corresponding structure, we need to actually
        // make the PRS for that structure so that the child exists. Start by grabbing self's monikers_for_child
        // since that can't be done once we mutably access self.
        let child_monikers = self.monikers_for_child();

        // Mutably access self's stype so that the prs can be populated
        if let PartiallyResolvedStructureType::Switch{ref mut prs, ..} = self.stype.as_mut() {
            // Get the index path for the child. It'll be the same as self's index path with a 0 appended to it
            // since it's always going to be an only child.
            let mut child_index = self.index_path.clone();
            child_index.push(0); 

            // Create the prs, initialize it's monikers, and store it in self's stype
            let child_prs = PartiallyResolvedStructure::new_indexed(case_struct, self.id.clone(), child_index, prs_map);
            child_prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers);
            *prs = Some(child_prs);

        } else {
            // This is unreachable since the previous immutable access returns early if self is not a Switch
            unreachable!()
        }

        // Lastly, create a successful dependency report with the identified dependencies and return it.
        let mut dr = DependencyReport::success(ExprValue::Integer(case_index as i64));
        dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
        Ok(dr)

    }

    fn is_breakable(&self) -> bool {
        #[cfg(test)] {
            // Panic during tests if self is not a repeating structure
            match self.stype.as_ref() {
                PartiallyResolvedStructureType::Repeat{..} | PartiallyResolvedStructureType::RepeatUntil{..} | PartiallyResolvedStructureType::Chain{..} => {
                    // Do nothing
                },
                _ => panic!("is_breakable called for non-repeating structure '{}'", self.id)
            }
        }

        !self.original.breaks.is_empty()
    }

    /// Attempts to determine if there is a break condition that indicates that the ith repetition
    /// should be the last, and returns the result as a dependency report. If the dependency report
    /// is incomplete, it will contain a complete list of all dependencies that could be needed. 
    /// However, if it's successful, it only returns the dependencies that were needed in order to
    /// determine the result. In the case of a false result, since that requires that all break 
    /// conditions evaluate to false, all dependencies will be listed in the report. In the case of 
    /// a true result, only the dependencies of the condition that evaluated to true will be returned.
    fn try_get_break(&self, i: usize, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        if self.original.breaks.is_empty() {
            // If there are no breaks, return false to indicate that no break condition has been met
            return Ok(DependencyReport::success(ExprValue::Bool(false)))
        }

        let target_ident = self.id.get_attr_ident(StructureAttrType::Break(i));

        // Get the PRS at index i
        let last_prs = match self.stype.as_ref() {
            // If self is a Repeat or RepeatUntil, then just access the ith element of the sequence
            PartiallyResolvedStructureType::RepeatUntil{ref seq, ..} | PartiallyResolvedStructureType::Repeat{ref seq, ..} => {
                if i >= seq.len() {
                    #[cfg(test)] {
                        // Panic during tests if the provided index is out of bounds
                        panic!("try_get_break called for out of bounds index {} in structure {}", i, self.id)
                    }

                    // Return an error if the index is out of bounds
                    return Err(FileSpecError::ident_not_found(target_ident.to_abstract()))
                }

                seq[i].clone()
            },
            // If self is a chain, then it's a bit trickier since the first repetition is different
            // from the rest. If i is 0, then the element is just stored in the 'prs' member. Otherwise,
            // the element is stored within an addressed prs in the sequence.
            PartiallyResolvedStructureType::Chain{ref prs, ..} if i == 0 => {
                prs.clone()
            },
            PartiallyResolvedStructureType::Chain{ref seq, ..} => {
                // The chain sequence is populated with members that are wrapped in addressed PRSs, so it should
                // never be anything other than Addressed. Make sure to access the element at i - 1 instead of i
                // since the first prs is not actually in the sequence.

                if i - 1 >= seq.len() {
                    #[cfg(test)] {
                        // Panic during tests if the provided index is out of bounds
                        panic!("try_get_break called for out of bounds index {} in structure {}", i - 1, self.id)
                    }

                    // Return an error if the index is out of bounds
                    return Err(FileSpecError::ident_not_found(target_ident.to_abstract()))
                }

                if let PartiallyResolvedStructureType::Addressed{pss, ..} = seq[i - 1].borrow().stype.as_ref() {
                    pss.clone()
                } else {
                    unreachable!()
                }
            }
            _ => {
                // If self is not a repeating structure, then it is not a valid ident
                return Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
            }
        };

        // This flag is needed in the upcoming iteration to indicate if there were any unresolved
        // dependencies that prevented a break expression from being evaluated.
        let mut incomplete = false;

        // Iterate through the breaks and evaluate the condition. If any evaluate to true, stop
        // iteration and return true. If any can't be evaluated due to unresolved dependencies,
        // then set the incomplete flag and skip over them.
        for b in &self.original.breaks {
            let expr = &b.condition;

            // The break condition is evaluated using the context of the child as opposed to self, which is
            // why last_prs's lookup function is being used. Note that since the expression is defined in
            // whatever file self's structure was defined, self.fname is used.
            match expr.evaluate_expect_bool(&self.fname, &|alias| {last_prs.borrow().lookup_str(&vd, alias)})? {
                Some(is_break) => {
                    // If the evaluation was successful and evaluates to 'true', then everything we need is
                    // right in that expression (nothing from any of the other expressions matters). Create 
                    // a dependency list from the condition's variables and return success.
                    if is_break {
                        let mut dr = DependencyReport::success(ExprValue::Bool(true));
                        let dependencies = last_prs.borrow().convert_expr_vars(b.condition.vars())?;
                        dr.add_pc_pairs(dependencies, HashSet::from([target_ident.to_abstract()]));
                        return Ok(dr)
                    }
                    
                },
                None => {
                    // If there was a lookup error, set the incomplete flag so that if there aren't any
                    // other expressions that evaluate to true (thus returning), an incomplete report will
                    // be returned instead of a success.
                    incomplete = true;
                }
            }
        }

        // If the above loop terminated normally, then it means that none of the break conditions evaluated
        // to true. Depending on whether any were skipped over, that could mean that we should either return
        // incomplete or success(false). Either way, a list of all dependencies will be needed.

        // Get the list of dependencies by concatenating the variables in each break condition within 
        // the context of the child.
        let mut dependencies = HashSet::new();
        for b in &self.original.breaks {
            dependencies.extend(last_prs.borrow().convert_expr_vars(b.condition.vars())?);
        }

        // If any conditions were skipped, return incomplete. Otherwise, return success.
        if incomplete {
            Ok(DependencyReport::incomplete(dependencies))
        } else {
            let mut dr = DependencyReport::success(ExprValue::Bool(false));
            dr.add_pc_pairs(dependencies, HashSet::from([target_ident.to_abstract()]));
    
            Ok(dr)
        }
        
    }

    /// Attempts to determine the number of repetitions of a Repeat, RepeatUntil, or Chain and
    /// returns the result as a dependency report. This method will create children to represent
    /// a repetition once it's confirmed that that repetition exists, regardless of whether the
    /// actual number of repetitions has been determined. Only documents dependencies on 'break'
    /// attributes if the structure actually has break conditions.
    ///
    /// NOTE: In the case of Chain, some dependencies will only be returned in a single call to
    /// this function, and will not be repeated in subsequent calls.
    ///
    /// Returns IdentNotValid if self is not a repeating structure.
    fn try_get_repetitions(&mut self, vd: &ValueDictionary, prs_map: &mut PrsMap<'a>) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        // Document the target ident to be included in dependency reports as needed
        let target_ident = self.id.get_attr_ident(StructureAttrType::Repetitions);

        // Compute the child monikers for creating new repetitions here since it requires borrowing self
        // so cannot be done within a mutable borrow later.
        let child_monikers = self.monikers_for_child();

        // Determine whether self has a break condition now since it requires a mutable borrow of self
        let is_breakable = self.is_breakable();

        // Declare the dependnecies list here so that variables needed to evaluate 'n' (see comment in following
        // logic) can be documented.
        let mut parents = HashSet::new();

        // Pre-compute the evaluated value of n for Repeat structures. This needs to be done ahead of time
        // because it requires a borrow of self so cannot be done within the mutable borrow in the later logic.
        let n_value: Option<Result<Option<i64>, ExprEvalError>>;
        if let PartiallyResolvedStructureType::Repeat{n, ..} = self.stype.as_ref() {
            // If self is a Repeat structure, make sure to add n's variables to the dependencies.
            parents.extend(self.convert_expr_vars(n.vars())?);
            n_value = Some(n.evaluate_expect_integer(&self.fname, &|alias| {self.lookup_str(&vd, alias)}))
        } else {
            // Otherwise, just store None since it does not exist and will not be needed.
            n_value = None
        }

        match self.stype.as_mut() {
            PartiallyResolvedStructureType::Repeat{n, structure, ref mut seq, ref mut finalized, ..} => {

                // Get the result of evaluating the 'n' expression. Unwrap always works since n_value 
                // is set when stype is Repeat
                match n_value.unwrap() { 
                    Ok(Some(v)) => {
                        // If the evaluation of n returned a value try to add new PRSs into seq until
                        // that number is hit. It's possible that seq already has some members in it,
                        // so we need to account for that.

                        if v < 0 {
                            // Return an error if n evaluated to a negative value
                            if v.is_negative() {
                                let mut err = FileSpecError::value_error("'n' evaluated to a negative integer".to_string(), n.span());
                                if let Some(fname) = &self.fname {
                                    err.load_context(fname.to_string())?;
                                }
                                return Err(err)
                            }
                        }

                        // Make a new variable n_repetations that will store the true number of repetitions.
                        // Initialize it to the value of the n expression, but this could change if a break
                        // condition is encountered.
                        let mut n_repetitions = v;

                        // Get the current number of members in the sequence for the start of the iterator 
                        // coming up
                        let current_len = seq.len();

                        #[cfg(test)] {
                            // Panic during tests if the sequence is somehow larger than n, which should be 
                            // the max size.
                            if current_len as i64 > n_repetitions {
                                panic!("seq in {:?} somehow got to be larger than n", self.id);
                            }  
                        }

                        // Iterate from the current length until the value of the n expression to add members to
                        // seq.
                        for i in current_len..v as usize {
                            // If this isn't the first member (in which case seq would be empty) and the structure
                            // contains break conditions, we need to check the break condition of the previous repetition
                            // before adding a new one.
                            if i != 0 && is_breakable {
                                // Grab the ident for the break condition of the previous repetition, add it to the
                                // dependencies list, and attempt to look it up. Return incomplete if the lookup fails.
                                let break_id = self.id.clone().get_attr_ident(StructureAttrType::Break(i - 1));
                                parents.insert(break_id.clone().to_abstract());
                                let is_break = match vd.lookup_struct_attr(&break_id) {
                                    Some(ev) => ev.expect_bool()?,
                                    None => return Ok(DependencyReport::incomplete(parents))
                                };
                                // If the break condition is met, overwrite n_repetitions with i (the number of repetitions
                                // when the break condition was met) and exit the loop without adding another repetiton.
                                if is_break {
                                    n_repetitions = i as i64;
                                    break;
                                }
                            }

                            // Assuming that the previous logic did not end up breaking the loop, it's safe to add a new repetition.
                            // Calculate the index of the new repetition by adding i (the index of the repetition) to self's index.
                            // Create the PRS, initialize it's monikers, and add it to seq.
                            let mut child_index = self.index_path.clone();
                            child_index.push(i);
                            let mut prs = PartiallyResolvedStructure::new_indexed(structure.clone(), self.id.clone(), child_index, prs_map);
                            prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
                            seq.push(prs);
                        }

                        // The above loop will exit either because a break condition was hit or because seq is populated up until the 
                        // value of n. Either way, seq is now complete and n_repetitions hold the number of repetitions. Set the finalized
                        // flag and construct and return a dependency report with the number of repetitions with the dependencies needed.
                        *finalized = true;
                        let mut dr = DependencyReport::success(ExprValue::Integer(n_repetitions));
                        dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
                        Ok(dr)
                    },
                    Ok(None) => {
                        // If the evaluation of n failed due to a lookup error, return an incomplete report
                        Ok(DependencyReport::incomplete(parents))
                    },
                    Err(err) => {
                        // Any other errors are a problem
                        Err(FileSpecError::from(err))
                    }
                }
            },
            PartiallyResolvedStructureType::RepeatUntil{structure, ref mut seq, ref mut finalized, ..} => {
                // For RepeatUntil structures, check if the current most recent repetition has a break condition and 
                // check if it's end location is equal to self's end location. Otherwise, add another member to the
                // sequence and return incomplete.

                // If the structure is already finalized, just return the length of the existing sequence
                if *finalized {
                    #[cfg(test)] {
                        // Panic during tests if this was called on a already finalized structure
                        panic!("try_get_repetitions called on {:?} after it was finalized", self.id);
                    }
                    return Ok(DependencyReport::success(ExprValue::Integer(seq.len() as i64)));
                }

                // If there are break conditions and there is at least one member of the sequence, check if it has met 
                // a break condition
                if !seq.is_empty() && is_breakable {
                    // Get the ident for the break condition of the most recent repetition, add it to the dependencies
                    // list, and attempt to look it up. If the lookup fails, return an incomplete report.
                    let break_id = self.id.clone().get_attr_ident(StructureAttrType::Break(seq.len() - 1));
                    parents.insert(break_id.clone().to_abstract());
                    let is_break = match vd.lookup_struct_attr(&break_id) {
                        Some(ev) => ev.expect_bool()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };

                    // If the break condition is met, set the finalized flag and return the current sequence length. Make
                    // sure to add the dependencies to the dependency report.
                    if is_break {
                        *finalized = true;
                        let mut dr = DependencyReport::success(ExprValue::Integer(seq.len() as i64));
                        dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
                        return Ok(dr)
                    }
                }

                // If a break condition hasn't been met, check if the end of the most recent repetition exceeds or 
                // is equal to self's end position. 
                // The end of the current repetition is either the start of the structure (if no repetitions have
                // been estabilished yet) or the end of the last established repetition. Get the ident of whichever
                // is applicable and add it to the dependencies list.
                let current_end_id = if seq.is_empty() {
                    self.id.get_attr_ident(StructureAttrType::Start)
                } else {
                    let last_index = seq.len() - 1;
                    seq[last_index].borrow().id.get_attr_ident(StructureAttrType::End)
                };
                
                parents.insert(current_end_id.clone().to_abstract());

                // Also add self's end id to the dependencies list
                let end_id = self.id.get_attr_ident(StructureAttrType::End);
                parents.insert(end_id.clone().to_abstract());

                // Try to look up the two end positions. Return incomplete if either fails
                let current_end = match vd.lookup_struct_attr(&current_end_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                let end = match  vd.lookup_struct_attr(&end_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                
                if current_end < end {
                    // If the end of the most recent repetition has not met or exceeded self's end, then add another repetition.
                    // Calculate the child's index by pushing the index of the repetition (the sequence length) to self's index.
                    // Initialize the new PRS's monikers
                    let mut child_index = self.index_path.clone();
                    child_index.push(seq.len());
                    let prs = PartiallyResolvedStructure::new_indexed(structure.clone(), self.id.clone(), child_index, prs_map);
                    prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());

                    // Add the new child's end attribute and (if there are any break conditions) the child's break ID to the 
                    // dependencies so they will be found by the next time this is called.
                    parents.insert(prs.borrow().id.get_attr_ident(StructureAttrType::End).to_abstract());
                    if is_breakable {
                        let break_id = self.id.clone().get_attr_ident(StructureAttrType::Break(seq.len()));
                        parents.insert(break_id.clone().to_abstract());
                    }
                    
                    // Add the new PRS to the sequence and return incomplete.
                    seq.push(prs);
                    Ok(DependencyReport::incomplete(parents))
                
                } else if current_end > end {

                    todo!("Last repetition exceeded end in {:?}", self.id)

                } else {
                    // Otherwise, the most recent repetition must be the last. Set the finalized flag and return the current 
                    // sequence length. Add the dependencies list to the report.

                    *finalized = true;
                    let mut dr = DependencyReport::success(ExprValue::Integer(seq.len() as i64));
                    dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
                    Ok(dr)
                }
            },
            PartiallyResolvedStructureType::Chain{next, structure, prs, ref mut seq, ref mut finalized} => {
                // Chain structures will always repeat until a break is encountered. A chain without any break
                // conditions is an error.

                if !is_breakable {
                    todo!("Unbreakable chain encountered: {:?}", self.id);
                }
                
                let next = next.clone(); // Need this for the borrow checker

                // If the structure is already finalized, just return the length of the existing sequence
                if *finalized {
                    #[cfg(test)] {
                        // Panic during tests if this was called on a already finalized structure
                        panic!("try_get_repetitions called on {:?} after it was finalized", self.id);
                    }
                    return Ok(DependencyReport::success(ExprValue::Integer(seq.len() as i64 + 1)));
                }

                // Get the ident for the break of the most recent repetition. Keep in mind that Chain structures
                // always have at least one repetition so this is always valid, even is the sequence is empty 
                // (the first repetition is stored in prs)
                let break_id = self.id.clone().get_attr_ident(StructureAttrType::Break(seq.len()));

                // Add the ident to the dependencies list and attempt to look it up. If the lookup fails, then
                // return incomplete.
                parents.insert(break_id.clone().to_abstract());
                let is_break = match vd.lookup_struct_attr(&break_id) {
                    Some(ev) => ev.expect_bool()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
                
                // If a break condition is met, set the finalized flag and return the current sequence length
                // plus one (to account for prs). Make sure to add the dependencies list.
                if is_break {
                    *finalized = true;
                    let mut dr = DependencyReport::success(ExprValue::Integer(seq.len() as i64 + 1));
                    dr.add_pc_pairs(parents, HashSet::from([target_ident.to_abstract()]));
                    return Ok(dr)
                }

                // If a break condition hasn't been encountered, we'll need to add another repetition. Start by
                // grabbing a reference to the most recent repetition since that's the context in which we'll have
                // to evaluate the 'next' expression. 
                let last_member = if seq.is_empty() {
                    // If the sequence is empty, then the most recent repetition is the first one, stored in prs.
                    // Return that, but not before adding the next expression's variables in it's context to the 
                    // dependencies list. This is needed in order to guarantee that the dependencies are documented
                    // for the first sequence entry. Subsequent depenedencies will be documented each time a new 
                    // repetition is added to the sequence. This is critical because these dependencies will
                    // not be documented again in subsequent calls.
                    parents.extend(prs.borrow().convert_expr_vars(next.vars())?);
                    prs.clone()
                    
                } else {
                    // If the sequence isn't empty, then the most recent repetition is the last member of the sequence.
                    // Since each repetition is wrapped in an (fake) Addressed structure, we actually need to grab the content
                    // from that addressed structure. 
                    let n = seq.len() - 1;
                    if let PartiallyResolvedStructureType::Addressed{pss, ..} = seq[n].borrow().stype.as_ref() {
                        pss.clone()
                    } else {
                        // This is unreachable since every child added to seq is wrapped in an Addressed structure.
                        unreachable!()
                    }
                };
                
                // Add the variables needed to evaluate 'next' in the last repetition's context to the dependencies list
                // TODO: this may not be needed since the dependencies should have been added in the previous call, when
                // the child was first created.
                // parents.extend(last_member.borrow().convert_expr_vars(next.vars())?);

                // Try to evaluate 'next' in the context of the last repetition. Keep in mind that self's fname is still
                // used here since the actual expression is defined in whatever file contains self's definition.
                match next.evaluate_expect_position(&self.fname, &|alias| {last_member.borrow().lookup_str(&vd, alias)}) {
                    Ok(Some(v)) => {

                        // Return an error if next evaluated to a negative position
                        if v.is_negative() {
                            let mut err = FileSpecError::value_error("'next' evaluated to a negative position".to_string(), next.span());
                            if let Some(fname) = &self.fname {
                                err.load_context(fname.to_string())?;
                            }
                            return Err(err)
                        }

                        // If the evaluation was successful, add another repetition at the resulting address. Compute the 
                        // index by adding the repetition index (the sequence length plus one to account for prs) to self's
                        // index and crate the PRS.
                        let mut child_index = self.index_path.clone();
                        child_index.push(seq.len() + 1); // + 1 to account for prs
                        let mut child = PartiallyResolvedStructure::new_indexed(structure.clone(), self.id.clone(), child_index, prs_map);
                        
                        // child must be an Addressed structure since "structure" was wrapped in an Addressed structure during
                        // self's initialization. Overwrite the position with the result of evaluating 'next'.
                        if let PartiallyResolvedStructureType::Addressed{ref mut position, ..} = child.borrow_mut().stype.as_mut() {
                            *position = Rc::new(ExprWrapper::from_default(Expr::value(ExprValue::Position(v))));
                        } else {
                            unreachable!()
                        }
                        
                        // Initialize the new repetition's monikers
                        child.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());

                        // Add the dependencies needed to evaluate 'next' in the new child's context to the dependencies list
                        // so that they'll be available for the next iteration. This is critical because these dependencies will
                        // not be documented again in subsequent calls.
                        if let PartiallyResolvedStructureType::Addressed{pss, ..} = child.borrow().stype.as_ref() {
                            parents.extend(pss.borrow().convert_expr_vars(next.vars())?);
                        } else {
                            unreachable!()
                        }
                        
                        // Add the new repetition to the sequence, and return an incomplete report
                        seq.push(child);

                        return Ok(DependencyReport::incomplete(parents));
                    },
                    Ok(None)=> {
                        // If the evaluation failed due to a lookup error, return incomplete with the dependencies list already found
                        Ok(DependencyReport::incomplete(parents))
                    },
                    Err(err) => {
                        // Any other errors are an actual problem.
                        Err(FileSpecError::from(err))
                    }
                }
            },
            _ => {
                // If self is not a repeating structure, then it is not a valid ident
                return Err(FileSpecError::ident_not_valid(target_ident.to_abstract()))
            }
        }
    }

    fn try_get_position(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {
        if let PartiallyResolvedStructureType::Addressed{position, ..} = self.stype.as_ref() {
            let target_ident = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
            self.try_evaluate_position_expression(&position, target_ident, vd)
        } else {
            // Note: This is discounting the possibility that the start location could be determined by 
            // measuring backward from some known position, but that can be avoided by revising the file spec
            Ok(DependencyReport::does_not_exist())
        }  
    }

    fn try_get_alignment(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {
        let target_ident = self.id.get_attr_ident(StructureAttrType::Align).to_abstract();
        self.try_evaluate_position_expression(&self.alignment, target_ident, vd)
    }

    fn try_get_alignment_base(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {
        let target_ident = self.id.get_attr_ident(StructureAttrType::AlignBase).to_abstract();
        self.try_evaluate_position_expression(&self.alignment_base, target_ident, vd)
    }

    fn try_get_start_pad(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        let alignment_id = self.id.get_attr_ident(StructureAttrType::Align);
        let alignment_base_id = self.id.get_attr_ident(StructureAttrType::AlignBase);
        let position_id = self.id.get_attr_ident(StructureAttrType::Position);
        let parents = HashSet::from([alignment_id.clone().to_abstract(), alignment_base_id.clone().to_abstract(), position_id.clone().to_abstract()]);

        let alignment = match vd.lookup_struct_attr(&alignment_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(parents))
        };

        let alignment_base = match vd.lookup_struct_attr(&alignment_base_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(parents))
        };

        let position = match vd.lookup_struct_attr(&position_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(parents))
        };

        let remainder = (&position - &alignment_base).rem_euclid(&alignment);

        let sp = if remainder.is_zero() {
            BitIndex::zero()
        } else {
            alignment - remainder
        };

        let mut dr = DependencyReport::success(ExprValue::Position(sp));
        let start_pad_id = self.id.get_attr_ident(StructureAttrType::StartPad).to_abstract();
        dr.add_pc_pairs(parents, HashSet::from([start_pad_id]));
        Ok(dr)
    }

    fn try_get_start(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        let start_pad_id = self.id.get_attr_ident(StructureAttrType::StartPad);
        let position_id = self.id.get_attr_ident(StructureAttrType::Position);
        let parents = HashSet::from([start_pad_id.clone().to_abstract(), position_id.clone().to_abstract()]);

        let start_pad = match vd.lookup_struct_attr(&start_pad_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(parents))
        };

        let position = match vd.lookup_struct_attr(&position_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(parents))
        };

        let mut dr = DependencyReport::success(ExprValue::Position(position + start_pad));
        let start_id = self.id.get_attr_ident(StructureAttrType::Start).to_abstract();
        dr.add_pc_pairs(parents, HashSet::from([start_id]));
        Ok(dr)
    }

    /// Attempts to determine the length of self and returns the result as a dependency report. If self's
    /// length-policy is expand, then the length is computed as self's parent's spare length and self's parent's
    /// spare length is the only dependency. If the length-policy is an expression, then the length is computed
    /// by evaluating that expression and the expression's variables are returned as the dependencies. If the
    /// length-policy is fit contents, then the length is equal to self's content length, and that is the only
    /// dependency.
    fn try_get_length(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        // Get ident that's being computed so it can be included in the report
        let target_ident = self.id.get_attr_ident(StructureAttrType::Length);

        // Determine the ident that will represent a length equivalent to self's length, based on self's length
        // policy. If the length policy is expand, then it's self's parent's spare length. If the length policy
        // if fit contents, then it's self's contents length. If the length policy is an expression, then there's
        // no other attribute that equivalent to self's length. Just try to evaluate the expression and return
        // the result immediately.
        let equivalent_length_id = match &self.length {
            LengthPolicy::Expand => {
                self.parent_id.get_attr_ident(StructureAttrType::SpareLength)
            },
            LengthPolicy::FitContents => {
                self.id.get_attr_ident(StructureAttrType::ContentsLength)
            },
            LengthPolicy::Expr(expr) => {
                return self.try_evaluate_position_expression(&expr, target_ident.to_abstract(), vd)
            }
        };

        // If an equivalent length was found, try to look it up. If the lookup fails, return an incomplete report.
        let length = match vd.lookup_struct_attr(&equivalent_length_id) {
            Some(ev) => ev.expect_position()?,
            None => return Ok(DependencyReport::incomplete(HashSet::from([equivalent_length_id.to_abstract()])))
        };

        // If the lookup was successful, return the result in a dependency report. Make sure to include the ID
        // that was used as the equivalent length as the only depenency.
        let mut dr = DependencyReport::success(ExprValue::Position(length));
        dr.add_pc_pairs(HashSet::from([equivalent_length_id.to_abstract()]), HashSet::from([target_ident.to_abstract()]));
        Ok(dr)
    }

    /// Attempts to determine self's "spare length", which is essentially the length that's allocated to any
    /// child with an expand length policy.
    fn try_get_spare_length(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {

        match self.length {
            LengthPolicy::FitContents => {
                todo!("No spare length in fit contents! (should this be zero?)")
            },
            _ => {}
        }

        let mut parents = HashSet::new();
        let length_id = self.id.get_attr_ident(StructureAttrType::Length);
        parents.insert(length_id.clone().to_abstract());

        match self.stype.as_ref() {
            PartiallyResolvedStructureType::Sequence(structures) => {
                let mut expand_found = false;
                for structure in structures {
                    match structure.borrow().length {
                        LengthPolicy::Expand => {
                            if expand_found {
                                panic!("{} contains two expand children!", self.id);
                            } else {
                                expand_found = true;
                            }
                        },
                        _ => {
                            let struct_length_id = structure.borrow().id.get_attr_ident(StructureAttrType::Length);
                            parents.insert(struct_length_id.to_abstract());
                        }
                    }
                }

                let mut length = match vd.lookup_struct_attr(&length_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                for structure in structures {
                    match structure.borrow().length {
                        LengthPolicy::Expand => {},
                        _ => {
                            let struct_length_id = structure.borrow().id.get_attr_ident(StructureAttrType::Length);
                            let child_length = match vd.lookup_struct_attr(&struct_length_id) {
                                Some(ev) => ev.expect_position()?,
                                None => return Ok(DependencyReport::incomplete(parents))
                            };
                            length = length - child_length;
                        }
                    }
                }

                let mut dr = DependencyReport::success(ExprValue::Position(length));
                let spare_length_id = self.id.get_attr_ident(StructureAttrType::SpareLength).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([spare_length_id]));
                Ok(dr)
            },
            _ => {
                let length = match vd.lookup_struct_attr(&length_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
                let mut dr = DependencyReport::success(ExprValue::Position(length));
                let spare_length_id = self.id.get_attr_ident(StructureAttrType::SpareLength).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([spare_length_id]));
                Ok(dr)
            }
        } 
    }

    fn try_resolve_import(&self, key: String) -> Result<DependencyReport<StructureIdent>, FileSpecError> {
        info!("Trying to resolve import in {:?}", self.id);
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{..} | PartiallyResolvedStructureType::Segment => {

                if self.exports.contains(&key) {
                    Ok(DependencyReport::success(self.id.clone()))
                } else {
                    Ok(DependencyReport::does_not_exist())
                }
                
            },
            PartiallyResolvedStructureType::Addressed{position, pss, ..} => {

                if self.exports.contains(&key) {

                    pss.borrow().try_resolve_import(key)
                } else {
                    Ok(DependencyReport::does_not_exist())
                }
                
            },
            PartiallyResolvedStructureType::Sequence(structures) => {
                let mut current_best = DependencyReport::does_not_exist();
                for structure in structures {
                    if structure.borrow().exports.contains(&key) {
                        match structure.borrow().try_resolve_import(key.clone()) {
                            Ok(dr) => match dr.result {
                                DepResult::Success(ref ev) => return Ok(dr),
                                DepResult::Incomplete(_) => return Ok(dr),
                                DepResult::MightExist(ref vars) => {
                                    current_best = dr;
                                },
                                DepResult::DoesNotExist => {
                                    
                                }
                            },
                            // Err(FileSpecError::IdentNotFound(_)) => {todo!("Not sure if this is right...")},
                            Err(err) => return Err(err)
                        }
                    }
                }

                Ok(current_best)
            },
            PartiallyResolvedStructureType::Switch{value, cases, default, prs, ..} => {

                if let Some(prs) = prs {
                    // If the swich case is known, then this should be populated. It is all we need
                    prs.borrow().try_resolve_import(key)

                } else {
                    
                    if default.exports.contains_key(&key) {
                        let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex).to_abstract();
                        return Ok(DependencyReport::might_exist(HashSet::from([switch_index_id])))
                    }

                    for (check, case) in cases {
                        if case.exports.contains_key(&key) {
                            let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex).to_abstract();
                            return Ok(DependencyReport::might_exist(HashSet::from([switch_index_id])))
                        }
                    }

                    Ok(DependencyReport::does_not_exist())

                }
            },
            PartiallyResolvedStructureType::Repeat{structure, seq, finalized, ..} | PartiallyResolvedStructureType::RepeatUntil{structure, seq, finalized, ..} => {
                let mut current_best;

                if structure.exports.contains_key(&key) {
                    if *finalized {
                        info!("Current best is DNE");
                        current_best = DependencyReport::does_not_exist()
                    } else {
                        info!("Current best is ME");
                        let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract();
                        current_best = DependencyReport::might_exist(HashSet::from([repetitions_id]));
                    }
                } else {
                    info!("Structure exports does not contain key!");
                    return Ok(DependencyReport::does_not_exist())
                }

                for structure in seq {
                    let dr = structure.borrow().try_resolve_import(key.clone())?;
                    info!("result: {:?}", dr.result);
                    match dr.result {
                        DepResult::Success(_) | DepResult::Incomplete(_) => return Ok(dr),
                        DepResult::MightExist(_) => current_best = dr,
                        DepResult::DoesNotExist => {}
                    }
                } 

                Ok(current_best)

            },
            PartiallyResolvedStructureType::Chain{structure, prs, seq, finalized, ..} => {
                let mut current_best;

                if structure.exports.contains_key(&key) {
                    if *finalized {
                        info!("Current best is DNE");
                        current_best = DependencyReport::does_not_exist()
                    } else {
                        info!("Current best is ME");
                        let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract();
                        current_best = DependencyReport::might_exist(HashSet::from([repetitions_id]));
                    }
                } else {
                    info!("Structure exports does not contain key!");
                    return Ok(DependencyReport::does_not_exist())
                }

                let dr = prs.borrow().try_resolve_import(key.clone())?;
                info!("result: {:?}", dr.result);
                match dr.result {
                    DepResult::Success(_) | DepResult::Incomplete(_) => return Ok(dr),
                    DepResult::MightExist(_) => current_best = dr,
                    DepResult::DoesNotExist => {}
                }

                for structure in seq {
                    let dr = structure.borrow().try_resolve_import(key.clone())?;
                    info!("result: {:?}", dr.result);
                    match dr.result {
                        DepResult::Success(_) | DepResult::Incomplete(_) => return Ok(dr),
                        DepResult::MightExist(_) => current_best = dr,
                        DepResult::DoesNotExist => {}
                    }
                } 

                Ok(current_best)

            },
            PartiallyResolvedStructureType::Assembly{structure, prs, ..} => {
                if structure.exports.contains_key(&key) {
                    prs.borrow().try_resolve_import(key.clone())
                } else {
                    info!("Structure exports does not contain key!");
                    Ok(DependencyReport::does_not_exist())
                }

            }
        }
    }

    /// Tries to compute the structure length based on contents
    fn try_get_contents_length(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                let section = initial;
                match &section.length {
                    LengthPolicy::Expand => {
                        return Ok(DependencyReport::does_not_exist())
                    },
                    LengthPolicy::Expr(expr) => {

                        let target_ident = self.id.get_attr_ident(StructureAttrType::ContentsLength).to_abstract();
                        self.try_evaluate_position_expression(&expr, target_ident, vd)

                    },
                    LengthPolicy::FitContents => {
                        if section.fields.is_empty() {
                            Ok(DependencyReport::success(ExprValue::Position(BitIndex::zero())))
                        } else {
                            let last_field_index = section.fields.len() - 1;
                            let last_field_id = self.id.clone().get_field_ident(section.fields[last_field_index].id.clone());
                            let last_field_end_id = last_field_id.get_attr_ident(FieldAttrType::End);
                            let position_id = self.id.get_attr_ident(StructureAttrType::Position);

                            let parents = HashSet::from([last_field_end_id.clone().to_abstract(), position_id.clone().to_abstract()]);

                            let contents_end = match vd.lookup_field_attr(&last_field_end_id) {
                                Some(value) => value.expect_position()?,
                                None => return Ok(DependencyReport::incomplete(parents))
                            };
                            let position = match vd.lookup_struct_attr(&position_id) {
                                Some(value) => value.expect_position()?,
                                None => return Ok(DependencyReport::incomplete(parents))
                            };

                            let mut dr = DependencyReport::success(ExprValue::Position(contents_end - position));
                            let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                            dr.add_pc_pairs(parents, HashSet::from([contents_length_id]));
                            Ok(dr)

                        }
                        
                    }
                }

            },
            PartiallyResolvedStructureType::Addressed{pss, ..} => {

                let length_id = pss.borrow().id.get_attr_ident(StructureAttrType::Length);
                match vd.lookup_struct_attr(&length_id) {
                    Some(value) => {
                        let mut dr = DependencyReport::success(value);
                        let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                        dr.add_pc_pairs(HashSet::from([length_id.to_abstract()]), HashSet::from([contents_length_id]));
                        Ok(dr)
                    },
                    None => Ok(DependencyReport::incomplete(HashSet::from([length_id.to_abstract()])))
                }

            },
            PartiallyResolvedStructureType::Sequence(structures) => {
                let mut length = BitIndex::zero();
                let mut parents = HashSet::new();
                for structure in structures {
                    if structure.borrow().is_inline() {
                        let start_pad_id = structure.borrow().id.clone().get_attr_ident(StructureAttrType::StartPad);
                        let length_id = structure.borrow().id.clone().get_attr_ident(StructureAttrType::Length);
                        parents.insert(start_pad_id.clone().to_abstract());
                        parents.insert(length_id.clone().to_abstract());

                        match vd.lookup_struct_attr(&start_pad_id) {
                            Some(value) => {
                                length = length + value.expect_position()?;
                            },
                            None => return Ok(DependencyReport::incomplete(parents))
                        }
                        match vd.lookup_struct_attr(&length_id) {
                            Some(value) => {
                                length = length + value.expect_position()?;
                            },
                            None => return Ok(DependencyReport::incomplete(parents))
                        }
                    }
                }
                let mut dr = DependencyReport::success(ExprValue::Position(length));
                let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([contents_length_id]));
                Ok(dr)
            },
            PartiallyResolvedStructureType::Switch{value, cases, default, prs, ..} => {
                // Note: This is discounting the possibility that the length could be determined
                // by finding that the lengths of all cases are identical
                let mut parents = HashSet::new();

                let switch_value_id = self.id.get_attr_ident(StructureAttrType::SwitchValue).to_abstract();
                parents.insert(switch_value_id);

                if let Some(prs) = prs {
                    if prs.borrow().is_inline() {
                        let struct_length_id = prs.borrow().id.get_attr_ident(StructureAttrType::Length);
                        let struct_start_pad_id = prs.borrow().id.get_attr_ident(StructureAttrType::StartPad);
                        parents.insert(struct_length_id.clone().to_abstract());
                        parents.insert(struct_start_pad_id.clone().to_abstract());
        
                        let length = match vd.lookup_struct_attr(&struct_length_id) {
                            Some(bp) => bp.expect_position()?,
                            None => { 
                                return Ok(DependencyReport::incomplete(parents))
                            }
                        };
        
                        let start_pad = match vd.lookup_struct_attr(&struct_start_pad_id) {
                            Some(bp) => bp.expect_position()?,
                            None => {
                                return Ok(DependencyReport::incomplete(parents))
                            }
                        };
                        let mut dr = DependencyReport::success(ExprValue::Position(length + start_pad));
                        let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                        dr.add_pc_pairs(parents, HashSet::from([contents_length_id]));
                        return Ok(dr)
                    } else {
                        let mut dr = DependencyReport::success(ExprValue::Position(BitIndex::zero()));
                        let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                        dr.add_pc_pairs(parents, HashSet::from([contents_length_id]));
                        return Ok(dr)
                    }
                } else {
                    // todo!()
                    return Ok(DependencyReport::incomplete(parents))
                }

            },
            PartiallyResolvedStructureType::Repeat{seq, finalized, ..} => {
                let mut parents = HashSet::new();

                let mut length = BitIndex::zero();

                let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions);

                parents.insert(repetitions_id.clone().to_abstract());
                match vd.lookup_struct_attr(&repetitions_id) {
                    Some(_) => {
                        // If repetions is not None, then the "seq" variable should already be populated.
                        // No processing is needed here.
                    },
                    None => return Ok(DependencyReport::incomplete(parents))
                }

                if *finalized {
                    // If finalized, just treat it like a Sequence
                    for structure in seq {
                        if structure.borrow().is_inline() {
                            let struct_length_id = structure.borrow().id.get_attr_ident(StructureAttrType::Length);
                            let struct_start_pad_id = structure.borrow().id.get_attr_ident(StructureAttrType::StartPad);
                            parents.insert(struct_length_id.clone().to_abstract());
                            parents.insert(struct_start_pad_id.clone().to_abstract());

                            match vd.lookup_struct_attr(&struct_start_pad_id) {
                                Some(value) => {
                                    length = length + value.expect_position()?;
                                },
                                None => return Ok(DependencyReport::incomplete(parents))
                            }

                            match vd.lookup_struct_attr(&struct_length_id) {
                                Some(value) => {
                                    length = length + value.expect_position()?;
                                },
                                None => return Ok(DependencyReport::incomplete(parents))
                            }
                        }
                    }

                    let mut dr = DependencyReport::success(ExprValue::Position(length));
                    let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                    dr.add_pc_pairs(parents, HashSet::from([contents_length_id]));
                    Ok(dr)
                } else { 
                    todo!("Cannot determine for unfinalized repeat")
                }

            },
            PartiallyResolvedStructureType::RepeatUntil{..} => {
                todo!("this won't work");
                let mut parents = HashSet::new();
                let start_id = self.id.get_attr_ident(StructureAttrType::Start);
                let end_id = self.id.get_attr_ident(StructureAttrType::End);
                parents.insert(start_id.clone().to_abstract());
                parents.insert(end_id.clone().to_abstract());
                let start = match vd.lookup_struct_attr(&start_id) {
                    Some(value) => value.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
                let end = match vd.lookup_struct_attr(&end_id) {
                    Some(value) => value.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                let mut dr = DependencyReport::success(ExprValue::Position(end - start));
                let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([contents_length_id]));
                Ok(dr)

            },
            PartiallyResolvedStructureType::Chain{prs, ..} => {
                // Same as addressed. The sequential structures don't matter since they're all addressed
                let length_id = prs.borrow().id.get_attr_ident(StructureAttrType::Length);
                match vd.lookup_struct_attr(&length_id) {
                    Some(value) => {
                        let mut dr = DependencyReport::success(value);
                        let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                        dr.add_pc_pairs(HashSet::from([length_id.to_abstract()]), HashSet::from([contents_length_id]));
                        Ok(dr)
                    },
                    None => Ok(DependencyReport::incomplete(HashSet::from([length_id.to_abstract()])))
                }

            },
            PartiallyResolvedStructureType::Assembly{..} | PartiallyResolvedStructureType::Segment => {
                error!("Trying to get contents length of {:?}", self.id);
                let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                Err(FileSpecError::ident_not_valid(contents_length_id))
            }
        }        
    }

    fn try_get_end(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, FileSpecError> {
        if let PartiallyResolvedStructureType::RepeatUntil{end, ..} = self.stype.as_ref() {
            let target_ident = self.id.get_attr_ident(StructureAttrType::End).to_abstract();
            self.try_evaluate_position_expression(&end, target_ident, vd)
        } else {
            let mut parents = HashSet::new();
            let start_id = self.id.get_attr_ident(StructureAttrType::Start);
            let length_id = self.id.get_attr_ident(StructureAttrType::Length);
            parents.insert(start_id.clone().to_abstract());
            parents.insert(length_id.clone().to_abstract());
            let start = match vd.lookup_struct_attr(&start_id) {
                Some(value) => value.expect_position()?,
                None => return Ok(DependencyReport::incomplete(parents))
            };
            let length = match vd.lookup_struct_attr(&length_id) {
                Some(value) => value.expect_position()?,
                None => return Ok(DependencyReport::incomplete(parents))
            };

            let mut dr = DependencyReport::success(ExprValue::Position(start + length));
            let end_id = self.id.get_attr_ident(StructureAttrType::End).to_abstract();
            dr.add_pc_pairs(parents, HashSet::from([end_id]));
            Ok(dr)
        }

    }

    /// Attempts to find the closest common ancestor between `self` and `other`. Returns an integer representing
    /// the number of generations back the ancestor is from 'self' as well as the ancestor's identity. Only fails if `other` 
    /// is not in `prs_map` or there is somehow no common ancestor.
    // fn common_ancestor(&self, other: StructureIdent, prs_map: &PrsMap<'a>) -> Result<(usize, StructureIdent), FileSpecError> {
    //     let mut other_ancestors = HashSet::new();
    //     let mut current = other;
    //     let mut next;
    //     loop {
    //         match prs_map.get(current) {
    //             Some(prs) => next = prs.parent_id.clone(),
    //             None => break // Indicates that the parent is the file, which is not the prs_map
    //         }
    //         other_ancestors.insert(current);
    //         current = next;
    //     }

    //     let mut generation = 0;
    //     current = self.id.clone();
    //     loop {
    //         if other_ancestors.contains_key(&current) {
    //             return Ok((generation, current))
    //         }
    //         match prs_map.get(current) {
    //             Some(prs) => current = prs.parent_id.clone(),
    //             None => {
    //                 // Indicates that the parent is the file, which is not the prs_map
    //                 // return Err(FileSpecError::IdentNotFound())
    //                 panic!("No common ancestor between {:?} and {:?}", self.id, other);
    //             }
    //         }
    //     }
    // }


    fn nearest_cousins(&self, id: String, prs_map: &PrsMap<'a>) -> Result<Vec<StructureIdent>, FileSpecError> {

        let mut ancestor_id = self.id.clone();
        let mut cousin: Option<Rc<Structure>> = None;
        loop {
            if let Some(ancestor) = prs_map.get(&ancestor_id) {
                if let Some(child) = ancestor.borrow().original.get_child_by_id(&id) {
                    cousin = Some(child);
                    break;
                } else {
                    ancestor_id = ancestor.borrow().parent_id.clone();
                }
            } else {
                break;
            }
        }

        if let Some(cousin) = cousin {
            let mut cousins = Vec::new();
            for prs in prs_map.values() {
                if Rc::ptr_eq(&cousin, &prs.borrow().original) {
                    cousins.push(prs.borrow().id.clone());
                }
            }
            Ok(cousins)
        } else {
            panic!("{:?} has no cousins with id {}", self.id, id);
        }

    }  

    fn get_assembly(&self, prs_map: &PrsMap<'a>) -> Result<StructureIdent, FileSpecError> {
        // TODO: Account for if there are multiple prs's that list this as a semgent (get the closest one in the tree)
        for prs in prs_map.values() {
            match prs.borrow().stype.as_ref() {
                PartiallyResolvedStructureType::Assembly{segments, ..} => {
                    if segments.contains(&self.id.id) {
                        return Ok(prs.borrow().id.clone())
                    }
                },
                _ => {}
            }
        }
        todo!()
    }

    fn try_get_data(&self, fm: &mut crate::FileManager, vd: &ValueDictionary, prs_map: &HashMap<StructureIdent, 
                Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<BitField>, FileSpecError> {

        match self.stype.as_ref() {
            PartiallyResolvedStructureType::Assembly{segments, ..} => {
                warn!("Getting segments: {:?}", segments);
                let mut members_set = HashSet::new();
                let mut parents = HashSet::new();
                for segment in segments {
                    for member_id in self.nearest_cousins(segment.clone(), prs_map)? {
                        let start_id = prs_map.get(&member_id).unwrap().borrow().id.clone().get_attr_ident(StructureAttrType::Start);
                        parents.insert(start_id.clone().to_abstract());
                        let end_id = prs_map.get(&member_id).unwrap().borrow().id.clone().get_attr_ident(StructureAttrType::End);
                        parents.insert(end_id.clone().to_abstract());

                        members_set.insert((start_id, end_id, member_id));
                    }
                }

                warn!("members_set: {:?}", members_set);

                let mut members = Vec::new();
                for (start_id, end_id, member_id) in members_set.into_iter() {
                    let start = match vd.lookup_struct_attr(&start_id) {
                        Some(ev) => ev.expect_position()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };

                    let end = match vd.lookup_struct_attr(&end_id) {
                        Some(ev) => ev.expect_position()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };

                    members.push((start, end, member_id));
                }
                members.sort_by_key(|item| item.0);

                info!("Members: {:?}", members);

                let mut data = BitField::from_vec(vec![]);

                for (start, end, member_id) in members.into_iter() {
                    let span = end - start;
                    let mut buffer = vec![0; end.ceil().byte() - start.byte()];
                    fm.get_bytes(start.byte(), &mut buffer)?;
                    let mut bf = BitField::from_vec(buffer) << start.bit() as usize;
                    bf.truncate(&span);

                    data.extend(&bf);
                }

                let dr = DependencyReport::success(data);
                // TODO: Add dependencies
                Ok(dr)
                
            },
            _ => todo!()
        }

        
    }

    fn unknowns(&self, vd: &ValueDictionary) -> Result<(HashSet<AbstractIdent>, HashSet<AbstractIdent>), FileSpecError> { // (Unknowns that need to be found to resolve fields, unknowns that need to be found to get more unknowns)
        let mut unks = HashSet::new();
        let mut expand_deps = HashSet::new();

        unks.insert(self.id.get_attr_ident(StructureAttrType::Start).to_abstract());
        unks.insert(self.id.get_attr_ident(StructureAttrType::Length).to_abstract());

        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{..} | PartiallyResolvedStructureType::Segment => {
                
                // for field in &initial.fields {
                //     let field_id = self.id.get_field_ident(field.id.clone());
                //     unks.insert(field_id.clone().get_attr_ident(FieldAttrType::Start).to_abstract());
                //     unks.insert(field_id.get_attr_ident(FieldAttrType::Length).to_abstract());
                // }

            },
            PartiallyResolvedStructureType::Addressed{pss, ..} => {
                let (new_unks, new_expand_deps) = pss.borrow().unknowns(vd)?;
                unks.extend(new_unks);
                expand_deps.extend(new_expand_deps);
                
            },
            PartiallyResolvedStructureType::Sequence(structures) => {
                for structure in structures {
                    let (new_unks, new_expand_deps) = structure.borrow().unknowns(vd)?;
                    unks.extend(new_unks);
                    expand_deps.extend(new_expand_deps);
                }
            },
            PartiallyResolvedStructureType::Switch{value, cases, default, prs, ..} => {
                let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex);
                expand_deps.insert(switch_index_id.to_abstract());
                if let Some(prs) = prs {
                    // If the swich case is known, then this should be populated. It is all we need
                    let (new_unks, new_expand_deps) = prs.borrow().unknowns(vd)?;
                    unks.extend(new_unks);
                    expand_deps.extend(new_expand_deps);
                } else {
                    for (i, (check, structure)) in cases.iter().enumerate() {
                        // TODO: This shouldn't require lookup...
                        let sai = self.id.get_attr_ident(StructureAttrType::SwitchCase(i));
                        match vd.lookup_struct_attr(&sai) {
                            Some(b) => {
                                if b.expect_bool()? {
                                    // This shouldn't happen, since if we hit this then we should know
                                    // which case is correct. But if that were true, prs should have been
                                    // Some(_) which would have avoided this loop altogether.
                                    panic!("Unexpected state")
                                } else {
                                    // We know that this is not the switch case, so ignore it.
                                }
                            },
                            None => {
                                // We don't know if this is the switch case or not, so we need to
                                // figure that out to expand the unknown list.
                                expand_deps.insert(sai.to_abstract());
                                return Ok((unks, expand_deps))
                            }
                        }
                    }

                    // This shouldn't happen, since if we hit this then we should know
                    // the default case is correct. But if that were true, prs should have been
                    // Some(_) which would have avoided this loop altogether.
                    panic!("Unexpected state")

                }
            },
            PartiallyResolvedStructureType::Repeat{seq, finalized, ..} => {
                // TODO: This shouldn't require lookup...
                let sai = self.id.get_attr_ident(StructureAttrType::Repetitions);
                match vd.lookup_struct_attr(&sai) {
                    Some(_) => {
                        // If repetions is not None, then the "seq" variable should already be fully populated.
                        for structure in seq {
                            let (new_unks, new_expand_deps) = structure.borrow().unknowns(vd)?;
                            unks.extend(new_unks);
                            expand_deps.extend(new_expand_deps);
                        }  
                    },
                    None => {
                        // We don't know how many repetitions there are, so we need that info until we can
                        // be sure we listed all unknowns
                        expand_deps.insert(sai.to_abstract());
                    }
                }

            },
            PartiallyResolvedStructureType::RepeatUntil{seq, finalized, ..} => {
                // Regardless of whether this structure is finalized, any members in seq are valid. add their
                // unknowns to the set.
                for member in seq {
                    let (new_unks, new_expand_deps) = member.borrow().unknowns(vd)?;
                    unks.extend(new_unks);
                    expand_deps.extend(new_expand_deps);
                } 

                if *finalized {
                    // If the structure is finalized, then the "seq" variable should already be fully populated.
                    // Nothing more is needed
                } else {
                    expand_deps.insert(self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract());
                }

            },
            PartiallyResolvedStructureType::Chain{prs, seq, finalized, ..} => {
                let (new_unks, new_expand_deps) = prs.borrow().unknowns(vd)?;
                unks.extend(new_unks);
                expand_deps.extend(new_expand_deps);

                // Regardless of whether this structure is finalized, any members in seq are valid. add their
                // unknowns to the set.
                for member in seq {
                    let (new_unks, new_expand_deps) = member.borrow().unknowns(vd)?;
                    unks.extend(new_unks);
                    expand_deps.extend(new_expand_deps);
                } 

                if *finalized {
                    // If the structure is finalized, then the "seq" variable should already be fully populated.
                    // Nothing more is needed
                } else {
                    expand_deps.insert(self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract());
                }

            },
            PartiallyResolvedStructureType::Assembly{prs, ..} => {
                let (new_unks, new_expand_deps) = prs.borrow().unknowns(vd)?;
                unks.extend(new_unks);
                expand_deps.extend(new_expand_deps);
                
            },
        }
        
        Ok((unks, expand_deps))
    }
}


pub struct XmlObject<'a> {
    element: MyStrSpan<'a>,
    attrs: HashMap<&'a str, MyStrSpan<'a>>,
    children: Vec<XmlObject<'a>>
}

impl<'a> XmlObject<'a> {
    fn remove_required_attr(&mut self, attr: &str) -> Result<MyStrSpan, ParseXmlError> {
        match self.attrs.remove(attr) {
            None => {
                let err = ParseXmlError::missing_required_attribute(&self.element, attr.to_string());
                Err(err)
            },
            Some(value)=> Ok(value)
        }
    }
    
}

#[derive(Clone)]
enum DefField {
    Search{start: ExprWrapper, find: String, n: ExprWrapper},
    Anchor{offset: ExprWrapper},
    Compute(Expr)
}

impl DefField {
    fn from_xml_object(mut obj: XmlObject) -> Result<DefField, ParseXmlError> {
        match obj.element.as_str() {
            "search" => {
                let start = ExprWrapper::from_xml_str_span(&obj.remove_required_attr("start")?)?;
                let find = convert_xml_symbols(obj.attrs.remove("find").unwrap().as_str());
                let n = match obj.attrs.remove("n") {
                    Some(expr) => ExprWrapper::from_xml_str_span(&expr)?,
                    None => ExprWrapper::from_default(Expr::value(ExprValue::Integer(0)))
                };
                Ok(DefField::Search{start, find, n})
            },
            "anchor" => {
                let offset = match obj.attrs.remove("offset") {
                    Some(expr) => ExprWrapper::from_xml_str_span(&expr)?,
                    None => ExprWrapper::from_default(Expr::value(ExprValue::Position(BitIndex::zero())))
                };
                Ok(DefField::Anchor{offset})
            },
            d => panic!("Unrecognized def field: '{}'", d)
        }
    }
}

#[derive(Clone)]
struct Break {
    condition: ExprWrapper
}

#[derive(Clone)]
enum StructureType {
    Section(Rc<SectionSpec>),
    Segment,
    Addressed{position: Rc<ExprWrapper>, content: Rc<Structure>},
    Sequence(Vec<Rc<Structure>>),
    Switch{value: Rc<ExprWrapper>, cases: Vec<(Rc<ExprWrapper>, Rc<Structure>)>, default: Rc<Structure>},
    Repeat{n: Rc<ExprWrapper>, structure: Rc<Structure>},
    RepeatUntil{end: Rc<ExprWrapper>, structure: Rc<Structure>},
    Chain{next: Rc<ExprWrapper>, structure: Rc<Structure>},
    Assembly{segments: Vec<String>, structure: Rc<Structure>}
}

#[derive(Clone)]
pub struct Structure {
    id: String,
    name: String,
    alignment: ExprWrapper,
    alignment_base: ExprWrapper,
    length: LengthPolicy,
    exports: HashMap<String, Option<ExprWrapper>>,
    def_fields: HashMap<String, DefField>,
    breaks: Vec<Break>,
    stype: StructureType,
    filename: Option<Rc<String>>
}

impl Structure {

    /// Returns the first child (or self) that has an id that matches the string supplied if
    /// one exists. Returns None otherwise
    fn get_child_by_id(self: &Rc<Self>, id: &str) -> Option<Rc<Structure>> {
        if self.id == id {
            Some(self.clone())
        } else {
            match &self.stype {
                StructureType::Section(_) | StructureType::Segment => None,
                StructureType::Addressed{content, ..} => content.get_child_by_id(id),
                StructureType::Sequence(seq) => {
                    for s in seq {
                        let res = s.get_child_by_id(id);
                        if res.is_some() {
                            return res
                        }
                    }
                    None
                },
                StructureType::Switch{cases, default, ..} => {
                    for (_, s) in cases {
                        let res = s.get_child_by_id(id);
                        if res.is_some() {
                            return res
                        }
                    }
                    default.get_child_by_id(id)
                },
                StructureType::Repeat{structure, ..} | StructureType::RepeatUntil{structure, ..} | StructureType::Chain{structure, ..} | StructureType::Assembly{structure, ..} => {
                    structure.get_child_by_id(id)
                }
            }
        }
    }

    fn wrap_addressed(inner: Rc<Structure>, position: Expr) -> Structure {
        let id = inner.id.clone() + "#wrapper";
        let name = inner.name.clone();
        let alignment = ExprWrapper::from_default(Expr::value(ExprValue::Position(BitIndex::bits(1)))); // Smallest increment possible
        let alignment_base = ExprWrapper::from_default(Expr::value(ExprValue::Position(BitIndex::zero()))); // Doesn't matter
        let length = LengthPolicy::FitContents; // This won't work if inner's length is Expand, but that isn't really valid for chain children anyway
        let exports = inner.exports.clone(); // Need to have the same exports so they can tunnel through
        let def_fields = HashMap::new();
        let breaks = vec![];
        let stype = StructureType::Addressed{position: Rc::new(ExprWrapper::from_default(position)), content: inner};

        Structure {
            id,
            name,
            alignment,
            alignment_base,
            length,
            exports,
            def_fields,
            breaks,
            stype,
            filename: None
        }
    }

    fn from_xml_object(mut obj: XmlObject, fname: Option<Rc<String>>) -> Result<Structure, ParseXmlError> {
        if obj.element.as_str() == "use" {
            let url = convert_xml_symbols(obj.attrs.remove("url").unwrap().as_str());
            if let Some((fname, struct_id)) = url.split_once("#") {
                let text = std::fs::read_to_string(&fname).unwrap();
                let s = match Structure::from_xml(&text, Some(Rc::new(fname.to_string()))) {
                    Ok(s) => Rc::new(s),
                    Err(mut err) => {
                        err.set_fname(fname.to_string());
                        err.set_context(&text);
                        return Err(err)
                    }
                };
                if let Some(child) = s.get_child_by_id(struct_id) {
                    std::mem::drop(s);
                    let mut s = Rc::into_inner(child).unwrap();
                    // s.set_fname(fname.to_string());
                    return Ok(s)
                } else {
                    panic!("Structure does not contain id '{}'", struct_id)
                }
            } else {
                panic!("Unrecognized url format: '{}'", url)
            }
        }

        // Get data that's common to all structural objects

        let length = match obj.attrs.remove("length-policy") {
            None => match obj.attrs.remove("length") {
                None => LengthPolicy::FitContents,
                Some(s) => LengthPolicy::Expr(ExprWrapper::from_xml_str_span(&s)?)
            },
            Some(s) => match convert_xml_symbols(s.as_str()).as_str() {
                "expr" => match obj.attrs.remove("length") {
                    None => panic!("Length policy is expr but no length specified"),
                    Some(s) => LengthPolicy::Expr(ExprWrapper::from_xml_str_span(&s)?)
                },
                "expand" => LengthPolicy::Expand,
                "fit" => LengthPolicy::FitContents,
                _ => panic!("Unrecognized length policy")
            }
        };
        let id = match obj.attrs.remove("id") {
            Some(s) => convert_xml_symbols(s.as_str()),
            None => String::new()
        };
        let name = match obj.attrs.remove("name") {
            Some(s) => convert_xml_symbols(s.as_str()),
            None => String::new()
        };
        let alignment = match obj.attrs.remove("align") {
            Some(s) => ExprWrapper::from_xml_str_span(&s)?,
            None => ExprWrapper::from_default(Expr::value(ExprValue::Position(BitIndex::bytes(1))))
        };
        let alignment_base = match obj.attrs.remove("align-base") {
            Some(s) => ExprWrapper::from_xml_str_span(&s)?,
            None => ExprWrapper::from_default(Expr::value(ExprValue::Position(BitIndex::zero())))
        };

        // Go through children looking for expors, def_fields, and breaks which are common for must structural
        // objects

        let mut exports = HashMap::new();
        let mut def_fields = HashMap::new();
        let mut breaks = Vec::new();

        let mut children = Vec::new();

        for mut child in obj.children.drain(..) {
            match child.element.as_str() {
                "export" => {
                    let export_source = convert_xml_symbols(child.attrs.remove("source").unwrap().as_str());
                    let export_default = match obj.attrs.remove("default") {
                        None => None,
                        Some(expr)=> {
                            Some(ExprWrapper::from_xml_str_span(&expr)?)
                        }
                    };
                    exports.insert(export_source, export_default);
                    if !child.children.is_empty() {
                        panic!("'export' should not have children!")
                    }
                },
                "search" | "anchor" => {
                    let id = convert_xml_symbols(child.attrs.remove("id").unwrap().as_str());
                    def_fields.insert(id, DefField::from_xml_object(child)?);
                },
                "break" => {
                    // let condition = Expr::from_str(&convert_xml_symbols(child.attrs.remove("condition").unwrap().as_str()))?;
                    // let condition_ss = child.attrs.remove("condition").unwrap();
                    // let condition = Expr::from_str_span(MyStrSpan::new(condition_ss.start(), condition_ss.as_str()))?;
                    let condition = ExprWrapper::from_xml_str_span(&child.remove_required_attr("condition")?)?;
                    breaks.push(Break{condition});
                },
                _ => children.push(child)
            }
        }



        let stype: StructureType;

        match obj.element.as_str() {
            "section" | "section-body" | "section-header" | "section-footer" => {
                let section_type = match obj.element.as_str() {
                    "section" | "section-body" => SectionType::Body,
                    "section-header" => SectionType::Header,
                    "section-footer" => SectionType::Footer,
                    _ => unreachable!()
                };

                let mut fields = vec![];
                let mut id_map = HashMap::new();

                for mut child in children {
                    let field = FieldSpec::from_xml_object(child)?;
                    id_map.insert(field.id.clone(), fields.len());
                    fields.push(field);
                }

                stype = StructureType::Section(Rc::new(SectionSpec {
                    length: length.clone(),
                    section_type,
                    fields,
                    id_map
                }));


            },
            "segment" => {
                stype = StructureType::Segment;
            },
            "addressed" => {
                let pos = ExprWrapper::from_xml_str_span(&obj.remove_required_attr("pos")?)?;

                let mut structure = None;
                let mut structure_element_range = None; // Needed for error message
                for mut child in children {
                    if structure.is_none() {
                        structure_element_range = Some(child.element.range());
                        structure = Some(Structure::from_xml_object(child, fname.clone())?);
                    } else {
                        let message = format!("'{}' object has more than one structural child", obj.element.as_str()).to_string();
                        let mut err = ParseXmlError::new(message);
                        let message = format!("This '{}' object should only have one child, but at least two were detected", obj.element.as_str()).to_string();
                        err.push_annotation(obj.element.range(), Some(message));
                        err.push_annotation(structure_element_range.unwrap(), Some("First structural child is defined here".to_string()));
                        err.push_annotation(child.element.range(), Some("Second structural child is defined here".to_string()));
                        err.set_help("Try wrapping children in a 'sequence' object".to_string());
                        return Err(err)
                    }
                }
                if let Some(structure) = structure {
                    let structure = Rc::new(structure);
                    stype = StructureType::Addressed{position: Rc::new(pos), content: structure.clone()};
                } else {
                    panic!("'addressed' has no children!")
                }
            },
            "sequence" => {
                let mut seq = vec![];
                for mut child in children {
                    seq.push(Rc::new(Structure::from_xml_object(child, fname.clone())?));
                }
                stype = StructureType::Sequence(seq);
            },
            "switch" => {
                // TODO: Make it some that value is not required
                let value = ExprWrapper::from_xml_str_span(&obj.remove_required_attr("value")?)?;
                let mut cases = vec![];
                let mut default = None;
                for mut child in children {
                    match child.element.as_str() {
                        "switch-case" => {
                            let check = ExprWrapper::from_xml_str_span(&child.remove_required_attr("check")?)?;
                            if child.children.len() != 1 {
                                panic!("switch-case must have exactly one child")
                            }
                            let s = Structure::from_xml_object(child.children.into_iter().nth(0).unwrap(), fname.clone())?;
                            cases.push((Rc::new(check), Rc::new(s)));
                        },
                        "switch-else" => {
                            if default.is_some() {
                                panic!("More than one default specified")
                            }
                            if child.children.len() != 1 {
                                panic!("switch-else must have exactly one child")
                            }
                            let s = Structure::from_xml_object(child.children.into_iter().nth(0).unwrap(), fname.clone())?;
                            default = Some(Rc::new(s));
                        },
                        _ => todo!()
                    }
                }
                if let Some(default) = default {
                    stype = StructureType::Switch{value: Rc::new(value), cases, default}
                } else {
                    panic!("No default case specified for switch")
                }
            },
            "repeat" => {
                let mut structure = None;
                for mut child in children {
                    if structure.is_none() {
                        structure = Some(Structure::from_xml_object(child, fname.clone())?);
                    } else {
                        panic!("'repeat' should only have one non-export child!");
                    }
                    
                }

                if let Some(structure) = structure {
                    let structure = Rc::new(structure);
                    match (obj.attrs.remove("until"), obj.attrs.remove("n")) {
                        (None, None) => panic!("'until' or 'n' must be specified for 'repeat' element"),
                        (Some(expr), None) => {
                            let end = ExprWrapper::from_xml_str_span(&expr)?;
                            stype = StructureType::RepeatUntil{end: Rc::new(end), structure};
                        },
                        (None, Some(expr)) => {
                            let n = ExprWrapper::from_xml_str_span(&expr)?;
                            stype = StructureType::Repeat{n: Rc::new(n), structure};
                        },
                        (Some(_), Some(_)) => panic!("Both 'until' and 'n' specified for 'repeat' element. One must be removed.")
                    }
                } else {
                    panic!("'repeat' has no children!")
                }
            },
            "chain" => {
                let next = ExprWrapper::from_xml_str_span(&obj.remove_required_attr("next")?)?;

                let mut structure = None;
                for mut child in children {
                    if structure.is_none() {
                        structure = Some(Structure::from_xml_object(child, fname.clone())?);
                    } else {
                        panic!("'chain' should only have one non-export child!");
                    }
                    
                }
                if let Some(structure) = structure {
                    let structure = Rc::new(structure);
                    stype = StructureType::Chain{next: Rc::new(next), structure};
                } else {
                    panic!("'chain' has no children!")
                }
            },
            "assembly" => {
                let segments = obj.remove_required_attr("segments")?.as_str().split(',').map(|s| s.trim().to_string()).collect::<Vec<String>>();

                let mut structure = None;
                for mut child in children {
                    if structure.is_none() {
                        structure = Some(Structure::from_xml_object(child, fname.clone())?);
                    } else {
                        panic!("'assembly' should only have one non-export child!");
                    }
                    
                }

                if let Some(structure) = structure {
                    let structure = Rc::new(structure);
                    stype = StructureType::Assembly{segments, structure};
                } else {
                    panic!("'assembly' has no children!")
                }
            }
            _ => todo!()
        }

        Ok(
            Structure {
                id,
                name,
                alignment,
                alignment_base,
                length,
                exports,
                def_fields,
                breaks,
                stype,
                filename: fname
            }
        )
    }

    pub fn from_xml(input: &str, fname: Option<Rc<String>>) -> Result<Structure, ParseXmlError> {
        let mut objects = Vec::<XmlObject>::new();
        let mut node_stack = Vec::<(XmlObject, MyStrSpan)>::new();
        let mut attr_map = HashMap::<&str, MyStrSpan>::new();
        //let mut members_stack = Vec::<Vec<SvgElement>>::new();
        let mut current_elem: Option<MyStrSpan> = None;
        let mut current_text = Vec::<&str>::new();
    
        //members_stack.push(vec![]);
        for token in Tokenizer::from(input) {
            match token {
                Ok(Token::Declaration{version, encoding, standalone, ..}) => {
                    // svg.xml_version = Some(version.as_str());
                    // if let Some(sspan) = encoding {
                    //     svg.xml_encoding = Some(sspan.as_str());
                    // }
                    // if let Some(b) = standalone {
                    //     svg.xml_standalone = Some(b);
                    // }
                },
                Ok(Token::ElementStart{local, ..}) => {
                    info!("Element Start: {}", local);
                    current_elem = Some(MyStrSpan::new(local.start(), local.as_str()));
                },
                Ok(Token::Attribute{local, value, ..}) => {
                    info!("\t{} = {}", local, value);
                    attr_map.insert(local.as_str(), MyStrSpan::new(value.start(), value.as_str()));
                },
                Ok(Token::ElementEnd{end, ..}) => {
                    info!("Element End: {:?}", end);
                    
    
                    match end {
                        ElementEnd::Open => {
                            // let elem = XplNode::from_xml_dtypes(&current_elem.unwrap(), &mut attr_map/*, &self.parse_errors*/)?;
                            // svg.register_element(elem.clone())?;
                            // if !attr_map.is_empty() {
                            //     let unrecognized_key = attr_map.keys().next().unwrap();
                            //     let message = format!("Unrecognized attribute '{}' supplied for element '{}'", unrecognized_key, &current_elem.unwrap());
                            //     //self.unrecognized_attributes.handle_error(ErrorAcceptanceLevel::Strict, message.as_str())?;
                            //     println!("{}", message);
                            //     attr_map.clear();
                            // }
                            //println!("NEW ELEMENT: {:?}", elem);
    
                            let obj = XmlObject{element: current_elem.clone().unwrap(), attrs: attr_map, children: vec![]};
                            attr_map = HashMap::new();
    
                            node_stack.push((obj, current_elem.unwrap()));
                            current_elem = None;
                        },
                        ElementEnd::Close(sspan1, sspan2) => {
                            match node_stack.pop() {
                                Some((elem, name)) if sspan2.as_str() != name.as_str() => {
                                    // let (line_index, col_index) = get_line_char(input, sspan2.start());
                                    let message = format!("Element end '{}' does not match start '{}'", sspan2.as_str(), name.as_str());
                                    let mut err = ParseXmlError::new(message.to_string());
                                    err.push_annotation(name.range(), Some("Element start is here".to_string()));
                                    err.push_annotation(sspan2.range(), Some("Element end is here".to_string()));
                                    return Err(err)
                                },
                                Some((obj, _)) => {
                                    let n = node_stack.len();
                                    if n == 0 {
                                        //svg.members.push(elem);
                                        // svg.attempt_add_child(elem);
                                        objects.push(obj);
                                    } else {
                                        //node_stack[n - 1].0.members.push(elem);
                                        node_stack[n - 1].0.children.push(obj);
                                    }
                                },
                                None => {
                                    let (line_index, col_index) = get_line_char(input, sspan2.start());
                                    let message = format!("Line {} column {}: Unexpected element end encountered: '{}'", 
                                                            line_index + 1, col_index + 1, sspan2.as_str());
                                    // self.malformed_xml.handle_error(ErrorAcceptanceLevel::Compliant, message.as_str())?;
                                    panic!("{}", message);
                                },
                                _ => {}
                            }
                        },
                        ElementEnd::Empty => {
                            // let elem = XplNode::from_xml_dtypes(&current_elem.unwrap(), &mut attr_map/*, &self.parse_errors*/)?;
                            // svg.register_element(elem.clone())?;
                            // if !attr_map.is_empty() {
                            //     let unrecognized_key = attr_map.keys().next().unwrap();
                            //     let message = format!("Unrecognized attribute '{}' supplied for element '{}'", unrecognized_key, &current_elem.unwrap());
                            //     //self.unrecognized_attributes.handle_error(ErrorAcceptanceLevel::Strict, message.as_str())?;
                            //     panic!("{}", message);
                            //     attr_map.clear();
                            // }
                            let obj = XmlObject{element: current_elem.unwrap(), attrs: attr_map, children: vec![]};
                            attr_map = HashMap::new();
                            //println!("NEW ELEMENT: {:?}", elem);
                            let n = node_stack.len();
                            if n == 0 {
                                //svg.members.push(elem);
                                // svg.attempt_add_child(elem);
                                objects.push(obj);
                            } else {
                                //node_stack[n - 1].0.members.push(elem);
                                // match node_stack[n - 1].0.attempt_add_child(elem) {
                                //     Err(err) => return Err(InvalidChildError::new(err.details, node_stack[n - 1].1.clone(), current_elem.unwrap().clone()).into()),
                                //     _ => {}
                                // }
                                node_stack[n - 1].0.children.push(obj);
                            }
                            //let n = members_stack.len();
                            //members_stack[n - 1].push(elem);
                            current_elem = None;
                        }
    
                    }
                },
                Ok(Token::Text{text}) => {
                    info!("Text: {}", text);
                    current_text.push(text.as_str());
                    let n = node_stack.len();
                    // if n == 0 {
                    //     svg.members.push(SvgNode::from_free_text(text.as_str()));
                    // } else {
                    //     node_stack[n - 1].0.members.push(SvgNode::from_free_text(text.as_str()));
                    // }
                },
                Ok(Token::Comment{text, ..}) => {
                    info!("Comment: {}", text);
                },
                Err(err) => {
                    match err {
                        xmlparser::Error::InvalidAttribute(stream_err, pos) => {
                            match stream_err {
                                xmlparser::StreamError::InvalidSpace(c, pos) => {
                                    let i = get_index_from_row_col(input, pos.row as usize, pos.col as usize);
                                    let message = format!("Encountered unexpected character: '{}' (expected space)", c as char).to_string();
                                    let mut err = ParseXmlError::new(message);
                                    err.push_annotation(i..i+1, None);
                                    return Err(err)
                                },
                                xmlparser::StreamError::InvalidChar(c1, c2, pos) => {
                                    let i = get_index_from_row_col(input, pos.row as usize, pos.col as usize);
                                    let message = format!("Encountered unexpected character: '{}' (expected '{}')", c1 as char, c2 as char).to_string();
                                    let mut err = ParseXmlError::new(message);
                                    err.push_annotation(i..i+1, None);
                                    return Err(err)
                                },
                                _ => todo!("{:?}", stream_err)
                            }
                        },
                        _ => todo!("{:?}", err)
                    }
                }
                _ => panic!("Unrecognized Token: {:?}", token)
            }
        }
        if !node_stack.is_empty() {
            //self.malformed_xml.handle_error(ErrorAcceptanceLevel::Compliant, "Encountered EOF with unclosed elements")?;
            // let mut index_vec = vec![];
            // for (_, sspan) in node_stack {
            //     index_vec.push((sspan.start(), sspan.end(), Some(format!("'{}' element opened here and never closed:", sspan.as_str()).to_string())));
            // }
            // return Err(ParseXplError {
            //     details: format!("Encountered end of file with unclosed elements:").to_string(),
            //     error_type: ParseXplErrorType::MalformedXml,
            //     index: index_vec,
            //     help_text: None
            // })
            let message = format!("Encountered end of file with unclosed elements:").to_string();
            let mut err = ParseXmlError::new(message);
            for (_, sspan) in node_stack {
                err.push_annotation(sspan.range(), Some(format!("'{}' element opened here and never closed:", sspan.as_str()).to_string()));
            }
            return Err(err)
            // panic!("Encountered EOF with unclosed elements");
        }
    
        if objects.len() == 1 {
            Structure::from_xml_object(objects.into_iter().nth(0).unwrap(), fname.clone())
        } else {
            panic!("Wrong number of objects in XML!")
        }
        // Ok(svg)
    }
}

use std::cell::RefCell;

// Node of a Directed Acyclic Graph representy dependency relationships between fields
#[derive(Eq, PartialEq)]
struct DepNode {
    name: AbstractIdent,
    known: bool,
    parents: Vec<Rc<RefCell<DepNode>>>, // Dependancies
    children: Vec<Rc<RefCell<DepNode>>> // Dependants
}

impl DepNode {
    fn new(id: AbstractIdent) -> DepNode {
        DepNode {
            name: id,
            known: false,
            parents: Vec::new(),
            children: Vec::new()
        }
    }

}

impl std::fmt::Debug for DepNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let parents = self.parents.iter().map(|p| p.as_ref().borrow().name.clone()).collect::<Vec<AbstractIdent>>();
        let children = self.children.iter().map(|p| p.as_ref().borrow().name.clone()).collect::<Vec<AbstractIdent>>();
        write!(f, "DepNode(name=\"{}\" parents={:?} children={:?})", self.name, parents, children)
    }
}

#[derive(Default)]
struct ValueDictionary {
    struct_attr_values: HashMap<StructureAttrIdent, ExprValue>,
    field_values: HashMap<FieldIdent, ExprValue>,
    field_attr_values: HashMap<FieldAttrIdent, ExprValue>,
}

impl ValueDictionary {
    fn new() -> ValueDictionary {
        ValueDictionary {
            struct_attr_values: HashMap::new(),
            field_values: HashMap::new(),
            field_attr_values: HashMap::new()
        }
    }

    fn lookup_struct_attr(&self, id: &StructureAttrIdent) -> Option<ExprValue> {
        match self.struct_attr_values.get(id) {
            Some(v) => Some(v.clone()),
            None => None
        }
    }

    fn lookup_field(&self, id: &FieldIdent) -> Option<ExprValue> {
        match self.field_values.get(id) {
            Some(v) => Some(v.clone()),
            None => None
        }
    }

    fn lookup_field_attr(&self, id: &FieldAttrIdent) -> Option<ExprValue> {
        match self.field_attr_values.get(id) {
            Some(v) => Some(v.clone()),
            None => None
        }
    }

    fn lookup_any(&self, id: &AbstractIdent) -> Option<ExprValue> {
        match id {
            AbstractIdent::StructureAttr(id) => self.lookup_struct_attr(id),
            AbstractIdent::Field(id) => self.lookup_field(id),
            AbstractIdent::FieldAttr(id) => self.lookup_field_attr(id)
        }
    }

    fn lookup_str(&self, key: &str) -> Result<Option<ExprValue>, ParseAbstractIdentError> {
        let id = AbstractIdent::from_str(key)?;
        Ok(self.lookup_any(&id))
    }

    fn contains_key(&self, id: &AbstractIdent) -> bool {
        match id {
            AbstractIdent::StructureAttr(id) => self.struct_attr_values.contains_key(id),
            AbstractIdent::Field(id) => self.field_values.contains_key(id),
            AbstractIdent::FieldAttr(id) => self.field_attr_values.contains_key(id)
        }
    }

    fn insert_struct_attr(&mut self, id: StructureAttrIdent, value: ExprValue) {
        self.struct_attr_values.insert(id, value);
    }

    fn insert_field(&mut self, id: FieldIdent, value: ExprValue) {
        self.field_values.insert(id, value);
    }

    fn insert_field_attr(&mut self, id: FieldAttrIdent, value: ExprValue) {
        self.field_attr_values.insert(id, value);
    }

    fn insert_any(&mut self, id: AbstractIdent, value: ExprValue) -> Option<ExprValue> {
        match id {
            AbstractIdent::StructureAttr(id) => self.struct_attr_values.insert(id, value),
            AbstractIdent::Field(id) => self.field_values.insert(id, value),
            AbstractIdent::FieldAttr(id) => self.field_attr_values.insert(id, value)
        }
    }

    fn len(&self) -> usize {
        self.struct_attr_values.len() + self.field_values.len() + self.field_attr_values.len()
    }
}

#[derive(Default)]
struct DepGraph {
    dep_roots: HashSet<AbstractIdent>,
    dep_map: HashMap<AbstractIdent, Rc<RefCell<DepNode>>>
}

impl DepGraph {
    fn new() -> DepGraph {
        DepGraph {
            dep_roots: HashSet::new(),
            dep_map: HashMap::new()
        }
    }

    fn reset(&mut self) {
        self.dep_roots.clear();
        self.dep_map.clear();
    }

    fn mark_known(&mut self, id: &AbstractIdent) {
        match self.dep_map.get(id) {
            Some(node) => node.borrow_mut().known = true,
            None => panic!()
        }
    }

    fn get_resolvable_unknowns(&self, vd: &ValueDictionary) -> Vec<AbstractIdent> {
        let mut result = Vec::new();
        for (s, node) in self.dep_map.iter() {
            // if vd.contains_key(s) != node.borrow().known {
            //     panic!("s: {}, {}, node: {}, {}", s, vd.contains_key(s), node.borrow().name, node.borrow().known)
            // }
            if !node.borrow().known {
                let mut resolvable = true;
                for parent in &node.borrow().parents {
                    if !parent.borrow().known {
                        resolvable = false;
                        break;
                    }
                }
                if resolvable {
                    result.push(s.clone());
                }
            }
        }
        result
    }

    // Inserts an unknown ident into the graph assuming that it has no dependencies. If the ident
    // is already in the graph, does nothing and returns false. Otherwise, returns true.
    fn insert(&mut self, id: AbstractIdent) -> bool {
        if self.dep_map.contains_key(&id) {
            false
        } else {
            let c = Rc::new(RefCell::new(DepNode::new(id.clone())));
            self.dep_map.insert(id.clone(), c.clone());
            self.dep_roots.insert(id);
            true
        }
    }

    fn add_dependancies(&mut self, id: AbstractIdent, dependencies: Vec<AbstractIdent>) {
        let child = match self.dep_map.get(&id) {
            Some(c) => {
                if !dependencies.is_empty() {
                    // If we're adding a dependency, then this will no longer be a root
                    // node (assuming it is one in the first place). Remove it from dep_roots
                    // just in case
                    self.dep_roots.remove(&id);
                }
                c.clone()
            },
            None => {
                let c = Rc::new(RefCell::new(DepNode::new(id.clone())));
                self.dep_map.insert(id.clone(), c.clone());
                if dependencies.is_empty() {
                    // If for some reason this is getting added without any dependencies,
                    // then make sure it's added to dep_roots since it would be a root node
                    // (without any children, yet)
                    self.dep_roots.insert(id);
                }
                c
            }
        };

        for parent_id in dependencies {
            match self.dep_map.get(&parent_id) {
                Some(parent) => {
                    if !child.borrow().parents.contains(&parent) {
                        child.borrow_mut().parents.push(parent.clone());
                    }
                    if !parent.borrow().children.contains(&child) {
                        parent.borrow_mut().children.push(child.clone());
                    }
                    
                },
                None => {
                    let parent = Rc::new(RefCell::new(DepNode::new(parent_id.clone())));
                    self.dep_roots.insert(parent_id.clone());
                    self.dep_map.insert(parent_id, parent.clone());
                    if !child.borrow().parents.contains(&parent) {
                        child.borrow_mut().parents.push(parent.clone());
                    }
                    if !parent.borrow().children.contains(&child) {
                        parent.borrow_mut().children.push(child.clone());
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum FileRegion {
    Field(FieldIdent),
    FieldPad(FieldIdent),
    StructurePad(StructureIdent),
    Segment(StructureIdent, std::ops::Range<BitIndex>),
    Spare(StructureIdent, std::ops::Range<BitIndex>)
}

pub struct FileMap<'a> {
    dep_graph: DepGraph,
    value_dict: ValueDictionary,
    prs: Rc<RefCell<PartiallyResolvedStructure<'a>>>,
    prs_map: PrsMap<'a>
}

impl<'a> FileMap<'a> {

    pub fn new(structure: Rc<Structure>, fm: &mut crate::FileManager) -> FileMap<'a> {
        let file_struct = StructureIdent::new("file".to_string());
        let mut prs_map = HashMap::new();
        let prs = PartiallyResolvedStructure::new(structure.clone(), file_struct.clone(), &mut prs_map);

        let eof = ExprValue::Position(BitIndex::bytes(fm.len()));
        let mut value_dict = ValueDictionary::new();
        let mut dep_graph = DepGraph::new();
        let eof_id = file_struct.clone().get_field_ident("EOF".to_string());
        value_dict.insert_field(eof_id.clone(), eof);
        dep_graph.add_dependancies(eof_id.clone().to_abstract(), vec![]);
        dep_graph.mark_known(&eof_id.clone().to_abstract());

        let mut initial_monikers = HashMap::new();
        initial_monikers.insert("EOF".to_string(), eof_id);
        prs.borrow_mut().initialize_inherited_monikers(&prs_map, initial_monikers);

        FileMap {
            dep_graph,
            value_dict,
            prs,
            prs_map
        }
    }

    pub fn region_at(&mut self, index: BitIndex, fm: &mut crate::FileManager) -> Result<FileRegion, FileSpecError> {
        let temp_result: FileRegion;
        loop {
            let vd = std::mem::take(&mut self.value_dict);
            let dr = (*self.prs).borrow().try_get_region_at(&vd, index).unwrap();
            self.value_dict = vd;
            match dr.result {
                DepResult::Success(fr) => {
                    match fr {
                        FileRegion::Spare{..} => {
                            temp_result = fr;
                            break;
                        },
                        _ => return Ok(fr)
                    }
                },
                DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                    self.get_data(deps.into_iter().collect(), fm)?;
                },
                DepResult::DoesNotExist => panic!("Field does not exist!"),
            }
        }

        // If the region seems to be spare, loop through the addressed structures to see if there
        // are any hiding there. TODO: Make this more efficient.
        let mut targets = vec![];
        loop {
            for prs in self.prs_map.values() {
                if let PartiallyResolvedStructureType::Addressed{pss, ..} = prs.borrow().stype.as_ref() {
                    info!("Looking for region in {:?}", pss.borrow().id);
                    let vd = std::mem::take(&mut self.value_dict);
                    let dr = pss.borrow().try_get_region_at(&vd, index).unwrap();
                    self.value_dict = vd;
                    match dr.result {
                        DepResult::Success(fr) => {
                            return Ok(fr)
                        },
                        DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                            targets.extend(deps);
                            // self.get_data(deps.into_iter().collect(), fm);
                        },
                        DepResult::DoesNotExist => continue
                    }
                }
            }
            if targets.is_empty() {
                return Ok(temp_result)
            }
            self.get_data(targets, fm)?;
            targets = vec![];
        }
    }

    /// Returns a list of fields that are related to the given field. Related fields include
    /// those used to derive ancestral switch values, addressed positions, and repetitions
    pub fn related_fields(&self, field: FieldIdent) -> Vec<FieldIdent> {
        info!("Getting related fields for {}", field);
        let mut related = Vec::new();
        let mut structure = self.prs_map.get(&field.structure).unwrap();
        loop {
            match structure.borrow().stype.as_ref() {
                PartiallyResolvedStructureType::Switch{value, ..} => {
                    for s in value.vars() {
                        match structure.borrow().get_source_field(s, &self.prs_map) {
                            Ok(Some(fid)) => related.push(fid),
                            Ok(None) => info!("{} must be a derived field", field),
                            Err(err) => error!("Error while finding related fields: {}", err)
                        }
                    }
                },
                PartiallyResolvedStructureType::Repeat{n, ..} => {
                    for s in n.vars() {
                        match structure.borrow().get_source_field(s, &self.prs_map) {
                            Ok(Some(fid)) => related.push(fid),
                            Ok(None) => info!("{} must be a derived field", field),
                            Err(err) => error!("Error while finding related fields: {}", err)
                        }
                    }
                },
                PartiallyResolvedStructureType::RepeatUntil{end, ..} => {
                    for s in end.vars() {
                        match structure.borrow().get_source_field(s, &self.prs_map) {
                            Ok(Some(fid)) => related.push(fid),
                            Ok(None) => info!("{} must be a derived field", field),
                            Err(err) => error!("Error while finding related fields: {}", err)
                        }
                    }
                },
                PartiallyResolvedStructureType::Addressed{position, ..} => {
                    info!("Addressed parent");
                    for s in position.vars() {
                        info!("Position var: {}", s);
                        match structure.borrow().get_source_field(s, &self.prs_map) {
                            Ok(Some(fid)) => related.push(fid),
                            Ok(None) => info!("{} must be a derived field", field),
                            Err(err) => error!("Error while finding related fields: {}", err)
                        }
                    }

                    // Addressed is a bit tricky because if it was created as part of a chain
                    // sequence, it'll be hard-coded to a certain value. In order to figure out
                    // what fields were actually used to derive it, we have to go back to the
                    // chain to find the previous iteration and use that as a context to find
                    // the fields used in `next`
                    let parent = match self.prs_map.get(&structure.borrow().parent_id) {
                        Some(parent) => parent,
                        None => break
                    };
                    if let PartiallyResolvedStructureType::Chain{next, prs, seq, ..} = parent.borrow().stype.as_ref() {
                        if prs.borrow().id == structure.borrow().id {
                            structure = parent;
                        } else if let Some(i) = seq.iter().position(|s| s.borrow().id == structure.borrow().id) {
                            let context = if i == 0 {
                                &prs
                            } else {
                                &seq[i - 1]
                            };
                            for s in next.vars() {
                                match context.borrow().get_source_field(s, &self.prs_map) {
                                    Ok(Some(fid)) => related.push(fid),
                                    Ok(None) => info!("{} must be a derived field", field),
                                    Err(err) => error!("Error while finding related fields: {}", err)
                                }
                            }
                        } else {
                            error!("Could not find {} in parent chain", structure.borrow().id);
                        }
                    }
                },
                _ => {
                    
                }
            }
            structure = match self.prs_map.get(&structure.borrow().parent_id) {
                Some(parent) => parent,
                None => break
            };
            info!("Parent: {}", structure.borrow().id);
        }
        // let dn = self.dep_graph.dep_map.get(&field.to_abstract()).unwrap();
        // let mut related = Vec::new();
        // let mut parents = dn.borrow().parents.clone();
        // let mut new_parents = Vec::new();
        // for i in 0..20 {
        //     info!("{}: {:?}", i, parents);
        //     for parent in parents {
        //         match &parent.borrow().name {
        //             AbstractIdent::Field(fi) => related.push(fi.clone()),
        //             _ => new_parents.extend(parent.borrow().parents.clone())
        //         }
        //     }
        //     parents = new_parents;
        //     new_parents = Vec::new();
        // }
        related
    }

    pub fn get_field_valid(&mut self, field: FieldIdent, fm: &mut crate::FileManager) -> Result<bool, FileSpecError> {
        let fai = field.clone().get_attr_ident(FieldAttrType::Valid);
        self.get_data(vec![fai.clone().to_abstract()], fm)?;

        if let Some(ev) = self.value_dict.lookup_field_attr(&fai) {
            return Ok(ev.expect_bool()?)
        } else {
            error!("Field attribute didn't get populated by get_data call: {}", fai);
            return Err(FileSpecError::ident_not_found(fai.to_abstract()))
        }
    }

    pub fn get_field(&mut self, field: FieldIdent, fm: &mut crate::FileManager) -> Result<UnparsedBinaryField, FileSpecError> {
        let child = self.prs_map.get(&field.structure).unwrap().clone();

        loop {

            let dr = child.borrow_mut().try_get_field(field.clone(), &self.value_dict, &self.prs_map).unwrap();
            match dr.result {
                DepResult::Success(ds) => {
                    match ds {
                        DataSource::Field(f) => {
                            // self.value_dict.insert_field(fi.clone(), v);
                            // let v = f.parse(fm)?;
                            for pc in dr.parents_children.into_iter() {
                                let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                for c in pc.1 {
                                    info!("Adding dependencies for {}: {:?}", c, deps);
                                    self.dep_graph.add_dependancies(c, deps.clone());
                                }
                            }
                            return Ok(f)
                        },
                        _ => {
                            error!("{:?} is not a field", field);
                            return Err(FileSpecError::ident_not_valid(field.to_abstract())) // TODO: This isn't really representative of the issue...
                        }
                    }
                },
                DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                    let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                    // std::mem::drop(dr);
                    self.get_data(deps.into_iter().collect(), fm)?;
                },
                DepResult::DoesNotExist => {
                    error!("Field does not exist: {:?}", field);
                    return Err(FileSpecError::ident_not_found(field.to_abstract()))
                }
            }
        }
    }

    pub fn format_field_data(&self, field: FieldIdent, value: ExprValue) -> Result<String, FileSpecError> {
        let s = self.prs_map[&field.structure].clone();
        let s_borrow = s.borrow();
        match s_borrow.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                return Ok(initial.format_field_data(field.id.clone(), value))
                

            },
            _ => {
                error!("Cannot get a field for a non-section structure!");
                return Err(FileSpecError::ident_not_valid(field.to_abstract()))
            }
        }
    }

    pub fn structure_name(&self, structure: StructureIdent) -> String {
        let s = self.prs_map.get(&structure).unwrap();
        let mut name = s.borrow().original.name.clone();
        if !structure.index.is_empty() {
            name.push_str(" ");
            let mut first = true;
            for i in structure.index {
                if first {
                    name.push_str(format!("{}", i).as_str());
                    first = false;
                } else {
                    name.push_str(format!(".{}", i).as_str());
                }
            }
        }
        name
        
    }

    pub fn get_assembly(&self, ident: StructureIdent) -> Result<StructureIdent, FileSpecError> {
        let structure = self.prs_map.get(&ident).unwrap().clone();
        let result = structure.borrow().get_assembly(&self.prs_map);
        result
    }

    pub fn assemble(&mut self, fm: &mut crate::FileManager, ident: StructureIdent) -> Result<Vec<u8>, FileSpecError> {

        let structure = self.prs_map.get(&ident).unwrap().clone();
        loop {
            let dr = structure.borrow().try_get_data(fm, &self.value_dict, &self.prs_map)?;
            match dr.result {
                DepResult::Success(bf) => {
                    // TODO: Get rid of this unwrap
                    return Ok(bf.into_boxed_slice().unwrap().into_vec())
                },
                DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                    let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                    self.get_data(deps.into_iter().collect(), fm)?;
                },
                DepResult::DoesNotExist => {
                    // error!("Field does not exist: {:?}", field);
                    // return Err(FileSpecError::ident_not_found(field.to_abstract()))
                    todo!()
                }
            }
        }
    }

    fn get_data(&mut self, targets: Vec<AbstractIdent>, fm: &mut crate::FileManager) -> Result<(), FileSpecError> {
        let mut dg = std::mem::take(&mut self.dep_graph);
        let mut vd = std::mem::take(&mut self.value_dict);
        let mut prs_map = std::mem::take(&mut self.prs_map);

        let mut targets = targets;

        let file_struct = StructureIdent::new("file".to_string());


        let mut field_timer = std::time::Duration::new(0, 0);
        let mut field_attr_timer = std::time::Duration::new(0, 0);
        let mut struct_attr_timer = std::time::Duration::new(0, 0);
        let mut unknowns_timer = std::time::Duration::new(0, 0);
        let mut field_count = 0;
        let mut field_attr_count = 0;
        let mut struct_attr_count = 0;


        for target in targets.into_iter() {
            dg.insert(target);
        }

        targets = dg.get_resolvable_unknowns(&vd);

        // println!("Targets: {:?}", targets);
        loop {

            for target in targets {

                let now = std::time::Instant::now();

                info!("");
                info!("Lookup up {}", target);
                match target {
                    AbstractIdent::Field(ref fi) => {
                        info!("Target is a Field");

                        let child = prs_map.get(&fi.structure).unwrap();

                        // let field_id = struct_id.get_field_ident(fi.id);
                        let dr = child.borrow_mut().try_get_field(fi.clone(), &vd, &prs_map)?;
                        match dr.result {
                            DepResult::Success(ds) => {
                                match ds {
                                    DataSource::Field(f) => {
                                        let v = f.parse(fm)?;
                                        info!("Field parsed! Value is {:?}", v);
                                        //dg.lookup_values.insert(target.clone(), v);
                                        vd.insert_field(fi.clone(), v);
                                        for pc in dr.parents_children.into_iter() {
                                            let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                            for c in pc.1 {
                                                info!("Adding dependencies for {}: {:?}", c, deps);
                                                dg.add_dependancies(c, deps.clone());
                                            }
                                        }
                                        dg.mark_known(&target);
                                    },
                                    DataSource::Search{start, n, find} => {
                                        let bre = BinRegex::new(&find).unwrap();
                                        let span = BitIndex::bytes(100); // TODO: This only searches the next 100 bytes at the moment...
                                        let end = start + span;
                                        let mut buffer = vec![0; end.ceil().byte() - start.byte()];
                                        fm.get_bytes(start.byte(), &mut buffer)?;
                                        let mut bf = BitField::from_vec(buffer) << start.bit() as usize;
                                        bf.truncate(&span);
                                        if let Some(bm) = bre.find_iter(&bf).nth(n) {

                                            let v = bm.start() + start;
                                            info!("Search finished! {:?}", v);
                                            vd.insert_field(fi.clone(), ExprValue::Position(v));
                                            for pc in dr.parents_children.into_iter() {
                                                // TODO: Somehow represent dependency on the searched bytes
                                                let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                                for c in pc.1 {
                                                    info!("Adding dependencies for {}: {:?}", c, deps);
                                                    dg.add_dependancies(c, deps.clone());
                                                }
                                            }
                                            dg.mark_known(&target);
                                        } else {
                                            panic!("Pattern not found in searched bytes")
                                        }
                                    },
                                    DataSource::Given(v) => {
                                        vd.insert_field(fi.clone(), v);
                                        for pc in dr.parents_children.into_iter() {
                                            let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                            for c in pc.1 {
                                                info!("Adding dependencies for {}: {:?}", c, deps);
                                                dg.add_dependancies(c, deps.clone());
                                            }
                                        }
                                        dg.mark_known(&target);
                                    }
                                }
                                

                            },
                            DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                                info!("Lookup incomplete");
                                let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                                info!("Adding dependencies for {}: {:?}", target, deps);
                                dg.add_dependancies(target.clone(), deps.into_iter().collect());
                            },
                            DepResult::DoesNotExist => {
                                error!("Field does not exist: {:?}", target);
                                return Err(FileSpecError::ident_not_found(target))
                            }
                        }

                        #[cfg(test)] {
                            field_timer += now.elapsed();
                            field_count += 1;
                        }

                        continue;

                    },
                    AbstractIdent::FieldAttr(ref fai) => {
                        info!("Target is a Field Attribute");

                        let child = prs_map.get(&fai.field.structure).unwrap().clone();

                        let dr = child.borrow_mut().try_lookup(target.clone(), &vd, &mut prs_map).unwrap();
                        match dr.result {
                            DepResult::Success(v) => {
                                //dg.lookup_values.insert(target.clone(), v);
                                vd.insert_field_attr(fai.clone(), v);
                                info!("Adding value for {}", target);
                                for pc in dr.parents_children.into_iter() {
                                    let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                    for c in pc.1 {
                                        info!("Adding dependencies for {}: {:?}", c, deps);
                                        dg.add_dependancies(c, deps.clone());
                                    }
                                }
                                dg.mark_known(&target);
                            },
                            DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                                info!("Lookup incomplete");
                                let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                                info!("Adding dependencies for {}: {:?}", target, deps);
                                dg.add_dependancies(target, deps.into_iter().collect());
                            },
                            DepResult::DoesNotExist => {
                                error!("Field Attribute does not exist: {:?}", target);
                                return Err(FileSpecError::ident_not_found(target))
                                
                            }
                        }

                        #[cfg(test)] {
                            field_attr_timer += now.elapsed();
                            field_attr_count += 1;
                        }

                        continue;
                    }
                    AbstractIdent::StructureAttr(ref sai) => {

                        info!("Targt is a Structure Attribute");
                        //if let (Some(structure_match), Some(attr_match)) = (caps.name("structure"), caps.name("attribute")) {

                        if sai.structure == file_struct {
                            match sai.attr {
                                StructureAttrType::End | StructureAttrType::SpareLength | StructureAttrType::Length => {
                                    vd.insert_struct_attr(sai.clone(), ExprValue::Position(BitIndex::bytes(fm.len())));
                                    dg.mark_known(&target);
                                },
                                StructureAttrType::Start => {
                                    vd.insert_struct_attr(sai.clone(), ExprValue::Position(BitIndex::zero()));
                                    dg.mark_known(&target);
                                },
                                _ => {
                                    error!("Invalid file property: {}", sai.attr);
                                    return Err(FileSpecError::ident_not_valid(target))
                                }
                            }
                            
                        } else {
                            //let struct_id = StructureIdent::new(structure_match.as_str().to_string());
                            let mut child = prs_map.get(&sai.structure).unwrap().clone();

                            loop {
                                let dr = child.borrow_mut().try_lookup(target.clone(), &vd, &mut prs_map)?;
                                match dr.result {
                                    DepResult::Success(v) => {
                                        vd.insert_struct_attr(sai.clone(), v);
                                        info!("Adding value for {}", target);
                                        for pc in dr.parents_children.into_iter() {
                                            let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                            for c in pc.1 {
                                                info!("Adding dependencies for {}: {:?}", c, deps);
                                                dg.add_dependancies(c, deps.clone());
                                            }
                                        }
                                        dg.mark_known(&target);
                                        break;
                                    },
                                    DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                                        info!("Lookup incomplete");
                                        let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                                        info!("Adding dependencies for {}: {:?}", target, deps);
                                        dg.add_dependancies(target, deps.into_iter().collect());
                                        break;
                                    },
                                    DepResult::DoesNotExist => {
                                        info!("Field does not exist. Searching in parent");
                                        let new_child_id = child.borrow().parent_id.clone();
                                        if new_child_id == file_struct {
                                            if *sai == self.prs.borrow().id.get_attr_ident(StructureAttrType::Position) {
                                                //dg.lookup_values.insert(target, ExprValue::Position(BitIndex::zero()));
                                                vd.insert_struct_attr(sai.clone(), ExprValue::Position(BitIndex::zero()));
                                                dg.mark_known(&target);
                                            } else if matches!(self.prs.borrow().length, LengthPolicy::Expand) {
                                                if target == self.prs.borrow().id.get_attr_ident(StructureAttrType::Length).to_abstract() {
                                                    let start_id = self.prs.borrow().id.get_attr_ident(StructureAttrType::Start);
                                                    if let Some(start) = vd.lookup_struct_attr(&start_id) {
                                                        let start = start.clone().expect_position().unwrap();
                                                        let end = BitIndex::bytes(fm.len());
                                                        //dg.lookup_values.insert(target, ExprValue::Position(end - start));
                                                        vd.insert_struct_attr(sai.clone(), ExprValue::Position(end - start));
                                                        dg.mark_known(&target);
                                                    } else {
                                                        dg.add_dependancies(target, vec![start_id.to_abstract()]);
                                                    }
                                                    
                                                } else {
                                                    error!("Invalid file property: {}", target);
                                                    return Err(FileSpecError::ident_not_valid(target))
                                                }
                                            } else {
                                                error!("Invalid file property: {}", target);
                                                return Err(FileSpecError::ident_not_valid(target))
                                            }
                                            break;
                                        } else {
                                            child = prs_map.get(&new_child_id).unwrap().clone();
                                        }
                                        
                                    }
                                }
                            }
                        }

                        #[cfg(test)] {
                            struct_attr_timer += now.elapsed();
                            struct_attr_count += 1;
                        }
                    }
                }


            }

            let now = std::time::Instant::now();

            targets = dg.get_resolvable_unknowns(&vd);//Vec::new();
            
            #[cfg(test)] {
                unknowns_timer += now.elapsed();
            }


            info!("{:?}", targets);
            if targets.is_empty() {
                break;
            }
        }

        #[cfg(test)] {
            println!("Field: {}ms, {}", field_timer.as_millis(), field_count);
            println!("Field attr: {}ms, {}", field_attr_timer.as_millis(), field_attr_count);
            println!("Struct attr: {}ms, {}", struct_attr_timer.as_millis(), struct_attr_count);
            println!("Unknowns: {}ms", unknowns_timer.as_millis());
        }

        self.dep_graph = dg;
        self.value_dict = vd;
        self.prs_map = prs_map;

        Ok(())
    }

    pub fn initialize(&mut self, fm: &mut crate::FileManager) -> Result<(), FileSpecError> {
        let vd = std::mem::take(&mut self.value_dict);
        let (unk0, unk1) = self.prs.borrow().unknowns(&vd).unwrap();
        self.value_dict = vd;

        info!("Unknowns 0: {:?}", unk0);
        info!("Unknowns 1: {:?}", unk1);

        let mut targets: Vec<AbstractIdent> = unk0.into_iter().collect();
        targets.extend(unk1);

        let mut interval = 0;
        loop {
            let now = std::time::Instant::now();

            let prev_prs_map: HashSet<StructureIdent> = self.prs_map.keys().cloned().collect();

            #[cfg(test)] {
                println!("\tCopy PRS map: {}ms", now.elapsed().as_millis());
            }

            let n = targets.len();

            self.get_data(targets, fm)?;

            #[cfg(test)] {
                println!("\tGet Data (targets: {}, knowns: {}): {}ms", n, self.value_dict.len(), now.elapsed().as_millis());
            }

            // Discover new unkowns
            for k in self.prs_map.keys() {
                if !prev_prs_map.contains(k) {
                    let (mut unk0, unk1) = self.prs_map[k].borrow().unknowns(&self.value_dict).unwrap();
                    unk0.extend(unk1);
                    for ident in unk0 {
                        if !self.dep_graph.dep_map.contains_key(&ident) {
                            self.dep_graph.dep_map.insert(ident.clone(), Rc::new(RefCell::new(DepNode::new(ident.clone()))));
                            self.dep_graph.dep_roots.insert(ident);
                        }
                    }
                }
            }

            #[cfg(test)] {
                println!("\tGet Unknowns: {}ms", now.elapsed().as_millis());
            }

            targets = self.dep_graph.get_resolvable_unknowns(&self.value_dict);

            #[cfg(test)] {
                println!("Interval #{}: {}ms", interval, now.elapsed().as_millis());
            }

            if targets.is_empty() {
                return Ok(())
            }
            interval += 1;
            info!("============= INTERVAL #{} ==============", interval);
            info!("Knowns: {}, Total: {}, Targets: {}", self.value_dict.len(), self.dep_graph.dep_map.len(), targets.len());
            if interval > 7000 {
                // println!("Width: {:?}", vd.lookup_any(&width_id).unwrap());

                panic!()
            }
        }
    }

}

pub fn make_png() -> Structure {
    let fname = "png-spec.xml".to_string();
    let s = std::fs::read_to_string(&fname).unwrap();
    match Structure::from_xml(&s, Some(Rc::new(fname.clone()))) {
        Ok(s) => {
            // s.set_fname(fname);
            s
        },
        Err(mut err) => {
            err.set_fname(fname);
            let error_string = err.get_message(&s);
            println!("{}", error_string);
            panic!()
        }
    }
}

#[cfg(test)]
struct PngIhdr {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8
}

#[cfg(test)]
struct PngTime {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8
}

#[cfg(test)]
enum PngSbit {
    Gray(u8),
    Rgb(u8, u8, u8),
    GrayAlpha(u8, u8),
    Rgba(u8, u8, u8, u8)
}

#[cfg(test)]
enum PngTrns {
    Gray(u16),
    Rgb(u16, u16, u16),
    Indexed(Vec<u8>)
}

#[cfg(test)]
enum PngBkgd {
    Gray(u16),
    Rgb(u16, u16, u16),
    Indexed(u8)
}

#[cfg(test)]
enum ExifIfdData {
    UByte(Vec<u8>),
    String(String),
    UShort(Vec<u16>),
    ULong(Vec<u32>),
    URat(Vec<(u32, u32)>),
    SByte(Vec<i8>),
    Undefined(Vec<u8>),
    SShort(Vec<i16>),
    SLong(Vec<i32>),
    SRat(Vec<(i32, i32)>),
    Float(Vec<f32>),
    Double(Vec<f64>)
}

#[cfg(test)]
impl ExifIfdData {
    fn format(&self) -> u16 {
        match self {
            ExifIfdData::UByte(_) => 1,
            ExifIfdData::String(_) => 2,
            ExifIfdData::UShort(_) => 3,
            ExifIfdData::ULong(_) => 4,
            ExifIfdData::URat(_) => 5,
            ExifIfdData::SByte(_) => 6,
            ExifIfdData::Undefined(_) => 7,
            ExifIfdData::SShort(_) => 8,
            ExifIfdData::SLong(_) => 9,
            ExifIfdData::SRat(_) => 10,
            ExifIfdData::Float(_) => 11,
            ExifIfdData::Double(_) => 12
        }
    }

    fn num(&self) -> usize {
        match self {
            ExifIfdData::UByte(v) => v.len(),
            ExifIfdData::String(s) => s.len(),
            ExifIfdData::UShort(v) => v.len(),
            ExifIfdData::ULong(v) => v.len(),
            ExifIfdData::URat(v) => v.len(),
            ExifIfdData::SByte(v) => v.len(),
            ExifIfdData::Undefined(v) => v.len(),
            ExifIfdData::SShort(v) => v.len(),
            ExifIfdData::SLong(v) => v.len(),
            ExifIfdData::SRat(v) => v.len(),
            ExifIfdData::Float(v) => v.len(),
            ExifIfdData::Double(v) => v.len()
        }
    }
}

#[cfg(test)]
struct ExifIfdEntry {
    tag: u32,
    address: Option<u32>,
    data: ExifIfdData
}


#[cfg(test)]
mod png_tests {
    use super::*;

    fn start_file<'a>(filename: &'a str) -> (FileMap<'a>, crate::FileManager) {
        // pretty_env_logger::init();
        let s = Rc::new(make_png());
        let mut fm = crate::FileManager::new(filename.to_string(), crate::FileManagerType::ReadOnly, false).unwrap();
        let mut png = FileMap::new(s, &mut fm);
        let now = std::time::Instant::now();
        png.initialize(&mut fm).unwrap();
        println!("Initialize: {}", now.elapsed().as_millis());
        (png, fm)
    }

    fn validate_chunks(png: &mut FileMap, fm: &mut crate::FileManager, chunks: Vec<(u32, &str, u32)>) {

        let png_chunks = StructureIdent::new_indexed("png_chunks".to_string(), vec![1]);
        let png_chunks_reps = png_chunks.get_attr_ident(StructureAttrType::Repetitions);

        let mut targets = vec![png_chunks_reps.to_abstract()];
        let mut results = vec![ExprValue::Integer(chunks.len() as i64)];

        for (i, chunk) in chunks.iter().enumerate() {
            let chunk_header = StructureIdent::new_indexed("chunk_header".to_string(), vec![1, i, 0]);
            let chunk_length = chunk_header.clone().get_field_ident("chunk_length".to_string());
            let chunk_type = chunk_header.clone().get_field_ident("chunk_type".to_string());
            let chunk_footer = StructureIdent::new_indexed("chunk_footer".to_string(), vec![1, i, 2]);
            let chunk_crc = chunk_footer.clone().get_field_ident("chunk_crc".to_string());
            targets.push(chunk_length.to_abstract());
            results.push(ExprValue::Integer(chunk.0 as i64));
            targets.push(chunk_type.to_abstract());
            results.push(ExprValue::String(chunk.1.to_string()));
            targets.push(chunk_crc.to_abstract());
            results.push(ExprValue::Integer(chunk.2 as i64));
        }

        let now = std::time::Instant::now();
        png.get_data(targets.clone(), fm).unwrap();
        println!("Getting chunks: {}", now.elapsed().as_millis());

        for (target, result) in targets.iter().zip(results.into_iter()) {
            assert_eq!(png.value_dict.lookup_any(&target).unwrap(), result);
        }
    }

    fn validate_ihdr(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, ihdr: PngIhdr) {

        let ihdr_struct = StructureIdent::new_indexed("ihdr_body".to_string(), vec![1, index, 1, 0]);
        let ihdr_width = ihdr_struct.clone().get_field_ident("ihdr_width".to_string()).to_abstract();
        let ihdr_height = ihdr_struct.clone().get_field_ident("ihdr_height".to_string()).to_abstract();
        let ihdr_bit_depth = ihdr_struct.clone().get_field_ident("ihdr_bit_depth".to_string()).to_abstract();
        let ihdr_color_type = ihdr_struct.clone().get_field_ident("ihdr_color_type".to_string()).to_abstract();
        let ihdr_compression_method = ihdr_struct.clone().get_field_ident("ihdr_compression_method".to_string()).to_abstract();
        let ihdr_filter_method = ihdr_struct.clone().get_field_ident("ihdr_filter_method".to_string()).to_abstract();
        let ihdr_interlace_method = ihdr_struct.clone().get_field_ident("ihdr_interlace_method".to_string()).to_abstract();

        let targets = vec![
            ihdr_width.clone(),
            ihdr_height.clone(),
            ihdr_bit_depth.clone(),
            ihdr_color_type.clone(),
            ihdr_compression_method.clone(),
            ihdr_filter_method.clone(),
            ihdr_interlace_method.clone(),
        ];

        png.get_data(targets.clone(), fm).unwrap();

        assert_eq!(png.value_dict.lookup_any(&ihdr_width).unwrap(), ExprValue::Integer(ihdr.width as i64));
        assert_eq!(png.value_dict.lookup_any(&ihdr_height).unwrap(), ExprValue::Integer(ihdr.height as i64));
        assert_eq!(png.value_dict.lookup_any(&ihdr_bit_depth).unwrap(), ExprValue::Integer(ihdr.bit_depth as i64));
        assert_eq!(png.value_dict.lookup_any(&ihdr_color_type).unwrap(), ExprValue::Integer(ihdr.color_type as i64));
        assert_eq!(png.value_dict.lookup_any(&ihdr_compression_method).unwrap(), ExprValue::Integer(ihdr.compression_method as i64));
        assert_eq!(png.value_dict.lookup_any(&ihdr_filter_method).unwrap(), ExprValue::Integer(ihdr.filter_method as i64));
        assert_eq!(png.value_dict.lookup_any(&ihdr_interlace_method).unwrap(), ExprValue::Integer(ihdr.interlace_method as i64));

    }

    fn validate_plte(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, rgb: Vec<(u8, u8, u8)>) {

        let plte_body = StructureIdent::new_indexed("plte_body".to_string(), vec![1, index, 1, 0]);
        let plte_reps = plte_body.get_attr_ident(StructureAttrType::Repetitions).to_abstract();

        let targets = vec![
            plte_reps.clone()
        ];

        png.get_data(targets.clone(), fm).unwrap();

        assert_eq!(png.value_dict.lookup_any(&plte_reps).unwrap(), ExprValue::Integer(rgb.len() as i64));

        let mut targets = vec![];
        for i in 0..rgb.len() {
            let plte_pallette = StructureIdent::new_indexed("plte_pallette".to_string(), vec![1, index, 1, 0, i]);
            targets.push(plte_pallette.clone().get_field_ident("plte_red".to_string()).to_abstract());
            targets.push(plte_pallette.clone().get_field_ident("plte_green".to_string()).to_abstract());
            targets.push(plte_pallette.clone().get_field_ident("plte_blue".to_string()).to_abstract());
        }

        png.get_data(targets.clone(), fm).unwrap();

        let mut target_index = 0;
        for (r, g, b) in rgb {
            assert_eq!(png.value_dict.lookup_any(&targets[target_index + 0]).unwrap(), ExprValue::Integer(r as i64));
            assert_eq!(png.value_dict.lookup_any(&targets[target_index + 1]).unwrap(), ExprValue::Integer(g as i64));
            assert_eq!(png.value_dict.lookup_any(&targets[target_index + 2]).unwrap(), ExprValue::Integer(b as i64));
            target_index += 3;
        }

    }

    fn validate_trns(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, trns: PngTrns) {
        match trns {
            PngTrns::Gray(k) => {
                let trns_struct = StructureIdent::new_indexed("trns_body_gray".to_string(), vec![1, index, 1, 0, 0]);
                let trns_gray = trns_struct.clone().get_field_ident("trns_gray".to_string()).to_abstract();

                let targets = vec![
                    trns_gray.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&trns_gray).unwrap(), ExprValue::Integer(k as i64));
            },
            PngTrns::Rgb(r, g, b) => {
                let trns_struct = StructureIdent::new_indexed("trns_body_rgb".to_string(), vec![1, index, 1, 0, 0]);
                let trns_red = trns_struct.clone().get_field_ident("trns_red".to_string()).to_abstract();
                let trns_green = trns_struct.clone().get_field_ident("trns_green".to_string()).to_abstract();
                let trns_blue = trns_struct.clone().get_field_ident("trns_blue".to_string()).to_abstract();

                let targets = vec![
                    trns_red.clone(),
                    trns_green.clone(),
                    trns_blue.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&trns_red).unwrap(), ExprValue::Integer(r as i64));
                assert_eq!(png.value_dict.lookup_any(&trns_green).unwrap(), ExprValue::Integer(g as i64));
                assert_eq!(png.value_dict.lookup_any(&trns_blue).unwrap(), ExprValue::Integer(b as i64));
            },
            PngTrns::Indexed(v) => {
                let trns_body = StructureIdent::new_indexed("trns_body_indexed".to_string(), vec![1, index, 1, 0, 0]);
                let trns_reps = trns_body.get_attr_ident(StructureAttrType::Repetitions).to_abstract();

                let targets = vec![
                    trns_reps.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&trns_reps).unwrap(), ExprValue::Integer(v.len() as i64));

                let mut targets = vec![];
                for i in 0..v.len() {
                    let trns_pallette = StructureIdent::new_indexed("trns_pallette".to_string(), vec![1, index, 1, 0, 0, i]);
                    let trns_alpha = trns_pallette.get_field_ident("trns_alpha".to_string()).to_abstract();
                    targets.push(trns_alpha)
                }

                png.get_data(targets.clone(), fm).unwrap();

                for (target, alpha) in targets.iter().zip(v.into_iter()) {
                    assert_eq!(png.value_dict.lookup_any(target).unwrap(), ExprValue::Integer(alpha as i64));
                }
            }
        }
    }

    fn validate_gama(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, gamma: u32) {
        let gama_struct = StructureIdent::new_indexed("gama_body".to_string(), vec![1, index, 1, 0]);
        let gama_gamma = gama_struct.clone().get_field_ident("gama_gamma".to_string()).to_abstract();

        let targets = vec![
            gama_gamma.clone()
        ];

        png.get_data(targets.clone(), fm).unwrap();

        assert_eq!(png.value_dict.lookup_any(&gama_gamma).unwrap(), ExprValue::Integer(gamma as i64));
    }

    fn validate_sbit(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, sbit: PngSbit) {
        match sbit {
            PngSbit::Gray(k) => {
                let sbit_struct = StructureIdent::new_indexed("sbit_body_gray".to_string(), vec![1, index, 1, 0, 0]);
                let sbit_gray = sbit_struct.clone().get_field_ident("sbit_gray".to_string()).to_abstract();

                let targets = vec![
                    sbit_gray.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&sbit_gray).unwrap(), ExprValue::Integer(k as i64));
            },
            PngSbit::Rgb(r, g, b) => {
                let sbit_struct = StructureIdent::new_indexed("sbit_body_rgb".to_string(), vec![1, index, 1, 0, 0]);
                let sbit_red = sbit_struct.clone().get_field_ident("sbit_red".to_string()).to_abstract();
                let sbit_green = sbit_struct.clone().get_field_ident("sbit_green".to_string()).to_abstract();
                let sbit_blue = sbit_struct.clone().get_field_ident("sbit_blue".to_string()).to_abstract();

                let targets = vec![
                    sbit_red.clone(),
                    sbit_green.clone(),
                    sbit_blue.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&sbit_red).unwrap(), ExprValue::Integer(r as i64));
                assert_eq!(png.value_dict.lookup_any(&sbit_green).unwrap(), ExprValue::Integer(g as i64));
                assert_eq!(png.value_dict.lookup_any(&sbit_blue).unwrap(), ExprValue::Integer(b as i64));
            },
            PngSbit::GrayAlpha(k, a) => {
                let sbit_struct = StructureIdent::new_indexed("sbit_body_ga".to_string(), vec![1, index, 1, 0, 0]);
                let sbit_gray = sbit_struct.clone().get_field_ident("sbit_gray".to_string()).to_abstract();
                let sbit_alpha = sbit_struct.clone().get_field_ident("sbit_alpha".to_string()).to_abstract();

                let targets = vec![
                    sbit_gray.clone(),
                    sbit_alpha.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&sbit_gray).unwrap(), ExprValue::Integer(k as i64));
                assert_eq!(png.value_dict.lookup_any(&sbit_alpha).unwrap(), ExprValue::Integer(a as i64));
            },
            PngSbit::Rgba(r, g, b, a) => {
                let sbit_struct = StructureIdent::new_indexed("sbit_body_rgba".to_string(), vec![1, index, 1, 0, 0]);
                let sbit_red = sbit_struct.clone().get_field_ident("sbit_red".to_string()).to_abstract();
                let sbit_green = sbit_struct.clone().get_field_ident("sbit_green".to_string()).to_abstract();
                let sbit_blue = sbit_struct.clone().get_field_ident("sbit_blue".to_string()).to_abstract();
                let sbit_alpha = sbit_struct.clone().get_field_ident("sbit_alpha".to_string()).to_abstract();

                let targets = vec![
                    sbit_red.clone(),
                    sbit_green.clone(),
                    sbit_blue.clone(),
                    sbit_alpha.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&sbit_red).unwrap(), ExprValue::Integer(r as i64));
                assert_eq!(png.value_dict.lookup_any(&sbit_green).unwrap(), ExprValue::Integer(g as i64));
                assert_eq!(png.value_dict.lookup_any(&sbit_blue).unwrap(), ExprValue::Integer(b as i64));
                assert_eq!(png.value_dict.lookup_any(&sbit_alpha).unwrap(), ExprValue::Integer(a as i64));
            },
        }
    }

    fn validate_bkgd(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, bkgd: PngBkgd) {
        match bkgd {
            PngBkgd::Gray(k) => {
                let bkgd_struct = StructureIdent::new_indexed("bkgd_body_gray".to_string(), vec![1, index, 1, 0, 0]);
                let bkgd_gray = bkgd_struct.clone().get_field_ident("bkgd_gray".to_string()).to_abstract();

                let targets = vec![
                    bkgd_gray.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&bkgd_gray).unwrap(), ExprValue::Integer(k as i64));
            },
            PngBkgd::Rgb(r, g, b) => {
                let bkgd_struct = StructureIdent::new_indexed("bkgd_body_rgb".to_string(), vec![1, index, 1, 0, 0]);
                let bkgd_red = bkgd_struct.clone().get_field_ident("bkgd_red".to_string()).to_abstract();
                let bkgd_green = bkgd_struct.clone().get_field_ident("bkgd_green".to_string()).to_abstract();
                let bkgd_blue = bkgd_struct.clone().get_field_ident("bkgd_blue".to_string()).to_abstract();

                let targets = vec![
                    bkgd_red.clone(),
                    bkgd_green.clone(),
                    bkgd_blue.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&bkgd_red).unwrap(), ExprValue::Integer(r as i64));
                assert_eq!(png.value_dict.lookup_any(&bkgd_green).unwrap(), ExprValue::Integer(g as i64));
                assert_eq!(png.value_dict.lookup_any(&bkgd_blue).unwrap(), ExprValue::Integer(b as i64));
            },
            PngBkgd::Indexed(a) => {
                let bkgd_struct = StructureIdent::new_indexed("bkgd_body_indexed".to_string(), vec![1, index, 1, 0, 0]);
                let bkgd_alpha = bkgd_struct.clone().get_field_ident("bkgd_alpha".to_string()).to_abstract();

                let targets = vec![
                    bkgd_alpha.clone()
                ];

                png.get_data(targets.clone(), fm).unwrap();

                assert_eq!(png.value_dict.lookup_any(&bkgd_alpha).unwrap(), ExprValue::Integer(a as i64));
            }
        }
    }

    fn validate_splt(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, name: &str, depth: u8, rgbaf: Vec<(u16, u16, u16, u16, u16)>) {
        let splt_header = StructureIdent::new_indexed("splt_header".to_string(), vec![1, index, 1, 0, 0]);
        let splt_name = splt_header.clone().get_field_ident("splt_name".to_string()).to_abstract();
        let splt_depth = splt_header.clone().get_field_ident("splt_sample_depth".to_string()).to_abstract();
        let splt_body = StructureIdent::new_indexed("splt_body".to_string(), vec![1, index, 1, 0, 1, 0]);
        let splt_reps = splt_body.get_attr_ident(StructureAttrType::Repetitions).to_abstract();

        let targets = vec![
            splt_name.clone(),
            splt_depth.clone(),
            splt_reps.clone()
        ];

        png.get_data(targets.clone(), fm).unwrap();

        assert_eq!(png.value_dict.lookup_any(&splt_name).unwrap(), ExprValue::String(name.to_string()));
        assert_eq!(png.value_dict.lookup_any(&splt_depth).unwrap(), ExprValue::Integer(depth as i64));
        assert_eq!(png.value_dict.lookup_any(&splt_reps).unwrap(), ExprValue::Integer(rgbaf.len() as i64));

        let mut targets = vec![];
        for i in 0..rgbaf.len() {
            let splt_pallette = StructureIdent::new_indexed("splt_pallette".to_string(), vec![1, index, 1, 0, 1, 0, i]);
            targets.push(splt_pallette.clone().get_field_ident("splt_red".to_string()).to_abstract());
            targets.push(splt_pallette.clone().get_field_ident("splt_green".to_string()).to_abstract());
            targets.push(splt_pallette.clone().get_field_ident("splt_blue".to_string()).to_abstract());
            targets.push(splt_pallette.clone().get_field_ident("splt_alpha".to_string()).to_abstract());
            targets.push(splt_pallette.get_field_ident("splt_freq".to_string()).to_abstract());
        }

        png.get_data(targets.clone(), fm).unwrap();

        let mut target_index = 0;
        for (r, g, b, a, f) in rgbaf {
            assert_eq!(png.value_dict.lookup_any(&targets[target_index + 0]).unwrap(), ExprValue::Integer(r as i64));
            assert_eq!(png.value_dict.lookup_any(&targets[target_index + 1]).unwrap(), ExprValue::Integer(g as i64));
            assert_eq!(png.value_dict.lookup_any(&targets[target_index + 2]).unwrap(), ExprValue::Integer(b as i64));
            assert_eq!(png.value_dict.lookup_any(&targets[target_index + 3]).unwrap(), ExprValue::Integer(a as i64));
            assert_eq!(png.value_dict.lookup_any(&targets[target_index + 4]).unwrap(), ExprValue::Integer(f as i64));
            target_index += 5;
        }
    }

    fn validate_hist(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, freqs: Vec<u16>) {

        let hist_body = StructureIdent::new_indexed("hist_body".to_string(), vec![1, index, 1, 0]);
        let hist_reps = hist_body.get_attr_ident(StructureAttrType::Repetitions).to_abstract();

        let targets = vec![
            hist_reps.clone()
        ];

        png.get_data(targets.clone(), fm).unwrap();

        assert_eq!(png.value_dict.lookup_any(&hist_reps).unwrap(), ExprValue::Integer(freqs.len() as i64));

        let mut targets = vec![];
        for i in 0..freqs.len() {
            let hist_entry = StructureIdent::new_indexed("hist_entry".to_string(), vec![1, index, 1, 0, i]);
            targets.push(hist_entry.get_field_ident("hist_freq".to_string()).to_abstract());
        }

        png.get_data(targets.clone(), fm).unwrap();

        for (target, f) in targets.iter().zip(freqs.into_iter()) {
            assert_eq!(png.value_dict.lookup_any(target).unwrap(), ExprValue::Integer(f as i64));
        }

    }

    fn validate_time(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, time: PngTime) {

        let time_struct = StructureIdent::new_indexed("time_body".to_string(), vec![1, index, 1, 0]);
        let time_year = time_struct.clone().get_field_ident("time_year".to_string()).to_abstract();
        let time_month = time_struct.clone().get_field_ident("time_month".to_string()).to_abstract();
        let time_day = time_struct.clone().get_field_ident("time_day".to_string()).to_abstract();
        let time_hour = time_struct.clone().get_field_ident("time_hour".to_string()).to_abstract();
        let time_minute = time_struct.clone().get_field_ident("time_minute".to_string()).to_abstract();
        let time_second = time_struct.clone().get_field_ident("time_second".to_string()).to_abstract();

        let targets = vec![
            time_year.clone(),
            time_month.clone(),
            time_day.clone(),
            time_hour.clone(),
            time_minute.clone(),
            time_second.clone(),
        ];

        png.get_data(targets.clone(), fm).unwrap();

        assert_eq!(png.value_dict.lookup_any(&time_year).unwrap(), ExprValue::Integer(time.year as i64));
        assert_eq!(png.value_dict.lookup_any(&time_month).unwrap(), ExprValue::Integer(time.month as i64));
        assert_eq!(png.value_dict.lookup_any(&time_day).unwrap(), ExprValue::Integer(time.day as i64));
        assert_eq!(png.value_dict.lookup_any(&time_hour).unwrap(), ExprValue::Integer(time.hour as i64));
        assert_eq!(png.value_dict.lookup_any(&time_minute).unwrap(), ExprValue::Integer(time.minute as i64));
        assert_eq!(png.value_dict.lookup_any(&time_second).unwrap(), ExprValue::Integer(time.second as i64));

    }

    fn validate_ifd(png: &mut FileMap, fm: &mut crate::FileManager, struct_id: StructureIdent, ifd0: Vec<ExifIfdEntry>) {
        // let ifd_struct = StructureIdent::new_indexed("exif_ifd_entries".to_string(), vec![1, index, 1, 0, 1, 0, 1]);
        let ifd_struct = struct_id.get_child_ident("exif_ifd_entries".to_string(), vec![1]);
        let ifd0_reps = ifd_struct.get_attr_ident(StructureAttrType::Repetitions).to_abstract();

        let targets = vec![ifd0_reps.clone()];

        png.get_data(targets.clone(), fm).unwrap();

        assert_eq!(png.value_dict.lookup_any(&ifd0_reps).unwrap(), ExprValue::Integer(ifd0.len() as i64));


        for (i, ifd) in ifd0.iter().enumerate() {

            // let ifd_entry_header = StructureIdent::new_indexed("exif_ifd_entry_header".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 0]);
            let ifd_entry_header = ifd_struct.get_child_ident("exif_ifd_entry_header".to_string(), vec![i, 0]);
            let ifd_tag = ifd_entry_header.get_field_ident("exif_ifd_tag".to_string()).to_abstract();
            let ifd_format = ifd_entry_header.get_field_ident("exif_ifd_format".to_string()).to_abstract();
            let ifd_num = ifd_entry_header.get_field_ident("exif_ifd_num".to_string()).to_abstract();
            let targets = vec![
                ifd_tag.clone(),
                ifd_format.clone(),
                ifd_num.clone()
            ];
            png.get_data(targets.clone(), fm).unwrap();
            assert_eq!(png.value_dict.lookup_any(&ifd_tag).unwrap(), ExprValue::Integer(ifd.tag as i64));
            assert_eq!(png.value_dict.lookup_any(&ifd_format).unwrap(), ExprValue::Integer(ifd.data.format() as i64));
            assert_eq!(png.value_dict.lookup_any(&ifd_num).unwrap(), ExprValue::Integer(ifd.data.num() as i64));

            let ifd_entry_body = ifd_struct.get_child_ident("exif_ifd_entry_body".to_string(), vec![i, 1]);

            let mut targets = vec![];

            match &ifd.data {
                ExifIfdData::String(s) => {
                    if let Some(addr) = ifd.address {
                        // let ifd_entry_addr_section = StructureIdent::new_indexed("exif_ifd_address_ascii_section".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 0]);
                        let ifd_entry_addr_section = ifd_entry_body.get_child_ident("exif_ifd_address_ascii_section".to_string(), vec![0, 0, 0]);
                        let ifd_entry_addr = ifd_entry_addr_section.get_field_ident("exif_ifd_address_ascii".to_string()).to_abstract();
                        // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_ascii_body_addressed".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 1, 0]);
                        let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_ascii_body_addressed".to_string(), vec![0, 0, 1, 0]);
                        let ifd_entry_value = ifd_entry_data.get_field_ident("exif_ifd_ascii".to_string()).to_abstract();
                        targets.push(ifd_entry_addr.clone());
                        targets.push(ifd_entry_value.clone());
                        png.get_data(targets.clone(), fm).unwrap();
                        assert_eq!(png.value_dict.lookup_any(&ifd_entry_addr).unwrap(), ExprValue::Integer(addr as i64));
                        assert_eq!(png.value_dict.lookup_any(&ifd_entry_value).unwrap(), ExprValue::String(s.clone()));
                    } else {
                        // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_ascii_body".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0]);
                        let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_ascii_body".to_string(), vec![0, 0]);
                        targets.push(ifd_entry_data.get_field_ident("exif_ifd_ascii".to_string()).to_abstract());
                        png.get_data(targets.clone(), fm).unwrap();
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::String(s.clone()));
                    }
                },
                ExifIfdData::UShort(v) => {
                    if let Some(addr) = ifd.address {
                        let ifd_entry_addr_section = ifd_entry_body.get_child_ident("exif_ifd_address_u16_section".to_string(), vec![0, 0, 0]);
                        let ifd_entry_addr = ifd_entry_addr_section.get_field_ident("exif_ifd_address_u16".to_string()).to_abstract();
                        targets.push(ifd_entry_addr);
                        for j in 0..v.len() {
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_u16_body_addressed".to_string(), vec![0, 0, 1, 0, j]);
                            let ifd_entry_value = ifd_entry_data.get_field_ident("exif_ifd_u16".to_string()).to_abstract();
                            targets.push(ifd_entry_value);
                        }
                        png.get_data(targets.clone(), fm).unwrap();
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::Integer(addr as i64));
                        for (target, x) in targets.iter().skip(1).zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target).unwrap(), ExprValue::Integer(*x as i64));
                        }
                    } else {
                        for j in 0..v.len() {
                            let field_tag = match ifd.tag {
                                259 => "exif_compression".to_string(),
                                _ => "exif_ifd_u16".to_string()
                            };
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_u16_body".to_string(), vec![0, 0, 0, j]);
                            targets.push(ifd_entry_data.get_field_ident(field_tag).to_abstract());
                        }
                        png.get_data(targets.clone(), fm).unwrap();
                        for (target, x) in targets.iter().zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target).unwrap(), ExprValue::Integer(*x as i64));
                        }
                    }
                },
                ExifIfdData::ULong(v) => {
                    if let Some(addr) = ifd.address {
                        // let ifd_entry_addr_section = StructureIdent::new_indexed("exif_ifd_address_u32_section".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 0]);
                        let ifd_entry_addr_section = ifd_entry_body.get_child_ident("exif_ifd_address_u32_section".to_string(), vec![0, 0, 0]);
                        let ifd_entry_addr = ifd_entry_addr_section.get_field_ident("exif_ifd_address_u32".to_string()).to_abstract();
                        targets.push(ifd_entry_addr);
                        for j in 0..v.len() {
                            // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_u32_body_addressed".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 1, 0, j]);
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_u32_body_addressed".to_string(), vec![0, 0, 1, 0, j]);
                            let ifd_entry_value = ifd_entry_data.get_field_ident("exif_ifd_u32".to_string()).to_abstract();
                            targets.push(ifd_entry_value);
                        }
                        png.get_data(targets.clone(), fm).unwrap();
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::Integer(addr as i64));
                        for (target, x) in targets.iter().skip(1).zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target).unwrap(), ExprValue::Integer(*x as i64));
                        }
                    } else {
                        for j in 0..v.len() {
                            let field_tag = match ifd.tag {
                                513 => "exif_jpeg_offset".to_string(),
                                514 => "exif_jpeg_byte_count".to_string(),
                                34665 => "exif_subifd_offset".to_string(),
                                _ => "exif_ifd_u32".to_string()
                            };
                            // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_u32_body".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 0, j]);
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_u32_body".to_string(), vec![0, 0, 0, j]);
                            targets.push(ifd_entry_data.get_field_ident(field_tag).to_abstract());
                        }
                        png.get_data(targets.clone(), fm).unwrap();
                        for (target, x) in targets.iter().zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target).unwrap(), ExprValue::Integer(*x as i64));
                        }
                    }
                },
                ExifIfdData::URat(v) => {
                    if let Some(addr) = ifd.address {
                        // let ifd_entry_addr_section = StructureIdent::new_indexed("exif_ifd_address_urat_section".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 0]);
                        let ifd_entry_addr_section = ifd_entry_body.get_child_ident("exif_ifd_address_urat_section".to_string(), vec![0, 0, 0]);
                        let ifd_entry_addr = ifd_entry_addr_section.get_field_ident("exif_ifd_address_urat".to_string()).to_abstract();
                        targets.push(ifd_entry_addr);
                        for j in 0..v.len() {
                            // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_urat_body_addressed".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 1, 0, j]);
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_urat_body_addressed".to_string(), vec![0, 0, 1, 0, j]);
                            let ifd_num_entry_value = ifd_entry_data.get_field_ident("exif_ifd_urat_num".to_string()).to_abstract();
                            let ifd_den_entry_value = ifd_entry_data.get_field_ident("exif_ifd_urat_den".to_string()).to_abstract();
                            targets.push(ifd_num_entry_value);
                            targets.push(ifd_den_entry_value);
                        }
                        png.get_data(targets.clone(), fm).unwrap();
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::Integer(addr as i64));
                        for (target, (num, den)) in targets[1..].chunks(2).zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target[0]).unwrap(), ExprValue::Integer(*num as i64));
                            assert_eq!(png.value_dict.lookup_any(&target[1]).unwrap(), ExprValue::Integer(*den as i64));
                        }
                    } else {
                        for j in 0..v.len() {
                            // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_urat_body".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 0, j]);
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_urat_body".to_string(), vec![0, 0, 0, j]);
                            targets.push(ifd_entry_data.get_field_ident("exif_ifd_urat_num".to_string()).to_abstract());
                            targets.push(ifd_entry_data.get_field_ident("exif_ifd_urat_den".to_string()).to_abstract());
                        }
                        png.get_data(targets.clone(), fm).unwrap();
                        for (target, (num, den)) in targets.chunks(2).zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target[0]).unwrap(), ExprValue::Integer(*num as i64));
                            assert_eq!(png.value_dict.lookup_any(&target[1]).unwrap(), ExprValue::Integer(*den as i64));
                        }
                    }
                },
                ExifIfdData::Undefined(v) => {
                    let bf = BitField::from_vec(v.clone());
                    if let Some(addr) = ifd.address {
                        let ifd_entry_addr_section = ifd_entry_body.get_child_ident("exif_ifd_address_unk_section".to_string(), vec![0, 0, 0]);
                        let ifd_entry_addr = ifd_entry_addr_section.get_field_ident("exif_ifd_address_unk".to_string()).to_abstract();
                        let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_unk_body_addressed".to_string(), vec![0, 0, 1, 0]);
                        let ifd_entry_value = ifd_entry_data.get_field_ident("exif_ifd_unk".to_string()).to_abstract();
                        targets.push(ifd_entry_addr.clone());
                        targets.push(ifd_entry_value.clone());
                        png.get_data(targets.clone(), fm).unwrap();
                        assert_eq!(png.value_dict.lookup_any(&ifd_entry_addr).unwrap(), ExprValue::Integer(addr as i64));
                        assert_eq!(png.value_dict.lookup_any(&ifd_entry_value).unwrap(), ExprValue::Bytes(bf));
                    } else {
                        let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_unk_body".to_string(), vec![0, 0]);
                        targets.push(ifd_entry_data.get_field_ident("exif_ifd_unk".to_string()).to_abstract());
                        png.get_data(targets.clone(), fm).unwrap();
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::Bytes(bf));
                    }
                },
                _ => {}
            }
            
            
        }
    }

    fn validate_exif(png: &mut FileMap, fm: &mut crate::FileManager, index: usize, ifd_vec: Vec<Vec<ExifIfdEntry>>, subifd: Vec<ExifIfdEntry>) {

        for (i, ifd) in ifd_vec.into_iter().enumerate() {
            let ifd_struct = if i == 0 {
                StructureIdent::new_indexed("exif_ifd".to_string(), vec![1, index, 1, 0, 1, 0])
            } else {
                StructureIdent::new_indexed("exif_ifd".to_string(), vec![1, index, 1, 0, 1, i, 0])
            };
            validate_ifd(png, fm, ifd_struct, ifd);
        }

        let subifd_struct = StructureIdent::new_indexed("exif_ifd".to_string(), vec![1, index, 1, 0, 2, 0, 0, 0]);
        validate_ifd(png, fm, subifd_struct, subifd);
    }

    #[test]
    fn idat_ps2n0g08() {
        let (mut png, mut fm) = start_file(r"tests/ps2n0g08.png");

        let assembly_body_id = StructureIdent::new_indexed("idat_assembly".to_string(), vec![2]);

        let idat = BitField::from_vec(vec![
            0x78, 0x9c, 0x63, 0x64, 0x60, 0x24, 0x00, 0x14, 0x08, 0xc8, 0xb3, 0x0c, 0x05, 0x05, 0x8c, 
            0x0f, 0x08, 0x29, 0xf8, 0xf7, 0x1f, 0x3f, 0x60, 0x79, 0x30, 0x1c, 0x14, 0x30, 0xca, 0x11, 
            0x90, 0x67, 0x64, 0xa2, 0x79, 0x5c, 0x0c, 0x06, 0x05, 0x8c, 0x8f, 0xf0, 0xca, 0xfe, 0xff, 
            0xcf, 0xf8, 0x87, 0xe6, 0x71, 0x31, 0x18, 0x14, 0x30, 0xca, 0xe0, 0x95, 0x65, 0x64, 0x04, 
            0x00, 0x50, 0xe5, 0xfe, 0x71
        ]);

        loop {
            let assembly_body = png.prs_map.get(&assembly_body_id).unwrap();
            let data = assembly_body.borrow().try_get_data(&mut fm, &png.value_dict, &png.prs_map).unwrap();
            match data.result {
                DepResult::Success(data) => {
                    // panic!("Data retreived: {:?}", data.len())
                    assert_eq!(idat, data);
                    break;
                },
                DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                    png.get_data(deps.into_iter().collect(), &mut fm).unwrap();
                },
                DepResult::DoesNotExist => {
                    panic!("Data does not exist")
                }
            }
        }

        
    }

    #[test]
    fn idat_oi4n2c16() {
        let (mut png, mut fm) = start_file(r"tests/oi4n2c16.png");

        let assembly_body_id = StructureIdent::new_indexed("idat_assembly".to_string(), vec![2]);

        let idat = BitField::from_vec(vec![
            0x78, 0x9c, 0xd5, 0x96, 0xc1, 0x0a, 0x83, 0x30, 0x10, 0x44, 0xa7, 0xe0, 0x41, 0x7f, 0xcb, 
            0x7e, 0xb7, 0xfd, 0xad, 0xf6, 0x96, 0x1e, 0x06, 0x03, 0x92, 0x86, 0x26, 0x66, 0x93, 0xcc, 
            0x7a, 0x18, 0x86, 0x45, 0xe4, 0x3d, 0xd6, 0xa0, 0x8f, 0x10, 0x42, 0x00, 0x3e, 0x2f, 0xe0, 
            0x9a, 0xef, 0x64, 0x72, 0x73, 0x7e, 0x18, 0x3d, 0x27, 0x33, 0x5f, 0xce, 0xe2, 0xf3, 0x5a, 
            0x77, 0xb7, 0x02, 0xeb, 0xce, 0x74, 0x28, 0x70, 0xa2, 0x33, 0x97, 0xf3, 0xed, 0xf2, 0x70, 
            0x5d, 0xd1, 0x01, 0x60, 0xf3, 0xb2, 0x81, 0x5f, 0xe8, 0xec, 0xf2, 0x02, 0x79, 0x74, 0xa6, 
            0xb0, 0xc0, 0x3f, 0x74, 0xa6, 0xe4, 0x19, 0x28, 0x43, 0xe7, 0x5c, 0x6c, 0x03, 0x35, 0xe8, 
            0xec, 0x32, 0x02, 0xf5, 0xe8, 0x4c, 0x01, 0x81, 0xbb, 0xe8, 0xcc, 0xa9, 0x67, 0xa0, 0x0d, 
            0x9d, 0xf3, 0x49, 0x1b, 0xb0, 0x40, 0x67, 0x1f, 0x2e, 0x60, 0x87, 0xce, 0x1c, 0x28, 0x60, 
            0x8d, 0x1e, 0x05, 0xf8, 0xc7, 0xee, 0x0f, 0x1d, 0x00, 0xb6, 0x67, 0xe7, 0x0d, 0xf4, 0x44, 
            0x67, 0xef, 0x26, 0xd0, 0x1f, 0xbd, 0x9b, 0xc0, 0x28, 0xf4, 0x28, 0x60, 0xf7, 0x1d, 0x18, 
            0x8b, 0xce, 0xfb, 0x8d, 0x36, 0x30, 0x03, 0x9d, 0xbd, 0x59, 0x60, 0x1e, 0x7a, 0xb3, 0xc0, 
            0x6c, 0xf4, 0x28, 0x50, 0x7f, 0x06, 0x34, 0xd0, 0x39, 0xaf, 0xdc, 0x80, 0x12, 0x3a, 0x7b, 
            0xb1, 0x80, 0x1e, 0x7a, 0xb1, 0x80, 0x2a, 0x7a, 0x14, 0xc8, 0x9f, 0x01, 0x6d, 0x74, 0xce, 
            0x33, 0x1b, 0xf0, 0x80, 0xce, 0x9e, 0x08, 0xf8, 0x41, 0x4f, 0x04, 0xbc, 0xa1, 0x33, 0xbf, 
            0xe6, 0x42, 0xfe, 0x5e
        ]);

        loop {
            let assembly_body = png.prs_map.get(&assembly_body_id).unwrap();
            let data = assembly_body.borrow().try_get_data(&mut fm, &png.value_dict, &png.prs_map).unwrap();
            match data.result {
                DepResult::Success(data) => {
                    // panic!("Data retreived: {:?}", data.len())
                    assert_eq!(idat, data);
                    break;
                },
                DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                    png.get_data(deps.into_iter().collect(), &mut fm).unwrap();
                },
                DepResult::DoesNotExist => {
                    panic!("Data does not exist")
                }
            }
        }

        
    }

    #[test]
    fn chunks_ps2n0g08() {

        let (mut png, mut fm) = start_file(r"tests/ps2n0g08.png");

        let chunk_data = vec![
            (13  , "IHDR", 0x56112528),
            (4   , "gAMA", 0x31e8965f),
            (2170, "sPLT", 0x96d08b86),
            (65  , "IDAT", 0x35e2d859),
            (0   , "IEND", 0xae426082)
        ];

        validate_chunks(&mut png, &mut fm, chunk_data);
    }

    #[test]
    fn chunks_s03n3p01() {

        let (mut png, mut fm) = start_file(r"tests/s03n3p01.png");

        let chunk_data = vec![
            (13  , "IHDR", 0x6ce627fc),
            (4   , "gAMA", 0x31e8965f),
            (3   , "sBIT", 0x77f8b5a3),
            (6   , "PLTE", 0xb1e5a59f),
            (14  , "IDAT", 0x39310c4b),
            (0   , "IEND", 0xae426082)
        ];

        validate_chunks(&mut png, &mut fm, chunk_data);
    }

    #[test]
    fn chunks_tbbn0g04() {

        let (mut png, mut fm) = start_file(r"tests/tbbn0g04.png");

        let chunk_data = vec![
            (13  , "IHDR", 0x93e1c829),
            (4   , "gAMA", 0x31e8965f),
            (2   , "tRNS", 0xe62cd0a9),
            (2   , "bKGD", 0xaa8d2332),
            (328 , "IDAT", 0x5b2a4df7),
            (0   , "IEND", 0xae426082)
        ];

        validate_chunks(&mut png, &mut fm, chunk_data);
    }

    #[test]
    fn chunks_ch1n3p04() {

        let (mut png, mut fm) = start_file(r"tests/ch1n3p04.png");

        let chunk_data = vec![
            (13  , "IHDR", 0x815467c7),
            (4   , "gAMA", 0x31e8965f),
            (3   , "sBIT", 0x77f8b5a3),
            (45  , "PLTE", 0xd2b049bd),
            (30  , "hIST", 0x48995941),
            (71  , "IDAT", 0x0f82057d),
            (0   , "IEND", 0xae426082)
        ];

        validate_chunks(&mut png, &mut fm, chunk_data);
    }

    #[test]
    fn chunks_cm9n0g04() {

        let (mut png, mut fm) = start_file(r"tests/cm9n0g04.png");

        let chunk_data = vec![
            (13  , "IHDR", 0x93e1c829),
            (4   , "gAMA", 0x31e8965f),
            (7   , "tIME", 0x75301fe0),
            (200 , "IDAT", 0xf8ff896b),
            (0   , "IEND", 0xae426082)
        ];

        validate_chunks(&mut png, &mut fm, chunk_data);
    }

    #[test]
    fn chunks_exif2c08() {

        let (mut png, mut fm) = start_file(r"tests/exif2c08.png");

        let chunk_data = vec![
            (13  , "IHDR", 0xfc18eda3),
            (978 , "eXIf", 0xba88aa83),
            (741 , "IDAT", 0xe63d2b24),
            (0   , "IEND", 0xae426082)
        ];

        validate_chunks(&mut png, &mut fm, chunk_data);
    }

    #[test]
    fn ihdr_ps2n0g08() {

        let ( mut png, mut fm) = start_file(r"tests/ps2n0g08.png");

        let ihdr = PngIhdr {
            width: 32,
            height: 32,
            bit_depth: 8,
            color_type: 0,
            compression_method: 0,
            filter_method: 0,
            interlace_method: 0   
        };

        validate_ihdr(&mut png, &mut fm, 0, ihdr);
    }

    #[test]
    fn ihdr_s03n3p01() {

        let ( mut png, mut fm) = start_file(r"tests/s03n3p01.png");

        let ihdr = PngIhdr {
            width: 3,
            height: 3,
            bit_depth: 1,
            color_type: 3,
            compression_method: 0,
            filter_method: 0,
            interlace_method: 0   
        };

        validate_ihdr(&mut png, &mut fm, 0, ihdr);
    }

    #[test]
    fn ihdr_tbbn0g04() {

        let ( mut png, mut fm) = start_file(r"tests/tbbn0g04.png");

        let ihdr = PngIhdr {
            width: 32,
            height: 32,
            bit_depth: 4,
            color_type: 0,
            compression_method: 0,
            filter_method: 0,
            interlace_method: 0   
        };

        validate_ihdr(&mut png, &mut fm, 0, ihdr);
    }

    #[test]
    fn ihdr_ch1n3p04() {

        let ( mut png, mut fm) = start_file(r"tests/ch1n3p04.png");

        let ihdr = PngIhdr {
            width: 32,
            height: 32,
            bit_depth: 4,
            color_type: 3,
            compression_method: 0,
            filter_method: 0,
            interlace_method: 0   
        };

        validate_ihdr(&mut png, &mut fm, 0, ihdr);
    }

    #[test]
    fn ihdr_exif2c08() {

        let ( mut png, mut fm) = start_file(r"tests/exif2c08.png");

        let ihdr = PngIhdr {
            width: 32,
            height: 32,
            bit_depth: 8,
            color_type: 2,
            compression_method: 0,
            filter_method: 0,
            interlace_method: 0   
        };

        validate_ihdr(&mut png, &mut fm, 0, ihdr);
    }

    #[test]
    fn plte_s03n3p01() {

        let (mut png, mut fm) = start_file(r"tests/s03n3p01.png");

        let rgb = vec![
            (0, 255, 0),
            (255, 119, 0)
        ];

        validate_plte(&mut png, &mut fm, 3, rgb);
    }

    #[test]
    fn plte_ch1n3p04() {

        let (mut png, mut fm) = start_file(r"tests/ch1n3p04.png");

        let rgb = vec![
            (34, 0, 255),
            (0, 255, 255),
            (136, 0, 255),
            (34, 255, 0),
            (0, 153, 255),
            (255, 102, 0),
            (221, 0, 255),
            (119, 255, 0),
            (255, 0, 0),
            (0, 255, 153),
            (221, 255, 0),
            (255, 0, 187),
            (255, 187, 0),
            (0, 68, 255),
            (0, 255, 68),
        ];

        validate_plte(&mut png, &mut fm, 3, rgb);
    }

    #[test]
    fn trns_tbbn0g04() {

        let (mut png, mut fm) = start_file(r"tests/tbbn0g04.png");

        validate_trns(&mut png, &mut fm, 2, PngTrns::Gray(15));

    }

    #[test]
    fn gama_ps2n0g08() {

        let (mut png, mut fm) = start_file(r"tests/ps2n0g08.png");

        validate_gama(&mut png, &mut fm, 1, 100000);

    }

    #[test]
    fn gama_s03n3p01() {

        let (mut png, mut fm) = start_file(r"tests/s03n3p01.png");

        validate_gama(&mut png, &mut fm, 1, 100000);

    }

    #[test]
    fn gama_tbbn0g04() {

        let (mut png, mut fm) = start_file(r"tests/tbbn0g04.png");

        validate_gama(&mut png, &mut fm, 1, 100000);

    }

    #[test]
    fn gama_ch1n3p04() {

        let (mut png, mut fm) = start_file(r"tests/ch1n3p04.png");

        validate_gama(&mut png, &mut fm, 1, 100000);

    }

    #[test]
    fn gama_cm9n0g04() {

        let (mut png, mut fm) = start_file(r"tests/cm9n0g04.png");

        validate_gama(&mut png, &mut fm, 1, 100000);

    }

    #[test]
    fn sbit_s03n3p01() {

        let (mut png, mut fm) = start_file(r"tests/s03n3p01.png");

        let sbit = PngSbit::Rgb(4, 4, 4);

        validate_sbit(&mut png, &mut fm, 2, sbit);

    }

    #[test]
    fn sbit_ch1n3p04() {

        let (mut png, mut fm) = start_file(r"tests/ch1n3p04.png");

        let sbit = PngSbit::Rgb(4, 4, 4);

        validate_sbit(&mut png, &mut fm, 2, sbit);

    }

    #[test]
    fn bkgd_tbbn0g04() {

        let (mut png, mut fm) = start_file(r"tests/tbbn0g04.png");

        validate_bkgd(&mut png, &mut fm, 3, PngBkgd::Gray(0));

    }

    #[test]
    fn splt_ps2n0g08() {

        let ( mut png, mut fm) = start_file(r"tests/ps2n0g08.png");

        let mut rgbaf = vec![];

        for r in 0..6 {
            for g in 0..6 {
                for b in 0..6 {
                    rgbaf.push((r * 51, g * 51, b * 51, 255, 0));
                }
            }
        }

        validate_splt(&mut png, &mut fm, 2, "six-cube", 16, rgbaf);

    }

    #[test]
    fn hist_ch1n3p04() {

        let (mut png, mut fm) = start_file(r"tests/ch1n3p04.png");

        let freqs = vec![
            64, 112, 48, 96, 96, 32, 32,
            80, 16, 128, 64, 16, 48, 80,
            112, 
        ];

        validate_hist(&mut png, &mut fm, 4, freqs);

    }

    #[test]
    fn time_cm9n0g04() {

        let (mut png, mut fm) = start_file(r"tests/cm9n0g04.png");

        let time = PngTime {
            year: 1999,
            month: 12,
            day: 31,
            hour: 23,
            minute: 59,
            second: 59
        };

        validate_time(&mut png, &mut fm, 2, time);

    }

    #[test]
    fn exif_exif2c08() {

        let ( mut png, mut fm) = start_file(r"tests/exif2c08.png");

        let ifd0 = vec![
            ExifIfdEntry {tag: 0x0112, address: None, data: ExifIfdData::UShort(vec![1])},
            ExifIfdEntry {tag: 0x011a, address: Some(98), data: ExifIfdData::URat(vec![(72, 1)])},
            ExifIfdEntry {tag: 0x011b, address: Some(106), data: ExifIfdData::URat(vec![(72, 1)])},
            ExifIfdEntry {tag: 0x0128, address: None, data: ExifIfdData::UShort(vec![2])},
            ExifIfdEntry {tag: 0x0213, address: None, data: ExifIfdData::UShort(vec![1])},
            ExifIfdEntry {tag: 0x8298, address: Some(114), data: ExifIfdData::String("2017 Willem van Schaik\x00".to_string())},
            ExifIfdEntry {tag: 0x8769, address: None, data: ExifIfdData::ULong(vec![138])},
        ];

        let subifd = vec![
            ExifIfdEntry {tag: 0x9000, address: None, data: ExifIfdData::Undefined(vec![0x30, 0x32, 0x32, 0x30])},
            ExifIfdEntry {tag: 0x9101, address: None, data: ExifIfdData::Undefined(vec![0x01, 0x02, 0x03, 0x00])},
            ExifIfdEntry {tag: 0x9286, address: Some(204), data: ExifIfdData::Undefined(vec![
                0x41, 0x53, 0x43, 0x49, 0x49, 0x00, 0x00, 0x00,
                0x50, 0x6e, 0x67, 0x53, 0x75, 0x69, 0x74, 0x65
            ])},
            ExifIfdEntry {tag: 0xa000, address: None, data: ExifIfdData::Undefined(vec![0x30, 0x31, 0x30, 0x30])},
            ExifIfdEntry {tag: 0xa001, address: None, data: ExifIfdData::UShort(vec![65535])}
        ];

        let ifd1 = vec![
            ExifIfdEntry {tag: 0x0103, address: None, data: ExifIfdData::UShort(vec![6])},
            ExifIfdEntry {tag: 0x011a, address: Some(298), data: ExifIfdData::URat(vec![(72, 1)])},
            ExifIfdEntry {tag: 0x011b, address: Some(306), data: ExifIfdData::URat(vec![(72, 1)])},
            ExifIfdEntry {tag: 0x0128, address: None, data: ExifIfdData::UShort(vec![2])},
            ExifIfdEntry {tag: 0x0201, address: None, data: ExifIfdData::ULong(vec![314])},
            ExifIfdEntry {tag: 0x0202, address: None, data: ExifIfdData::ULong(vec![663])},
        ];

        validate_exif(&mut png, &mut fm, 1, vec![ifd0, ifd1], subifd);
    }
}

