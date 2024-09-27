use std::str::FromStr;
use std::str::CharIndices;

use bitutils2::{BitIndex, BitField, BinRegex};
use xmlparser::{Tokenizer, Token, ElementEnd, StrSpan};
use log::*;

pub struct BinaryField {
    pub name: String,
    pub value: Option<String>,
    pub start: usize,
    pub span: usize
}

pub trait FileSpec {
    fn new(fm: &mut crate::hex_edit::FileManager) -> Self where Self: Sized;

    fn field_at(self: &mut Self, index: usize, fm: &mut crate::hex_edit::FileManager) -> Option<BinaryField>;
}

pub struct PngFileSpec {
    chunks: Vec<usize>,
    chunk_types: Vec<[u8; 4]>
}

impl FileSpec for PngFileSpec {
    fn new(fm: &mut crate::hex_edit::FileManager) -> Self {
        let file_length = fm.len();
        let mut buffer_32 = vec![0; 4];
        let mut chunks = Vec::new();
        let mut chunk_types = Vec::new();
        let mut index = 8;
        while index + 4 < file_length {
            chunks.push(index);
            fm.get_bytes(index, &mut buffer_32);
            let chunk_length = u32::from_be_bytes([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]) + 12;
            fm.get_bytes(index + 4, &mut buffer_32);
            chunk_types.push([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]);
            index += chunk_length as usize;
        }
        
        PngFileSpec {
            chunks,
            chunk_types
        }
    }

    fn field_at(self: &mut Self, index: usize, fm: &mut crate::hex_edit::FileManager) -> Option<BinaryField> {
        if index < 8 {
            Some(
                BinaryField {
                    name: "PNG File Signature".to_string(),
                    value: None,
                    start: 0,
                    span: 8
                }
            )
        } else {
            let chunk = bisection::bisect_right(&self.chunks, &index) - 1;
            let chunk_index = self.chunks[chunk];
            let chunk_diff = self.chunks[chunk + 1] - chunk_index;
            if index >= chunk_index {
                match index - chunk_index {
                    0..=3 => {
                        let mut buffer_32 = vec![0; 4];
                        fm.get_bytes(chunk_index, &mut buffer_32);
                        let chunk_length = u32::from_be_bytes([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]);
                        Some(
                            BinaryField {
                                name: format!("Chunk #{} Length", chunk).to_string(),
                                value: Some(format!("{} Bytes", chunk_length).to_string()),
                                start: chunk_index,
                                span: 4
                            }
                        )
                    },
                    4..=7 => {
                        let mut buffer_32 = vec![0; 4];
                        fm.get_bytes(chunk_index + 4, &mut buffer_32);
                        let value = match String::from_utf8(buffer_32) {
                            Ok(s) => Some(s),
                            Err(_) => None
                        };
                        Some(
                            BinaryField {
                                name: format!("Chunk #{} Type", chunk).to_string(),
                                value,
                                start: chunk_index + 4,
                                span: 4
                            }
                        )
                    },
                    i if i + 4 >= chunk_diff => {
                        Some(
                            BinaryField {
                                name: format!("Chunk #{} CRC", chunk).to_string(),
                                value: None,
                                start: chunk_index + chunk_diff - 4,
                                span: 4
                            }
                        )
                    },
                    i => {
                        let data_start = chunk_index + 8;
                        let i = i - 8;
                        match self.chunk_types[chunk] {
                            [b'I', b'H', b'D', b'R'] => {
                                match i {
                                    0..=3 => {
                                        let mut buffer_32 = vec![0; 4];
                                        fm.get_bytes(data_start, &mut buffer_32);
                                        let value = u32::from_be_bytes([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]);
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Width").to_string(),
                                                value: Some(format!("{}", value).to_string()),
                                                start: data_start,
                                                span: 4
                                            }
                                        )
                                    },
                                    4..=7 => {
                                        let mut buffer_32 = vec![0; 4];
                                        fm.get_bytes(data_start + 4, &mut buffer_32);
                                        let value = u32::from_be_bytes([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]);
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Height").to_string(),
                                                value: Some(format!("{}", value).to_string()),
                                                start: data_start + 4,
                                                span: 4
                                            }
                                        )
                                    },
                                    8 => {
                                        let value = match fm.get_byte(data_start + 8) {
                                            Some(v) => Some(format!("{}", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Bit Depth").to_string(),
                                                value,
                                                start: data_start + 8,
                                                span: 1
                                            }
                                        )
                                    },
                                    9 => {
                                        let value = match fm.get_byte(data_start + 9) {
                                            Some(0) => Some(format!("{} (Grayscale)", 0).to_string()),
                                            Some(2) => Some(format!("{} (Truecolor)", 2).to_string()),
                                            Some(3) => Some(format!("{} (Indexed)", 3).to_string()),
                                            Some(4) => Some(format!("{} (Grayscale and Alpha)", 4).to_string()),
                                            Some(6) => Some(format!("{} (Truecolor and Alpha)", 6).to_string()),
                                            Some(v) => Some(format!("{} (Invalid)", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Color Type").to_string(),
                                                value,
                                                start: data_start + 9,
                                                span: 1
                                            }
                                        )
                                    },
                                    10 => {
                                        let value = match fm.get_byte(data_start + 10) {
                                            Some(0) => Some(format!("{}", 0).to_string()),
                                            Some(v) => Some(format!("{} (Invalid)", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Compression Method").to_string(),
                                                value,
                                                start: data_start + 10,
                                                span: 1
                                            }
                                        )
                                    },
                                    11 => {
                                        let value = match fm.get_byte(data_start + 11) {
                                            Some(0) => Some(format!("{}", 0).to_string()),
                                            Some(v) => Some(format!("{} (Invalid)", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Filter Method").to_string(),
                                                value,
                                                start: data_start + 11,
                                                span: 1
                                            }
                                        )
                                    },
                                    12 => {
                                        let value = match fm.get_byte(data_start + 12) {
                                            Some(0) => Some(format!("{} (No Interlace)", 0).to_string()),
                                            Some(1) => Some(format!("{} (Adam7 Interlace)", 1).to_string()),
                                            Some(v) => Some(format!("{} (Invalid)", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Interlace Method").to_string(),
                                                value,
                                                start: data_start + 12,
                                                span: 1
                                            }
                                        )
                                    },
                                    _ => None
                                }
                            },
                            _ => None
                        }
                    }
                }
            } else {
                panic!("Unexpected condition: {}, {}, {}", index, chunk_index, chunk)
            }
        }
    }
}



#[derive(Clone, Debug, PartialEq)]
pub struct MyStrSpan<'a> {
    start: usize,
    text: &'a str
}

impl<'a> MyStrSpan<'a> {
    fn new(start: usize, text: &'a str) -> MyStrSpan<'a> {
        MyStrSpan {
            start, text
        }
    }

    fn from_range(input: &'a MyStrSpan, rng: std::ops::Range<usize>) -> MyStrSpan<'a> {
        input.slice(rng.start, rng.end)
    }

    /// Constructs a new `StrSpan` from substring.
    pub fn from_substr(text: &str, start: usize, end: usize) -> MyStrSpan {
        debug_assert!(start <= end);
        MyStrSpan { text: &text[start..end], start }
    }

    /// Returns `true` is self is empty.
    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    /// Returns the start position of the span.
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the end position of the span.
    pub fn end(&self) -> usize {
        self.start + self.text.len()
    }

    pub fn as_str(&self) -> &'a str {
        &self.text
    }

    pub fn range(&self) -> std::ops::Range<usize> {
        std::ops::Range {
            start: self.start,
            end: self.end()
        }
    }

    pub fn chars(&self) -> StrSpanChars {
        StrSpanChars::new(self)
    }

    fn slice(&self, start: usize, stop: usize) -> MyStrSpan<'a> {
        MyStrSpan {
            start,
            text: &self.text[(start - self.start)..(stop - self.start)]
        }
    }

    fn trim(&self) -> MyStrSpan<'a> {
        let new_text = self.text.trim_start();
        MyStrSpan {
            start: self.start + self.text.len() - new_text.len(),
            text: new_text.trim_end()
        }
    }
}

pub struct StrSpanChars<'a> {
    start: usize,
    text: &'a str,
    iter: CharIndices<'a>
}

impl<'a> Iterator for StrSpanChars<'a> {
    type Item = MyStrSpan<'a>;

    fn next(&mut self) -> Option<MyStrSpan<'a>> {
        match self.iter.next() {
            None => None,
            Some((i, ch)) => {
                let n = ch.to_string().bytes().len();
                let s = &self.text[i..i+n];//self.text.split_at(i).1.split_at(n).0;
                Some(MyStrSpan::new(self.start + i, s))
            }
        }
    }

    #[inline]
    fn count(self) -> usize {
        self.iter.count()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }

    // #[inline]
    // fn last(mut self) -> Option<(usize, char)> {
    //     self.iter.next_back().map(|ch| {
    //         let index = self.front_offset + self.iter.iter.len();
    //         (index, ch)
    //     })
    // }

}

impl<'a> StrSpanChars<'a> {
    fn new(sspan: &MyStrSpan<'a>) -> StrSpanChars<'a> {
        StrSpanChars {
            start: sspan.start,
            text: sspan.text,
            iter: sspan.text.char_indices()
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ExprOp {
    Add,
    Sub,
    Mult,
    Div,
    Neg,
    Eq,
    Le,
    Or,
    List,
    Function(String)
}

impl ExprOp {
    fn priority(&self) -> u8 {
        match self {
            ExprOp::Function(_) => 0,
            ExprOp::Mult => 1,
            ExprOp::Div => 1,
            ExprOp::Neg => 0,
            ExprOp::Add => 2,
            ExprOp::Sub => 2,
            ExprOp::Eq => 3,
            ExprOp::Le => 3,
            ExprOp::Or => 4,
            ExprOp::List => 5,
            _ => todo!()
        }
    }

    // Returns true if it is valid for the operator to appear before the
    // first operand (e.g. '+' in '+x + y' or '-' in '-x - y'). Returns
    // false otherwise (e.g. '*x * y' would be invalid)
    fn is_prefix(&self) -> bool {
        match self {
            ExprOp::Neg | ExprOp::Add => true,
            _ => false
        }
    }

    // Returns true if it is valid for the operator to appear after the
    // last operand (e.g. factorial). Returns false otherwise (e.g. 
    // 'x * y*' would be invalid)
    fn is_postfix(&self) -> bool {
        match self {
            _ => false
        }
    }

    // Returns true if it is valid for the operator to not appear before
    // the first operand (e.g. not negation). This is not guaranteed to be
    // the opposite of is_prefix (e.g. '+' is valid in both prefix and 
    // non-prefix)
    fn is_nonprefix(&self) -> bool {
        match self {
            ExprOp::Neg => false,
            _ => true
        }
    }

    // Returns true if it is valid for the operator to not appear after
    // the last operand (e.g. not factorial). This is not guaranteed to be
    // the opposite of is_prefix (e.g. '+' is valid in both prefix and 
    // non-prefix)
    fn is_nonpostfix(&self) -> bool {
        match self {
            _ => true
        }
    }

    // Returns true if it is valid for the operator to appear multiple times
    // without an operand in between (e.g. List if a member is not included).
    fn is_consecutive(&self) -> bool {
        match self {
            ExprOp::List => true,
            _ => false
        }
    }

    // Returns true if it is valid for the operator to have more than one
    // operand (e.g. '+' in 'x + y+ z').
    fn is_stringable(&self) -> bool {
        match self {
            _ => true
        }
    }

    fn apply(&self, args: Vec<ExprValue>) -> Result<ExprValue, ()> {
        match self {
            ExprOp::Add => {
                let mut args_iter = args.iter();
                let init = args_iter.next().unwrap().clone();
                Ok(args_iter.fold(init, |a, b| (&a + b)))
            },
            ExprOp::Sub => {
                let mut args_iter = args.iter();
                let init = args_iter.next().unwrap().clone();
                Ok(args_iter.fold(init, |a, b| (&a - b)))
            },
            ExprOp::Mult => {
                let mut args_iter = args.iter();
                let init = args_iter.next().unwrap().clone();
                Ok(args_iter.fold(init, |a, b| (&a * b)))
                // todo!()
            },
            ExprOp::Div => {
                // let mut args_iter = args.iter();
                // let init = args_iter.next().unwrap().clone();
                // Ok(args_iter.fold(init, |a, b| (a / b)))
                todo!()
            },
            ExprOp::Neg => {
                let mut args_iter = args.iter();
                let init = args_iter.next().unwrap().clone();
                Ok(args_iter.fold(-init, |a, b| (&a - b)))
            },
            ExprOp::Eq => {
                let mut args_iter = args.iter();
                let init = args_iter.next().unwrap().clone();
                for arg in args_iter {
                    if &init != *arg {
                        return Ok(ExprValue::Bool(false));
                    }
                }
                Ok(ExprValue::Bool(true))
            },
            ExprOp::Le => {
                let mut args_iter = args.iter();
                let mut prev = args_iter.next().unwrap().clone();
                for arg in args_iter {
                    if &prev > arg {
                        return Ok(ExprValue::Bool(false));
                    } else {
                        prev = arg.clone();
                    }
                }
                Ok(ExprValue::Bool(true))
            },
            ExprOp::Or => {
                for arg in args {
                    match arg {
                        ExprValue::Bool(b) => {
                            if b {
                                return Ok(ExprValue::Bool(true))
                            }
                        },
                        _ => panic!("Incorrect type supplied for 'or' operation. Expected bool")
                    }
                }
                return Ok(ExprValue::Bool(false))
            },
            ExprOp::Function(fname) => {
                match fname.as_str() {
                    "bytes" => {
                        if args.len() != 1 {
                            panic!("Wrong number of arguments supplied")
                        } 
                        match args[0] {
                            ExprValue::Integer(n) => Ok(ExprValue::Position(BitIndex::from_i64_bytes(n))),
                            _ => panic!("Incorrect type supplied to 'bytes'. Expected integer")
                        }
                    },
                    _ => panic!("Function name not recognized: {}", fname.as_str())
                }
            }
            _ => todo!()
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprValue {
    Integer(i64),
    Position(BitIndex),
    Bool(bool),
    String(String),
    Bytes(BitField)
}

impl ExprValue {
    pub fn datatype_as_string(&self) -> String {
        match self {
            ExprValue::Integer(_) => "Integer".to_string(),
            ExprValue::Position(_) => "Position".to_string(),
            ExprValue::Bool(_) => "Boolean".to_string(),
            ExprValue::String(_) => "String".to_string(),
            ExprValue::Bytes(_) => "Bytes".to_string()
        }
    }

    pub fn expect_integer(self) -> Result<i64, ExprEvalError> {
        match self {
            ExprValue::Integer(value) => Ok(value),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "Integer".to_string()})
        }
    }

    pub fn expect_position(self) -> Result<BitIndex, ExprEvalError> {
        match self {
            ExprValue::Position(value) => Ok(value),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "Position".to_string()})
        }
    }

    pub fn expect_bool(self) -> Result<bool, ExprEvalError> {
        match self {
            ExprValue::Bool(value) => Ok(value),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "Boolean".to_string()})
        }
    }

    pub fn expect_string(self) -> Result<String, ExprEvalError> {
        match self {
            ExprValue::String(value) => Ok(value),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "String".to_string()})
        }
    }
}

impl std::cmp::PartialEq<ExprValue> for &ExprValue {
    fn eq(&self, rhs: &ExprValue) -> bool {
        match (self, rhs) {
            (ExprValue::Integer(left), ExprValue::Integer(right)) => {
                *left == *right
            },
            (ExprValue::Position(left), ExprValue::Position(right)) => {
                *left == *right
            },
            (ExprValue::Bool(left), ExprValue::Bool(right)) => {
                *left == *right
            },
            (ExprValue::String(left), ExprValue::String(right)) => {
                *left == *right
            },
            (ExprValue::Bytes(left), ExprValue::Bytes(right)) => {
                *left == *right
            },
            _ => todo!()
        }
    }
}

impl std::cmp::PartialOrd for ExprValue {
    fn partial_cmp(&self, other: &ExprValue) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for ExprValue {
    fn cmp(&self, other: &ExprValue) -> std::cmp::Ordering {
        match (self, other) {
            (ExprValue::Integer(left), ExprValue::Integer(right)) => {
                std::cmp::Ord::cmp(left, right)
            },
            (ExprValue::Position(left), ExprValue::Position(right)) => {
                std::cmp::Ord::cmp(left, right)
            },
            _ => todo!()
        }
    }
}

impl std::ops::Neg for ExprValue {
    type Output = ExprValue;
    fn neg(self) -> Self::Output {
        match self {
            ExprValue::Integer(v) => {
                ExprValue::Integer(-v)
            },
            ExprValue::Position(v) => {
                ExprValue::Position(-v)
            },
            ExprValue::Bool(v) => {
                ExprValue::Bool(!v)
            },
            _ => todo!(),
        }
    }
}

impl std::ops::Add<ExprValue> for ExprValue {
    type Output = ExprValue;
    fn add(self, rhs: ExprValue) -> Self::Output {
        match (self, rhs) {
            (ExprValue::Integer(left), ExprValue::Integer(right)) => {
                ExprValue::Integer(left + right)
            },
            (ExprValue::Position(left), ExprValue::Position(right)) => {
                ExprValue::Position(left + right)
            },
            _ => todo!()
        }
    }
}

impl std::ops::Sub<ExprValue> for ExprValue {
    type Output = ExprValue;
    fn sub(self, rhs: ExprValue) -> Self::Output {
        match (self, rhs) {
            (ExprValue::Integer(left), ExprValue::Integer(right)) => {
                ExprValue::Integer(left - right)
            },
            (ExprValue::Position(left), ExprValue::Position(right)) => {
                ExprValue::Position(left - right)
            },
            _ => todo!()
        }
    }
}

impl std::ops::Mul<ExprValue> for ExprValue {
    type Output = ExprValue;
    fn mul(self, rhs: ExprValue) -> Self::Output {
        match (self, rhs) {
            (ExprValue::Integer(left), ExprValue::Integer(right)) => {
                ExprValue::Integer(left * right)
            },
            (ExprValue::Position(left), ExprValue::Integer(right)) => {
                ExprValue::Position(left * (right as i128))
            },
            (ExprValue::Integer(left), ExprValue::Position(right)) => {
                ExprValue::Position(right * (left as i128))
            },
            _ => todo!()
        }
    }
}

impl<'a, 'b> std::ops::Add<&'a ExprValue> for &'b ExprValue {
    type Output = ExprValue;
    fn add(self, rhs: &'a ExprValue) -> Self::Output {
        self.clone() + rhs.clone()
        
    }
}

impl<'a, 'b> std::ops::Sub<&'a ExprValue> for &'b ExprValue {
    type Output = ExprValue;
    fn sub(self, rhs: &'a ExprValue) -> Self::Output {
        self.clone() - rhs.clone()
        
    }
}

impl<'a, 'b> std::ops::Mul<&'a ExprValue> for &'b ExprValue {
    type Output = ExprValue;
    fn mul(self, rhs: &'a ExprValue) -> Self::Output {
        self.clone() * rhs.clone()
        
    }
}

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

#[derive(Debug)]
pub struct ParseExprError {
    details: String,
    annotations: Vec<(Option<String>, std::ops::Range<usize>)>,
    help: Option<String>
}

impl ParseExprError {
    pub fn new(range: std::ops::Range<usize>, details: String) -> ParseExprError {
        ParseExprError{details, annotations: vec![(None, range)], help: None}
    }

    pub fn set_help(&mut self, text: String) {
        self.help = Some(text);
    }

    pub fn make_message(&self, context: &str) -> String {
        let mut msg = format!("\x1b[0;31mERROR:\x1b[0m {}", self.details).to_string();
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
        msg
    }

}

impl std::fmt::Display for ParseExprError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.details)
    }    
}

#[derive(Debug)]
pub enum ExprToken {
    Var(String, std::ops::Range<usize>),
    Delimited(std::ops::Range<usize>),
    Operator(ExprOp, std::ops::Range<usize>),
    Parsed(Expr, std::ops::Range<usize>)
}

impl ExprToken {
    fn range(&self) -> std::ops::Range<usize> {
        match self {
            ExprToken::Var(_, r) => r.clone(),
            ExprToken::Delimited(r) => r.clone(),
            ExprToken::Operator(_, r) => r.clone(),
            ExprToken::Parsed(_, r) => r.clone()
        }
    }
}

impl ExprToken {

    /// Converts a string span that does not contain delimiters into a token stream
    fn from_undelimited_sspan(mut sspan: MyStrSpan) -> Result<Vec<ExprToken>, ParseExprError> {
        info!("Parsing section: {}", sspan.as_str());

        // Vector used to accumulate tokens
        let mut tokens = Vec::<ExprToken>::new();

        // Continue trying to match tokens until there is nothing left
        while !sspan.as_str().is_empty() {

            let (token, match_end_index) = if let Some(m) = EXPR_VAR_RE.find(sspan.as_str()) {

                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                info!("Var token: {}", m.as_str());
                (ExprToken::Var(m.as_str().to_string(), token_sspan.range()), m.end())

            } else if let Some(m) = EXPR_ARG_RE.find(sspan.as_str()) {

                info!("Arg token: {}", m.as_str());
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                match usize::from_str(&m.as_str()[1..]) {
                    Ok(i) => {
                        (ExprToken::Parsed(Expr::Arg(i), token_sspan.range()), m.end())
                    },
                    Err(err) => {
                        let error_details = format!("Error while parsing argument index: {}", err).to_string();
                        return Err(ParseExprError::new(token_sspan.range(), error_details))
                    }
                }

            } else if let Some(m) = EXPR_BP_RE.find(sspan.as_str()) {

                info!("BitIndex token: {}", m.as_str());
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                (ExprToken::Parsed(Expr::Value(ExprValue::Position(BitIndex::from_str(m.as_str()).unwrap())), token_sspan.range()), m.end())

            } else if let Some(m) = EXPR_FLOAT_RE.find(sspan.as_str()) {
                info!("Float token: {}", m.as_str());

                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                todo!();
                //(ExprToken::Parsed(Expr::Value(ExprValue::Float(f64::from_str(m.as_str()))), token_sspan.range()), m.end());

            } else if let Some(m) = EXPR_INT_RE.find(sspan.as_str()) {

                info!("Int token: {}", m.as_str());
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                (ExprToken::Parsed(Expr::Value(ExprValue::Integer(i64::from_str(m.as_str()).unwrap())), token_sspan.range()), m.end())
                
            } else if let Some(m) = EXPR_OP_RE.find(sspan.as_str()) {

                info!("Op token: {}", m.as_str());
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                let op = match m.as_str() {
                    "+" => ExprOp::Add,
                    "-" => {
                        if tokens.is_empty() || matches!(tokens[tokens.len() - 1], ExprToken::Operator(_, _)) {
                            ExprOp::Neg
                        } else {
                            ExprOp::Sub
                        }
                    },
                    "*" => ExprOp::Mult,
                    "/" => ExprOp::Div,
                    "==" => ExprOp::Eq,
                    "<=" => ExprOp::Le,
                    "||" => ExprOp::Or,
                    "," => ExprOp::List,
                    _ => {
                        let error_details = format!("Unrecognized operator: {}", token_sspan.as_str()).to_string();
                        return Err(ParseExprError::new(token_sspan.range(), error_details))
                    }
                };
                (ExprToken::Operator(op, token_sspan.range()), m.end())

            } else {

                let error_details = format!("Unrecognized token: {}", sspan.as_str()).to_string();
                return Err(ParseExprError::new(sspan.range(), error_details))

            };

            tokens.push(token);
            sspan = sspan.slice(sspan.start() + match_end_index, sspan.end()).trim();
        }

        Ok(tokens)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Expr {
    Op{op: ExprOp, args: Vec<Expr>},
    Value(ExprValue),
    Var(String),
    Arg(usize),
    Empty
}

impl Expr {
    fn evaluate(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<ExprValue, ExprEvalError> {
        self.evaluate_with_args(&vec![], lookup)
    }

    fn evaluate_with_args(&self, arguments: &Vec<ExprValue>, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<ExprValue, ExprEvalError> {
        match self {
            Expr::Op{op, args} => {
                let mut eval_args = Vec::new();
                for arg in args {
                    eval_args.push(arg.evaluate_with_args(arguments, lookup)?)
                }
                Ok(op.apply(eval_args).unwrap())
                
            },
            Expr::Value(v) => Ok(v.clone()),
            Expr::Var(s) => match lookup(s) {
                Some(result) => Ok(result),
                None => Err(ExprEvalError::LookupError{key: s.clone()})
            },
            Expr::Arg(n) => {
                if *n < arguments.len() {
                    Ok(arguments[*n].clone())
                } else {
                    Err(ExprEvalError::ArgumentCountError{accessed: *n, provided: arguments.len()})
                }
            },
            Expr::Empty => todo!()
        }
    }

    fn evaluate_expect_position(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BitIndex, ExprEvalError> {
        match self.evaluate(lookup)? {
            ExprValue::Position(bp) => Ok(bp),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "Position".to_string()})
        }
    }

    fn evaluate_expect_integer(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<i64, ExprEvalError> {
        self.evaluate(lookup)?.expect_integer()
    }

    fn evaluate_expect_bool(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<bool, ExprEvalError> {
        match self.evaluate(lookup)? {
            ExprValue::Bool(b) => Ok(b),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "Bool".to_string()})
        }
    }

    fn vars(&self) -> HashSet<String> {
        match self {
            Expr::Op{op, args} => {
                let mut args_iter = args.iter();
                let init = args_iter.next().unwrap().clone();
                args_iter.fold(init.vars(), |mut a, b| {a.extend(b.vars()); a})
            },
            Expr::Value(_) => HashSet::new(),
            Expr::Var(s) => {
                let mut vars = HashSet::new();
                vars.insert(s.to_string());
                vars
            },
            Expr::Arg(_) | Expr::Empty => HashSet::new(),
        }
    }

    fn from_str_span(input: MyStrSpan) -> Result<Expr, ParseExprError> {
        let tokens = Expr::tokens_from_str_span(&input)?;
        Expr::from_tokens(&input, tokens)
    }

    // Parses a token stream from tokens_from_str_span into an Expr object
    fn from_tokens(input: &MyStrSpan, mut tokens: Vec<ExprToken>) -> Result<Expr, ParseExprError> {
        info!("Parsing Tokens: {:?}", tokens);

        if tokens.len() == 0 {
            // If there are no tokens, then just return Empty. This could happen in
            // a situation like a negation operation, where the operator is not preceded
            // by any tokens.
            return Ok(Expr::Empty);

        } else if tokens.len() == 1 {
            // If there is just one token, then how it gets converted depends on what 
            // kind of token it is.
            match tokens.pop().unwrap() {

                ExprToken::Delimited(rng) => {
                    // Delimited tokens denote spans of input that were within delimiters
                    // during the original token conversion. This should be converted into
                    // an expression using from_str_span.
                    let sspan = MyStrSpan::from_range(input, rng);
                    return Expr::from_str_span(sspan)
                },
                ExprToken::Operator(_, rng) => {
                    // An operator should never be alone in a token stream. The only way 
                    // that this would happen is if an operator is by itself inside 
                    // delimiters or the entire input stream is just a single operator.
                    let err = ParseExprError::new(rng.clone(), format!("Unexpected operator encountered: {}", input.slice(rng.start, rng.end).as_str()).to_string());
                    return Err(err)
                },
                ExprToken::Parsed(expr, _) => {
                    // If the token is already a parsed expression, just return the expression
                    return Ok(expr)
                },
                ExprToken::Var(s, _) => {
                    // If the token is a variable literal, return a variable expression
                    return Ok(Expr::Var(s))
                }
            }

        } else if tokens.len() == 2 {
            // If there are two tokens in the stream, then it could be a function since
            // a function is represented by a variable followed by a parenthetical expression.
            // This checks for that case, and otherwise recomposes the tokens vector to allow 
            // the normal multi-token processing to occur.

            let token_1 = tokens.pop().unwrap();
            let token_0 = tokens.pop().unwrap();
            match (token_0, token_1) {
                (ExprToken::Var(s, _), ExprToken::Delimited(rng)) => {
                    // If the two tokens are a variable followed by a parenthetical,
                    // then it represents a function call. Check if the content of
                    // the parenthetical is a list, and if so decompose it into multiple
                    // arguments (e.g. atan2(x, y)). If not, supply it as a single argument
                    // (e.g. cos(x)).
                    let op = ExprOp::Function(s);
                    let sspan = MyStrSpan::from_range(input, rng);
                    let function_args = Expr::from_str_span(sspan)?;
                    let args = match function_args {
                        Expr::Op{op, args} if matches!(op, ExprOp::List) => {
                            args
                        },
                        function_args => vec![function_args]
                    };
                    return Ok(Expr::Op{op, args})
                },
                (token_0, token_1) => {
                    // If the two tokens do not represent a function call, recompose the
                    // token vector to allow for normal processing to occur.
                    tokens = vec![token_0, token_1];
                }
            }
        }

       
        // This block loops through the tokens and determines if there is an operator in 
        // the stream (op_present) and finds the highest priority of all of the operators
        // it encounters (op_priority). This is used in later processing to ensure that
        // lower priority operators are not processed until after the token stream has been
        // split into sections divided by the highest priority operators.
        let mut op_priority = 0;
        let mut op_present = false;
        for token in &tokens {
            if let ExprToken::Operator(op, _) = token {
                op_priority = std::cmp::max(op_priority, op.priority());
                op_present = true;
            }
        }

        // If there are no operators present in the stream, raise an error. The only way this
        // would be valid would be in the one- and two-token cases checked for above.
        if !op_present {
            let range_start = tokens[0].range().start;
            let range_end = tokens[tokens.len() - 1].range().end;
            let error_range = range_start..range_end;
            return Err(ParseExprError::new(error_range, "Expression does not contain operator".to_string()))
        }

        // == Declarations for variables to be used when iterating over the token stream ==

        // current_group is used to accumulate tokens between occurrences of the highest 
        // priority operators so that it can be processed as a separate stream.
        let mut current_group = Vec::<ExprToken>::new();

        // current_op is used to keep track of the most recent highest-priority operator
        // encountered. This is important in the case of multiple operators with the same
        // priority. This is initialized to None and will be changed to Some once the 
        // operator is encountered in the following iteration.
        let mut current_op: Option<ExprOp> = None;

        // expr_args is used to accumulate the expressions that are generated by parsing 
        // the contents of current_group. Once the current operator is terminated (either
        // by end of input or by enountering another operator of equal priority), this 
        // vector is used to populate the "args" paramter of that expr.
        let mut expr_args = Vec::<Expr>::new();

        // last_op_pos is used to keep track of the position (range) of the most recently
        // encountered instance of the current operator (current_op). This is used exclusively
        // to help generate error messages.
        let mut last_op_pos: Option<std::ops::Range<usize>> = None;

        // =================================================================================

        // Essentially what's happening here is that we're iterating over all of the tokens and
        // looking for whatever operator has a priority that matches the highest priority in the
        // stream (op_priority). Once the first operator with that priority is found, it's saved 
        // as current_op and everything that was already iterated over is collected and parsed 
        // recursively as an independent token stream to get the first argument for that current_op.
        // From there, the iteration continues until another occurrence of an operator with that
        // priority is found. If it matches current_op, then we repeat the process of collecting all
        // the tokens that were iterated over and parsing them to get another argument. This continues
        // until either the end of the stream, or an operator of equal priority that is not current_op
        // is encountered. If that happens, collect the remaining tokens and parse them to get the
        // last argument for current_op. To complicate things, the iteration I just described actually
        // needs to happen backwards in order to because the standard order of operations for same-
        // priority operators is left-to-right, so the tree must be generated right-to-left.

        // The token iterator must be created explicitly and iterated over using a while loop
        // instead of a for loop because there is a condition that will terminate the iteration
        // and collect the remaining elements into a vector. This is easier when the iterator
        // is accessible from within the loop. Iteration must be reveresed per description above.
        let mut token_iter = tokens.into_iter().rev();

        while let Some(token) = token_iter.next() {

            match &current_op {
                None => {
                    // If a high priority operator has not yet been encountered (current_op is None),
                    // then check if this token meets the criteria to become current_op.
                    match token {

                        ExprToken::Operator(op, rng) if op.priority() == op_priority => {
                            // If the token is a high priority operator, then we've got our operator 
                            // for this iteration. 

                            // Check for a couple error conditions. If there aren't actually any tokens in the
                            // current_group, then the operator is in a postfix position. If that's the case, check
                            // to make sure that a postfix position is valid for the operator. If current_group 
                            // isn't empty, check to make sure that it's okay for the operator to not be in a 
                            // postfix position.
                            if current_group.is_empty() {
                                if !op.is_postfix() {
                                    let err = ParseExprError::new(rng, "Non-postfix operator encountered in postfix location".to_string());
                                    return Err(err)
                                } else {
                                    // expr_args.push(Expr::Empty);
                                }
                            } else if !op.is_nonpostfix() {
                                // Untested case
                                let err = ParseExprError::new(rng, "Postfix operator encountered in non-postfix location".to_string());
                                return Err(err)
                            }

                            // Set last_op_pos and current_op to indicate that the operator has been found
                            last_op_pos = Some(rng);
                            current_op = Some(op);

                            // Collect everything that's been iterated over already (which has been 
                            // accumulated into current_group) and parse that token stream to get the 
                            // first argument (well, actually last since everything is happenening in
                            // reverse) for current_op. The token stream needs to be reversed to account 
                            // for the fact that we're collecting the tokens backwards.
                            let expr_arg = Expr::from_tokens(input, current_group.into_iter().rev().collect())?;
                            expr_args.push(expr_arg);

                            // Reset current_group in preparation for accumulating tokens for the 
                            // next argument.
                            current_group = vec![];

                        },
                        token => {
                            // If the token is not the operator we're looking for, just add it to the accumulator
                            // so it can contribute to the first argument
                            current_group.push(token);
                        }
                    }
                },
                Some(current_op) => {
                    // If a high priority operator has already been encountered, then check if the token is either a 
                    // re-occurrence of that operator or a new operator with the same priority.
                    match &token {
                        ExprToken::Operator(op, rng) if op == current_op => {
                            // If the token is a re-occurrence of the operator we've already encountered, then do similar
                            // processing to that done when the first occurrence was encountered. Check for error conditions,
                            // parse the current_group to get another argument, and update last_op_pos.

                            // Check for a couple error conditions. If there aren't acutally any tokens in current_group,
                            // the the operator must have occurred twice in a row with nothing in between. If that's the
                            // case, check to make sure it's valid for that operator. If current_group isn't empty, then
                            // we've got at least two occurrences of the operator with an operand in between. Check to make
                            // sure that's valid for the operator (operator is stringable)
                            if current_group.is_empty() {
                                if !op.is_consecutive() {
                                    let err = ParseExprError::new(rng.clone(), "Non-consecutive operator encountered in consecutive location".to_string());
                                    return Err(err)
                                } else {
                                    // expr_args.push(Expr::Empty);
                                }
                            } else if !op.is_stringable() {
                                // Untested case
                                let err = ParseExprError::new(rng.clone(), "Non-stringable operator encountered in stringable location".to_string());
                                return Err(err)
                            }

                            // If the token is a re-occurrence of the operator we've already encountered, then parse all
                            // of the tokens accumulated since the last occurrence and add it to the list of arguments.
                            // Once again, the token stream needs to be reversed since we're iterating in reverse.
                            let expr_arg = Expr::from_tokens(input, current_group.into_iter().rev().collect())?;
                            expr_args.push(expr_arg);

                            // Reset current_group in preparation for accumulating tokens for the 
                            // next argument.
                            current_group = vec![];

                            // Update the last_op_pos to point to this occurrence
                            last_op_pos = Some(rng.clone());

                        },
                        ExprToken::Operator(op, _) if op.priority() == op_priority => {
                            // If this token is another operator of the same priority, then whatever remains of the token
                            // stream is going to be our last argument since that operator is going to be the highest priority
                            // for that part of the streak. Push the current token followed by the remainder of the input tokens
                            // to the current group and break out of the loop (the loop will terminate after this step anyway
                            // since the iterator will be consumed. The break statement is mostly just here for clarity.)
                            current_group.push(token);
                            current_group.extend(token_iter.collect::<Vec<ExprToken>>());
                            break;
                        },
                        _ => {
                            // If the token isn't a high priority operator, just add it to the accumulator so it can contribute
                            // to the next argument.
                            current_group.push(token);
                        }
                    
                    } 
                }
            }
        }

        // Now that the iteration is finished, unwrap the current_op. This is safe because prior processing confirmed that an
        // operator was present, and so current_op will have been converted to Some in the previous while loop.
        let current_op = current_op.unwrap();

        // Check for a couple error conditions. 
        if current_group.is_empty() {
            // If there aren't any tokens accumulated since the last occurrence of current_op, then the operator is in a prefix 
            // position. Check to make sure that's valid for the operator.
            if !current_op.is_prefix() {
                let err = ParseExprError::new(last_op_pos.unwrap(), "Non-prefix operator encountered in prefix location".to_string());
                return Err(err);
            } else {
                // expr_args.push(Expr::Empty);
            }
            
        } else if !current_op.is_nonprefix() {
            // If current_group isn't empty, then the operator is in a nonprefix position. If that's not valid for the operator,
            // raise an error.

            // Untested case
            let err = ParseExprError::new(last_op_pos.unwrap(), "Prefix operator encountered in non-prefix location".to_string());
            return Err(err)
        } else {
            // Otherwise, just parse the contents of current_group (reversed since it was accumulated in reverse) and add that as the
            // last argument for current_op.
            expr_args.push(Expr::from_tokens(input, current_group.into_iter().rev().collect())?);
        }

        // Reverse the order of the arguments that have been accumulated since they were accumulated in a reversed iterator and return 
        // the resulting expression
        expr_args = expr_args.into_iter().rev().collect();
        Ok(Expr::Op{op: current_op, args: expr_args})
    }

    /// Converts a string span into a vector of tokens, which is intended to be processed
    /// by the from_tokens method.
    fn tokens_from_str_span(input: &MyStrSpan) -> Result<Vec<ExprToken>, ParseExprError> {
        info!("Input: {}", input.as_str());

        let mut delimiter_stack = Vec::<MyStrSpan>::new();
        let mut in_string_literal = false;
        let mut current_string_literal = String::new();
        let mut escaped = false;
        let mut tokens = Vec::<ExprToken>::new();
        let mut current_token_start = input.start();
        // let mut was_token_match = false;

        let mut chars_iter = input.chars();

        while let Some(ch) = chars_iter.next() {
            let ch_str = ch.as_str();

            if in_string_literal {
                // If the current character is inside a string literal, then there's a need to keep track
                // of what's escaped so that the end delimiter (') can be processed appropriately. 
                if ch_str == "\\" {
                    // If a backslash is encountered, then invert the escaped flag. This enables processing 
                    // many backslashes in a row. 
                    escaped = !escaped
                } else if !escaped {
                    // If a closing delimiter (') is encountered, and is not escaped, then the string literal
                    // has been closed.
                    if ch_str == "'" {
                        let string_literal_sspan = input.slice(current_token_start, ch.start());
                        info!("String literal token: '{}'", string_literal_sspan.as_str());
                        tokens.push(ExprToken::Parsed(Expr::Value(ExprValue::String(current_string_literal)), string_literal_sspan.range()));
                        current_string_literal = String::new();
                        current_token_start = ch.end();
                        in_string_literal = false;
                    } else {
                        // Any other non-escaped characters get pushed onto the literal
                        current_string_literal.push_str(ch_str);
                    }
                } else {
                    // If a character that is escaped is encountered, push it onto the literal without 
                    // processing it for any other meaning and reset the escaped flag. The only valid 
                    // escape is for the closing delimiter ('), so there's a check to generate an error
                    // if anything else is escaped. Escaped backslashes are handled in an earlier check.
                    escaped = false;
                    current_string_literal.push_str(ch_str);
                    if ch_str != "'" {
                        let mut err = ParseExprError::new(ch.range(), format!("Invalid escaped character: {}", ch_str).to_string());
                        err.set_help("Try escaping the preceding backslash".to_string());
                        return Err(err)
                    }
                }
            } else {
                match ch_str {
                    "'" => {
                        // If the current character is an opening string delimiter ('), process the
                        // current span for tokens, set the in_string_literal flag to indicate that the
                        // following span is within a string literal, and update the current_token_start
                        // to the start of the string literal.
                        let mut undelimited_sspan = input.slice(current_token_start, ch.start()).trim();
                        tokens.extend(ExprToken::from_undelimited_sspan(undelimited_sspan)?);
                        in_string_literal = true;
                        current_token_start = ch.end();
                    },
                    "(" | "[" | "{" => {
                        // If the current character is a non-string opening delimiter, check if we are at the
                        // top level (previous span is not within delimiters) by checking if the delimiter stack
                        // is empty. If so, process the current span for tokens. and update the current_token_start
                        // to the start of the contents of the delimiter. Add the opening delimiter to the stack
                        // regardless.
                        if delimiter_stack.is_empty() {
                            let mut undelimited_sspan = input.slice(current_token_start, ch.start()).trim();
                            tokens.extend(ExprToken::from_undelimited_sspan(undelimited_sspan)?);
                            current_token_start = ch.end();
                        }
                        delimiter_stack.push(ch)
                    },
                    ")" | "]" | "}" => {
                        // If the current character is a closing delimiter, try removing the last item on the 
                        // delimiter stack, which should be the most recent opening delimiter. If the stack is
                        // empty (pop returns None), then the closing delimiter does not pair with an opening 
                        // delimiter and is invalid. Otherwise, if the opening delimiter from the stack matches
                        // the closing delimiter, then the delimiter is valid. In that case, if the closed delimiter
                        // is at the top level (the stack is empty after popping) then add a delimited token to
                        // the stream with the contents since the opening delimiter and update current_token_start.
                        // If the opening delimiter from the stack doesn't match the closing delimiter, then we 
                        // have mismatched delimiters.
                        match (ch_str, delimiter_stack.pop().map(|d| d.as_str())) {
                            (")", Some("(")) | ("]", Some("[")) | ("}", Some("{")) => {
                                if delimiter_stack.is_empty() {
                                    let token_sspan = input.slice(current_token_start, ch.start());
                                    info!("Delimited: '{}'", token_sspan.as_str());
                                    tokens.push(ExprToken::Delimited(token_sspan.range()));
                                    current_token_start = ch.end();
                                }
                            },
                            (_, None) => return Err(ParseExprError::new(current_token_start..ch.end(), format!("Encountered unexpected '{}'", ch_str).to_string())),
                            (_, Some(d)) => return Err(ParseExprError::new(current_token_start..ch.end(), format!("Mismatched delimiters: {}, {}", d, ch_str).to_string()))
                        }
                    },
                    _ => {
                        // Do nothing
                    }
    
                }
            }

        }

        if in_string_literal {
            return Err(ParseExprError::new(current_token_start..input.end(), "Encountered end of stream while parsing string literal".to_string()));
        }

        if delimiter_stack.is_empty() {
            let mut undelimited_sspan = input.slice(current_token_start, input.end()).trim();
            tokens.extend(ExprToken::from_undelimited_sspan(undelimited_sspan)?);
        } else {
            let d = delimiter_stack.pop().unwrap();
            return Err(ParseExprError::new(d.range(), format!("Unclosed delimiter: {}", d.as_str()).to_string()));
        }
        info!("Tokens: {:?}", tokens);
        Ok(tokens)
    }
}

impl FromStr for Expr {
    type Err = ParseExprError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Expr::from_str_span(MyStrSpan::new(0, s))
    }
}

#[cfg(test)]
mod expr_parse_tests {
    use super::*;
    #[test]
    fn expr_parse_tests() {
        let add_arg1 = Expr::Op{op: ExprOp::Add, args: vec![Expr::Var("x".to_string()), Expr::Value(ExprValue::Integer(5))]}; // x + 5
        assert_eq!(Expr::from_str("x + 5").unwrap(), add_arg1);
        let mul_arg1 = Expr::Op{op: ExprOp::Mult, args: vec![Expr::Value(ExprValue::Integer(3)), Expr::Var("x".to_string())]}; // 3 * x
        let mul_arg2 = Expr::Op{op: ExprOp::Mult, args: vec![Expr::Value(ExprValue::Integer(5)), Expr::Arg(12), add_arg1]}; // 5 * $12 * (x + 5)
        let add_arg2 = Expr::Op{op: ExprOp::Add, args: vec![mul_arg1, mul_arg2]}; // 3 * x + 5 * $12 * (x + 5)
        let eq_arg1 = Expr::Op{op: ExprOp::Eq, args: vec![add_arg2.clone(), Expr::Value(ExprValue::String("hello?".to_string()))]};
        assert_eq!(Expr::from_str("3 * x + 5 * $12 * (x + 5) == 'hello?'").unwrap(), eq_arg1);
        let eq_arg2 = Expr::Op{op: ExprOp::Eq, args: vec![add_arg2.clone(), Expr::Value(ExprValue::String("escape'test".to_string()))]};
        assert_eq!(Expr::from_str(r"3 * x + 5 * $12 * (x + 5) == 'escape\'test'").unwrap(), eq_arg2);
        let bp1 = Expr::Value(ExprValue::Position(BitIndex::new(5, 3)));
        assert_eq!(Expr::from_str("5B3b").unwrap(), bp1);
        let atan2 = Expr::Op{op: ExprOp::Function("atan2".to_string()), args: vec![add_arg2, Expr::Var("y".to_string())]};
        assert_eq!(Expr::from_str("atan2(3 * x + 5 * $12 * (x + 5), y)").unwrap(), atan2);
        let sub_arg1 = Expr::Op{op: ExprOp::Sub, args: vec![
            Expr::Op{op: ExprOp::Neg, args: vec![Expr::Var("x".to_string())]}, 
            Expr::Value(ExprValue::Integer(5)), 
            Expr::Op{op: ExprOp::Neg, args: vec![Expr::Arg(10)]}
            ]}; // -x - 5 - -$10
        assert_eq!(Expr::from_str("-x - 5 - -$10").unwrap(), sub_arg1);
    }

    #[test]
    fn order_of_operations() {
        let mul_arg1 = Expr::Op{op: ExprOp::Mult, args: vec![Expr::Value(ExprValue::Integer(5)), Expr::Value(ExprValue::Integer(3))]}; // 5 * 3
        let div_arg1 = Expr::Op{op: ExprOp::Div, args: vec![mul_arg1, Expr::Var("x".to_string())]}; // 5 * 3 / x
        let mul_arg2 = Expr::Op{op: ExprOp::Mult, args: vec![div_arg1, Expr::Value(ExprValue::Integer(2))]}; // 5 * 3 / x * 2
        // println!("Message: {}", Expr::from_str("5 * 3 / x * 2").unwrap_err().make_message("5 * 3 / x * 2"));
        assert_eq!(Expr::from_str("5 * 3 / x * 2").unwrap(), mul_arg2.clone());
        let mul_arg3 = Expr::Op{op: ExprOp::Mult, args: vec![Expr::Value(ExprValue::Integer(3)), Expr::Value(ExprValue::Integer(2))]}; // 3 * 2
        let div_arg2 = Expr::Op{op: ExprOp::Div, args: vec![mul_arg3, Expr::Op{op: ExprOp::Neg, args: vec![Expr::Var("x".to_string())]}]}; // 3 * 2 / -x
        let mul_arg4 = Expr::Op{op: ExprOp::Mult, args: vec![div_arg2, Expr::Value(ExprValue::Integer(6))]}; // 3 * 2 / -x * 6
        assert_eq!(Expr::from_str("3 * 2 / -x * 6").unwrap(), mul_arg4.clone());
        let add_arg1 = Expr::Op{op: ExprOp::Add, args: vec![mul_arg2, mul_arg4, Expr::Op{op: ExprOp::Neg, args: vec![Expr::Var("y".to_string())]}]}; // 5 * 3 / x * 2 + 3 * 2 / -x * 6 + -y
        assert_eq!(Expr::from_str("5 * 3 / x * 2 + 3 * 2 / -x * 6 + -y").unwrap(), add_arg1.clone());
        let sub_arg1 = Expr::Op{op: ExprOp::Sub, args: vec![add_arg1, Expr::Value(ExprValue::Integer(5))]}; // 5 * 3 / x * 2 + 3 * 2 / -x * 6 + -y - 5
        assert_eq!(Expr::from_str("5 * 3 / x * 2 + 3 * 2 / -x * 6 + -y - 5").unwrap(), sub_arg1.clone());
        let add_arg2 = Expr::Op{op: ExprOp::Add, args: vec![sub_arg1, Expr::Op{op: ExprOp::Neg, args:vec![Expr::Var("z".to_string())]}]}; // 5 * 3 / x * 2 + 3 * 2 / -x * 6 + -y - 5 + -z
        assert_eq!(Expr::from_str("5 * 3 / x * 2 + 3 * 2 / -x * 6 + -y - 5 + -z").unwrap(), add_arg2);
    }

    #[test]
    fn error_tests() {

        let input = "x + (* x * y)".to_string();
        let err = Expr::from_str(&input).unwrap_err();
        assert_eq!(err.details, "Non-prefix operator encountered in prefix location".to_string());
        assert_eq!(err.annotations, vec![(None, 5..6)]);

        let input = "x + (x * y *)".to_string();
        let err = Expr::from_str(&input).unwrap_err();
        assert_eq!(err.details, "Non-postfix operator encountered in postfix location".to_string());
        assert_eq!(err.annotations, vec![(None, 11..12)]);

        let input = "x + (x * y) +".to_string();
        let err = Expr::from_str(&input).unwrap_err();
        assert_eq!(err.details, "Non-postfix operator encountered in postfix location".to_string());
        assert_eq!(err.annotations, vec![(None, 12..13)]);

        let input = "x + (x * * y)".to_string();
        let err = Expr::from_str(&input).unwrap_err();
        assert_eq!(err.details, "Non-consecutive operator encountered in consecutive location".to_string());
        assert_eq!(err.annotations, vec![(None, 7..8)]);
        // let input = r"+x + (4 - 5) + 1 + 'hello'".to_string();
        // let err = Expr::from_str(&input).unwrap_err();
        // panic!("{}", err.make_message(&input));
    }
}

#[derive(Debug)]
pub enum ExprEvalError {
    LookupError{key: String},
    ArgumentCountError{accessed: usize, provided: usize},
    DataTypeMismatch{found: String, expected: String},
    ParseError{details: String}
}

impl std::fmt::Display for ExprEvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprEvalError::LookupError{key} => write!(f, "Lookup of '{}' failed", key),
            ExprEvalError::ArgumentCountError{accessed, provided} => write!(f, "Expression attempted to use argument #{} but only {} were provided.", accessed, provided),
            ExprEvalError::DataTypeMismatch{found, expected} => write!(f, "Data type mismatch: '{}' found, '{}' expeced", found, expected),
            ExprEvalError::ParseError{details} => write!(f, "Parse error: {}", details)
        }
        
    }    
}

impl From<ParseAbstractIdentError> for ExprEvalError {
    fn from(err: ParseAbstractIdentError) -> Self {
        ExprEvalError::ParseError{
            details: err.details
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

#[derive(Debug)]
pub struct ParseXmlError {
    details: String
}

impl From<ParseExprError> for ParseXmlError {
    fn from(err: ParseExprError) -> Self {
        ParseXmlError{
            details: format!("{}", err).to_string()
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
    offset: Expr,
    length: Expr,
    alignment: Expr,
    alignment_base: Expr,
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

    fn parse(&self, position: BitIndex, fm: &mut crate::hex_edit::FileManager, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BinaryField, ExprEvalError> {
        let alignment_base = self.alignment_base.evaluate_expect_position(lookup)?;
        let alignment = self.alignment.evaluate_expect_position(lookup)?;
        let length = self.length.evaluate_expect_position(lookup)?;
        let offset = self.offset.evaluate_expect_position(lookup)?;

        let remainder = (&position + &offset - alignment_base).rem_euclid(&alignment);
        let position = position + offset + remainder;


        todo!()
    }

    fn get_position(&self, initial_position: BitIndex, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BitIndex, ExprEvalError> {
        let alignment_base = self.alignment_base.evaluate_expect_position(lookup)?;
        let alignment = self.alignment.evaluate_expect_position(lookup)?;
        let length = self.length.evaluate_expect_position(lookup)?;
        let position = initial_position + self.offset.evaluate_expect_position(lookup)?;

        info!("alignment_base={:?}, alignment={:?} length={:?}, position={:?}", alignment_base, alignment, length, position);

        info!("diff: {:?}", &position - &alignment_base);

        let remainder = (&position - &alignment_base).rem_euclid(&alignment);

        info!("remainder: {:?}", remainder);
        if remainder.is_zero() {
            Ok(position)
        } else {
            Ok(position + alignment - remainder)
        }
    }

    fn get_length(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BitIndex, ExprEvalError> {
        self.length.evaluate_expect_position(lookup)
    }

    // A dependency on the section start or end of previous field is implicit
    fn start_dependencies(&self) -> HashSet<String> {
        let mut deps = self.offset.vars();
        deps.extend(self.alignment.vars());
        deps.extend(self.alignment_base.vars());
        deps
    }

    // A dependency on the section start or end of previous field is implicit
    fn end_dependencies(&self) -> HashSet<String> {
        let mut deps = self.start_dependencies();
        deps.extend(self.length.vars());
        deps
    }

    fn from_xml_object(mut obj: XmlObject) -> Result<FieldSpec, ParseXmlError> {
        if obj.element.as_str() != "field" {
            panic!("Trying to parse field from non-field element")
        }
        let length = Expr::from_str(&convert_xml_symbols(obj.attrs.remove("length").unwrap().as_str()))?;
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
            Some(s) => Expr::from_str(&convert_xml_symbols(s.as_str()))?,
            None => Expr::Value(ExprValue::Position(BitIndex::zero()))
        };
        let alignment = match obj.attrs.remove("align") {
            Some(s) => Expr::from_str(&convert_xml_symbols(s.as_str()))?,
            None => Expr::Value(ExprValue::Position(BitIndex::bytes(1)))
        };
        let alignment_base = match obj.attrs.remove("align-base") {
            Some(s) => Expr::from_str(&convert_xml_symbols(s.as_str()))?,
            None => Expr::Value(ExprValue::Position(BitIndex::zero()))
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

struct PositionedSectionSpec<'a> {
    parent: &'a SectionSpec,
    position: BitIndex, // Position == start since alignment is already done
    resolved_length: BitIndex,
    field_offsets: Vec<BitIndex>,
    field_lengths: Vec<BitIndex>
}

impl<'a> PositionedSectionSpec<'a> {
    pub fn new(position: BitIndex, parent: &'a SectionSpec) -> PositionedSectionSpec {
        let n_fields = parent.fields.len();
        PositionedSectionSpec {
            parent,
            position,
            resolved_length: BitIndex::new(0, 0),
            field_offsets: Vec::new(),
            field_lengths: Vec::new()
        }
    }

    fn get_position(&self) -> BitIndex { 
        self.position.clone()
    }

    fn try_get_position(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        return Ok(DependencyReport::success(ExprValue::Position(self.position.clone())))

    }

    fn contains_value(&self, key: String) -> bool {
        self.parent.contains_value(key)
    }

}

#[derive(Clone)]
struct SectionSpec {
    length: LengthPolicy,
    section_type: SectionType,
    fields: Vec<FieldSpec>,
    id_map: HashMap<String, usize>, // Map of ID to index in fields array
}

impl SectionSpec {



    fn length_id(&self) -> String {
        todo!()
    }

    fn try_get_field(&self, key: String) -> Result<DependencyReport<FieldSpec>, ExprEvalError> {
        todo!("Trying to get field {}", key);
    }

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

    /// Returns the actual start position of the structure given the "desired" position. This
    /// primarily is intended to account for alignment.
    fn try_get_start(&self, position: BitIndex, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<BitIndex>, ExprEvalError> {
        todo!()
    }

    fn contains_value(&self, key: String) -> bool {
        self.id_map.contains_key(&key)
    }

    fn try_get_length(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<BitIndex>, ExprEvalError> {
        match &self.length {
            LengthPolicy::Expr(length) => match length.evaluate_expect_position(lookup) {
                Ok(length) => Ok(Some(length)),
                Err(ExprEvalError::LookupError{..}) => Ok(None),
                Err(err) => Err(err)
            },
            LengthPolicy::FitContents => todo!(),
            LengthPolicy::Expand => todo!()
        }
    }

    fn try_get_end(&self, position: BitIndex, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<BitIndex>, ExprEvalError> {
        match self.try_get_start(position, lookup)? {
            Some(start) => match self.try_get_length(lookup)? {
                Some(length) => Ok(Some(start + length)),
                None => Ok(None)
            },
            None => Ok(None)
        }
    }

}

#[cfg(test)]
mod struct_tests {
    use super::*;

    fn lookup1(s: &str) -> Option<ExprValue> {
        match s {
            "var1" => Some(ExprValue::Position(BitIndex::new(4, 0))),
            "var2" => Some(ExprValue::Position(BitIndex::new(2, 0))),
            "var3" => Some(ExprValue::Position(BitIndex::new(8, 0))),
            _ => None
        }
    }

    // #[test]
    // fn simple() {
    //     let f1 = FieldSpec {
    //         id: "f1".to_string(),
    //         name: "Field 1".to_string(),
    //         offset: Expr::Value(ExprValue::Position(BitIndex::new(0, 0))),
    //         length: Expr::Value(ExprValue::Position(BitIndex::new(4, 0))),
    //         alignment: Expr::Value(ExprValue::Position(BitIndex::new(1, 0))),
    //         alignment_base: Expr::Value(ExprValue::Position(BitIndex::new(0, 0))),
    //         dtype: "u32".to_string(),
    //         endian: Endianness::Big
    //     };

    //     let f2 = FieldSpec {
    //         id: "f2".to_string(),
    //         name: "Field 2".to_string(),
    //         offset: Expr::Value(ExprValue::Position(BitIndex::new(0, 0))),
    //         length: Expr::Var("var1".to_string()),
    //         alignment: Expr::Value(ExprValue::Position(BitIndex::new(1, 0))),
    //         alignment_base: Expr::Value(ExprValue::Position(BitIndex::new(0, 0))),
    //         dtype: "u32".to_string(),
    //         endian: Endianness::Big
    //     };

    //     let f3 = FieldSpec {
    //         id: "f3".to_string(),
    //         name: "Field 3".to_string(),
    //         offset: Expr::Var("var2".to_string()),
    //         length: Expr::Var("var3".to_string()),
    //         alignment: Expr::Value(ExprValue::Position(BitIndex::new(1, 0))),
    //         alignment_base: Expr::Value(ExprValue::Position(BitIndex::new(0, 0))),
    //         dtype: "u64".to_string(),
    //         endian: Endianness::Big
    //     };

    //     let fields = vec![f1, f2, f3];

    //     let mut id_map = HashMap::new();
    //     id_map.insert("f1".to_string(), 0);
    //     id_map.insert("f2".to_string(), 1);
    //     id_map.insert("f3".to_string(), 2);

    //     let ss = Rc::new(SectionSpec {
    //         length: LengthPolicy::FitContents,
    //         section_type: SectionType::Body,
    //         fields,
    //         id_map
    //     });

    //     let mut section = PositionedSectionSpec::new(BitIndex::new(10, 0),&ss);

    //     // assert_eq!(section.try_resolve(&lookup1).unwrap(), 3);
    //     // println!("{:?}", section.field_offsets);
    //     // assert_eq!(section.try_get_field_address("f1").unwrap(), (BitIndex::new(10, 0), BitIndex::new(4, 0)));
    //     // assert_eq!(section.try_get_field_address("f2").unwrap(), (BitIndex::new(14, 0), BitIndex::new(4, 0)));
    //     // assert_eq!(section.try_get_field_address("f3").unwrap(), (BitIndex::new(20, 0), BitIndex::new(8, 0)));
    // }
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

enum ExprOrOther<T> {
    Expr(Expr),
    Other(T)
}

use lazy_static::lazy_static;
lazy_static! {
    static ref KEY_RE: regex::Regex = regex::Regex::new(r"^(?<structure>\w+)\.(?<attribute>\w+)(?<index>[\d+])?$").unwrap();
    static ref FIELD_KEY_RE: regex::Regex = regex::Regex::new(r"^(?<structure>\w+):(?<field>\w+)(\.(?<attribute>\w+))?$").unwrap();

    static ref EXPR_ARG_RE: regex::Regex = regex::Regex::new(r"^\$[0-9]+").unwrap();
    static ref EXPR_VAR_RE: regex::Regex = regex::Regex::new(r"^[_a-zA-Z][_a-zA-Z0-9\.]*").unwrap();
    static ref EXPR_INT_RE: regex::Regex = regex::Regex::new(r"^[0-9]+").unwrap();
    static ref EXPR_BP_RE: regex::Regex = regex::Regex::new(r"[\-\+]?(([0-9]+B)?[0-9]+b)|([0-9]+B)").unwrap();
    static ref EXPR_FLOAT_RE: regex::Regex = regex::Regex::new(r"^[0-9]*((\.[0-9]*)|((\.[0-9]*)?([eE][\-\+]?[0-9]+)))").unwrap();
    static ref EXPR_OP_RE: regex::Regex = regex::Regex::new(r"^[^0-9a-zA-Z_ \$]+").unwrap();

    static ref STRUCT_ATTR_RE: regex::Regex = regex::Regex::new(r"^(?<structure>\w+)\.(?<attribute>\w+)(?<index>[\d+])?$").unwrap();
    static ref FIELD_ATTR_RE: regex::Regex = regex::Regex::new(r"^(?<structure>\w+):(?<field>\w+)(\.(?<attribute>\w+))?$").unwrap();
}

#[derive(Clone)]
enum LengthPolicy {
    FitContents,
    Expand,
    Expr(Expr)
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
    Addressed{position: Rc<Expr>, pss: Rc<RefCell<PartiallyResolvedStructure<'a>>>},
    Sequence(Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>),
    Switch{value: Rc<Expr>, cases: Vec<(Rc<Expr>, Rc<Structure>)>, default: Rc<Structure>, pss: Option<Rc<RefCell<PartiallyResolvedStructure<'a>>>>},
    Repeat{n: Rc<Expr>, structure: Rc<Structure>, seq: Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>, finalized: bool}, // seq is used to store the actual structure instances once "n" is determined
    RepeatUntil{end: Rc<Expr>, structure: Rc<Structure>, seq: Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>, finalized: bool},
    Chain{next: Rc<Expr>, structure: Rc<Structure>, prs: Rc<RefCell<PartiallyResolvedStructure<'a>>>, seq: Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>, finalized: bool}
}

struct PartiallyResolvedStructure<'a> {
    id: StructureIdent,
    original: Rc<Structure>,
    parent_id: StructureIdent,
    alignment: Expr,
    alignment_base: Expr,
    // start: Option<BitIndex>,
    length: LengthPolicy,
    // end: Option<BitIndex>,
    stype: Box<PartiallyResolvedStructureType<'a>>,
    import_monikers: HashMap<String, Option<StructureIdent>>,
    exports: HashSet<String>,
    inherited_monikers: HashMap<String, FieldIdent>,
    index_path: Vec<usize>
}

impl<'a> PartiallyResolvedStructure<'a> {

    fn new(original: Rc<Structure>, parent_id: StructureIdent, 
        prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Rc<RefCell<PartiallyResolvedStructure<'a>>> {
        PartiallyResolvedStructure::new_indexed(original, parent_id, vec![], prs_map)
    }

    fn new_indexed(original: Rc<Structure>, parent_id: StructureIdent, index: Vec<usize>, prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Rc<RefCell<PartiallyResolvedStructure<'a>>> {
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
            StructureType::Addressed{position, content} => {
                for name in content.exports.keys() {
                    import_monikers.insert(name.clone(), None);
                }
                let mut child_index = index.clone();
                child_index.push(0);
                let pss = PartiallyResolvedStructure::new_indexed(content.clone(), parent_id.clone(), child_index, prs_map);
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
                PartiallyResolvedStructureType::Switch{value: value.clone(), cases: cases.iter().map(|(e, c)| (e.clone(), c.clone())).collect(), default: default.clone(), pss: None}
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
                let structure = Structure::wrap_addressed(structure.clone(), Expr::Value(ExprValue::Bool(false))); // The "address" put in here 
                // must never be used, hence why it's a boolean
                PartiallyResolvedStructureType::Chain{next: next.clone(), structure: Rc::new(structure), prs, seq: Vec::new(), finalized: false}
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
            index_path: index
        }));

        prs_map.insert(id.clone(), prs.clone());
        prs
    }

    fn monikers_for_child(&self) -> HashMap<String, FieldIdent> {
        let mut child_monikers = self.inherited_monikers.clone();
        for key in self.import_monikers.keys().clone() {
            if !child_monikers.contains_key(key) {
                child_monikers.insert(key.to_string(), self.id.get_field_ident(key.clone()));
            }
        }
        child_monikers
    }

    fn initialize_inherited_monikers(&mut self, prs_map: &HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>,
            monikers: HashMap<String, FieldIdent>) {
        
        self.inherited_monikers = monikers;
        for key in self.original.def_fields.keys() {
            self.inherited_monikers.insert(key.clone(), self.id.clone().get_field_ident(key.clone()));
        }
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
                        new.insert(AbstractIdent::from_str(&s)?);
                    }
                }
            }
        }
        Ok(new)
    }

    fn try_get_region_at(&self, vd: &ValueDictionary, location: BitIndex) -> Result<DependencyReport<FileRegion>, ExprEvalError> {
        // info!("Trying to get region for {:?}", self.id);
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
            PartiallyResolvedStructureType::Switch{pss, ..} => {
                if let Some(pss) = pss {
                    let dr = pss.borrow().try_get_region_at(vd, location)?;
                    match dr.result {
                        DepResult::DoesNotExist => {
                            // Location is between the end of the switch contents and the end of the switch... It's a spare.
                            let pss_end_id = pss.borrow().id.clone().get_attr_ident(StructureAttrType::End);
                            parents.insert(pss_end_id.clone().to_abstract());
                            let pss_end = match vd.lookup_struct_attr(&pss_end_id) {
                                Some(ev) => ev.expect_position()?,
                                None => return Ok(DependencyReport::incomplete(parents))
                            };
                            return Ok(DependencyReport::success(FileRegion::Spare(self.id.clone(), pss_end..end)));
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
            }
        }   
    }

    fn try_get_field(&self, field_id: FieldIdent, vd: &ValueDictionary,
            prs_map: &HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<DataSource>, ExprEvalError> {
        info!("Import monikers for {:?}: {:?}", self.id, self.import_monikers);
        if self.import_monikers.contains_key(&field_id.id) {
            match &self.import_monikers[&field_id.id] {
                Some(source_id) => {
                    // Grab value from the import source
                    info!("Trying to import {} from {:?}", field_id, source_id);
                    let source = prs_map.get(source_id).unwrap();
                    let new_field_id = source_id.get_field_ident(field_id.id);
                    source.borrow().try_get_field(new_field_id, vd, prs_map)
                },
                None => {
                    let dr = self.try_resolve_import(field_id.id.clone())?;
                    match dr.result {
                        DepResult::Success(source_id) => {
                            info!("Successfully resolved import");
                            let source = prs_map.get(&source_id).unwrap();
                            let new_field_id = source_id.get_field_ident(field_id.id);
                            source.borrow().try_get_field(new_field_id, vd, prs_map)
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
                                let lookup = self.get_lookup_fn(vd);
                                match default.evaluate(&lookup) {
                                    Ok(value) => {
                                        let mut dr = DependencyReport::success(DataSource::Given(value));
                                        dr.add_pc_pairs(self.convert_expr_vars(default.vars())?, HashSet::from([field_id.to_abstract()]));
                                        Ok(dr)
                                    },
                                    Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(default.vars())?)),
                                    Err(err) => Err(err)
                                }
                            } else {
                                Ok(DependencyReport::does_not_exist())
                            }
                            
                        }
                        // _ => todo!()
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
                            Err(()) => panic!("Structure did not contain the field!!!")
                        }
                        
        
                    },
                    _ => panic!("Cannot get a field for a non-section structure!")
                }
            }
        } else {
            panic!("Field {} does not exist!", field_id);

        }
    }

    fn get_lookup_fn<'b, 'c>(&'c self, vd: &'b ValueDictionary) -> impl Fn(&str) -> Option<ExprValue> + 'b {
        // TODO: MAKE THIS MORE EFFICIENT!!!
        let monikers = self.inherited_monikers.clone();
        let import_monikers = self.import_monikers.clone();
        let self_id = self.id.clone();
        // info!("Looking for {} in {}", key, self.id);
        info!("Monikers for {:?}: {:?}", self.id, monikers);
        info!("Imports for {:?}: {:?}", self.id, import_monikers);
        let lookup = move |alias: &str| match monikers.get(alias) {
            Some(fi) => vd.lookup_field(fi),
            None => {
                if import_monikers.contains_key(alias) {
                    let fi = self_id.clone().get_field_ident(alias.to_string());
                    vd.lookup_field(&fi)
                } else {
                    vd.lookup_str(alias).unwrap_or(None)
                }
                
            }
        };
        lookup
    }

    fn try_lookup(&mut self, key: AbstractIdent, vd: &ValueDictionary, prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        // let monikers = self.inherited_monikers.clone();
        info!("Looking for {} in {}", key, self.id);
        // info!("Monikers: {:?}", monikers);
        // let lookup = move |alias: &str| match monikers.get(alias) {
        //     Some(fi) => vd.lookup_field(fi),
        //     None => {
        //         vd.lookup_str(alias).unwrap_or(None)
        //     }
        // };
        let lookup = self.get_lookup_fn(vd);
        // TODO: replace this with a match
        let mut result = match key {
            AbstractIdent::StructureAttr(ref sai) => {
                if sai.structure == self.id {
                    match sai.attr {
                        StructureAttrType::Position => self.try_get_position(&lookup),
                        StructureAttrType::Align => self.try_get_alignment(&lookup),
                        StructureAttrType::AlignBase => self.try_get_alignment_base(&lookup),
                        StructureAttrType::StartPad => self.try_get_start_pad(vd),
                        StructureAttrType::Start => self.try_get_start(vd),
                        StructureAttrType::Length => self.try_get_length(vd, &lookup),
                        StructureAttrType::SpareLength => self.try_get_spare_length(vd),
                        StructureAttrType::ContentsLength => self.try_get_contents_length(vd, &lookup),
                        StructureAttrType::End => self.try_get_end(vd, &lookup),
                        StructureAttrType::SwitchValue => self.try_get_switch_value(&lookup),
                        StructureAttrType::SwitchIndex => self.try_get_switch_index(vd, prs_map),
                        StructureAttrType::Repetitions => self.try_get_repetitions(vd, &lookup, prs_map),
                        StructureAttrType::SwitchCase(i) => self.try_get_switch_case(i, vd, &lookup),
                        StructureAttrType::Break(i) => self.try_get_break(i, vd, &lookup)
                    }
                } else {
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
                        match fai.attr {
                            FieldAttrType::Start => self.try_get_field_start(fai.field.clone(), vd),
                            FieldAttrType::StartPad => self.try_get_field_start_pad(fai.field.clone(), vd),
                            FieldAttrType::Length => self.try_get_field_length(fai.field.clone(), &lookup),
                            FieldAttrType::End => self.try_get_field_end(fai.field.clone(), vd),
                            FieldAttrType::Offset => self.try_get_field_offset(fai.field.clone(), &lookup),
                            FieldAttrType::Position => self.try_get_field_position(fai.field.clone(), vd),
                            FieldAttrType::Align => self.try_get_field_alignment(fai.field.clone(), &lookup),
                            FieldAttrType::AlignBase => self.try_get_field_alignment_base(fai.field.clone(), &lookup),
                            FieldAttrType::SearchStart => self.try_get_def_search_start(fai.field.clone(), &lookup),
                            FieldAttrType::SearchN => self.try_get_def_search_n(fai.field.clone(), &lookup),
                            FieldAttrType::AnchorOffset => self.try_get_def_anchor_offset(fai.field.clone(), &lookup),
                        }
                    }
                } else {
                    info!("\t Searching in children");
                    self.try_lookup_in_children(key, vd, prs_map)
                }
            },
            AbstractIdent::Field(_) => panic!("Field lookups not supported")
        }?;

        for (parents, children) in result.parents_children.iter_mut() {
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

    fn try_get_field_position(&self, field_id: FieldIdent, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                let field_index = match initial.id_map.get(&field_id.id) {
                    Some(i) => i,
                    None => return Ok(DependencyReport::does_not_exist())
                };

                let field = &initial.fields[*field_index];

                let offset_id = field_id.clone().get_attr_ident(FieldAttrType::Offset);
                let mut parents = HashSet::from([offset_id.clone().to_abstract()]);

                let pre_offset = if *field_index == 0 {
                    // First field in the structure, must be positioned starting from the structure start
                    let structure_start_id = self.id.get_attr_ident(StructureAttrType::Start);
                    parents.insert(structure_start_id.clone().to_abstract());
                    let structure_start = match vd.lookup_struct_attr(&structure_start_id) {
                        Some(ev) => ev.expect_position()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };
                    structure_start
                } else {
                    // Field is positioned starting from previous field's end
                    let prev_field = self.id.clone().get_field_ident(initial.fields[*field_index - 1].id.clone());
                    let previous_end_id = prev_field.clone().get_attr_ident(FieldAttrType::End);
                    parents.insert(previous_end_id.clone().to_abstract());
                    let previous_end = match vd.lookup_field_attr(&previous_end_id) {
                        Some(ev) => ev.expect_position()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };
                    previous_end
                };

                let offset = match vd.lookup_field_attr(&offset_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                let mut dr = DependencyReport::success(ExprValue::Position(pre_offset + offset));
                let field_position_id = field_id.get_attr_ident(FieldAttrType::Position).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([field_position_id]));
                Ok(dr)

            },
            _ => {
                todo!()
            }
        }       
    }

    fn try_get_field_start(&self, field_id: FieldIdent, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {

                let start_pad_id = field_id.clone().get_attr_ident(FieldAttrType::StartPad);
                let position_id = field_id.clone().get_attr_ident(FieldAttrType::Position);

                let parents = HashSet::from([start_pad_id.clone().to_abstract(), position_id.clone().to_abstract()]);

                let start_pad = match vd.lookup_field_attr(&start_pad_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
        
                let position = match vd.lookup_field_attr(&position_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
        
                let mut dr = DependencyReport::success(ExprValue::Position(position + start_pad));
                let start_id = field_id.clone().get_attr_ident(FieldAttrType::Start).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([start_id]));
                Ok(dr)

            },
            _ => {
                todo!()
            }
        }       
    }

    fn try_get_field_start_pad(&self, field_id: FieldIdent, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, ExprEvalError> {

        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {

                let alignment_id = field_id.clone().get_attr_ident(FieldAttrType::Align);
                let alignment_base_id = field_id.clone().get_attr_ident(FieldAttrType::AlignBase);
                let position_id = field_id.clone().get_attr_ident(FieldAttrType::Position);

                let parents = HashSet::from([alignment_id.clone().to_abstract(), alignment_base_id.clone().to_abstract(),
                                            position_id.clone().to_abstract()]);


                let alignment = match vd.lookup_field_attr(&alignment_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
        
                let alignment_base = match vd.lookup_field_attr(&alignment_base_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
        
                let position = match vd.lookup_field_attr(&position_id) {
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
                let start_pad_id = field_id.clone().get_attr_ident(FieldAttrType::StartPad).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([start_pad_id]));
                Ok(dr)

            },
            _ => {
                todo!()
            }
        } 
    }

    fn try_get_field_length(&self, field_id: FieldIdent, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                let field_index = match initial.id_map.get(&field_id.id) {
                    Some(i) => i,
                    None => return Ok(DependencyReport::does_not_exist())
                };
                let field = &initial.fields[*field_index];
                match field.length.evaluate_expect_position(lookup) {
                    Ok(length) => {
                        info!("Length for {}: {:?}", field_id.id, length);
                        let mut dr = DependencyReport::success(ExprValue::Position(length));
                        let field_length_id = field_id.clone().get_attr_ident(FieldAttrType::Length).to_abstract();
                        dr.add_pc_pairs(self.convert_expr_vars(field.length.vars())?, HashSet::from([field_length_id]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => {
                        Ok(DependencyReport::incomplete(self.convert_expr_vars(field.length.vars())?))
                    }
                    Err(err) => return Err(err)
                }
            },
            _ => {
                todo!("Trying to get field length for {}", field_id);
            }
        }
    }

    fn try_get_field_offset(&self, field_id: FieldIdent, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                let field_index = match initial.id_map.get(&field_id.id) {
                    Some(i) => i,
                    None => return Ok(DependencyReport::does_not_exist())
                };
                let field = &initial.fields[*field_index];
                match field.offset.evaluate_expect_position(lookup) {
                    Ok(offset) => {
                        let mut dr = DependencyReport::success(ExprValue::Position(offset));
                        let field_offset_id = field_id.clone().get_attr_ident(FieldAttrType::Offset).to_abstract();
                        dr.add_pc_pairs(self.convert_expr_vars(field.offset.vars())?, HashSet::from([field_offset_id]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => {
                        Ok(DependencyReport::incomplete(self.convert_expr_vars(field.offset.vars())?))
                    }
                    Err(err) => return Err(err)
                }
            },
            _ => {
                todo!()
            }
        }
    }

    fn try_get_field_alignment(&self, field_id: FieldIdent, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                let field_index = match initial.id_map.get(&field_id.id) {
                    Some(i) => i,
                    None => return Ok(DependencyReport::does_not_exist())
                };
                let field = &initial.fields[*field_index];
                match field.alignment.evaluate_expect_position(lookup) {
                    Ok(alignment) => {
                        let mut dr = DependencyReport::success(ExprValue::Position(alignment));
                        let field_alignment_id = field_id.clone().get_attr_ident(FieldAttrType::Align).to_abstract();
                        dr.add_pc_pairs(self.convert_expr_vars(field.alignment.vars())?, HashSet::from([field_alignment_id]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => {
                        Ok(DependencyReport::incomplete(self.convert_expr_vars(field.alignment.vars())?))
                    }
                    Err(err) => return Err(err)
                }
            },
            _ => {
                todo!()
            }
        }
    }

    fn try_get_field_alignment_base(&self, field_id: FieldIdent, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                let field_index = match initial.id_map.get(&field_id.id) {
                    Some(i) => i,
                    None => return Ok(DependencyReport::does_not_exist())
                };
                let field = &initial.fields[*field_index];
                match field.alignment_base.evaluate_expect_position(lookup) {
                    Ok(alignment_base) => {
                        let mut dr = DependencyReport::success(ExprValue::Position(alignment_base));
                        let field_alignment_base_id = field_id.clone().get_attr_ident(FieldAttrType::AlignBase).to_abstract();
                        dr.add_pc_pairs(self.convert_expr_vars(field.alignment_base.vars())?, HashSet::from([field_alignment_base_id]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => {
                        Ok(DependencyReport::incomplete(self.convert_expr_vars(field.alignment_base.vars())?))
                    }
                    Err(err) => return Err(err)
                }
            },
            _ => {
                todo!()
            }
        }
    }

    fn try_get_field_end(&self, field_id: FieldIdent, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {

                let start_id = field_id.clone().get_attr_ident(FieldAttrType::Start);
                let length_id = field_id.clone().get_attr_ident(FieldAttrType::Length);
                let parents = HashSet::from([start_id.clone().to_abstract(), length_id.clone().to_abstract()]);

                let start = match vd.lookup_field_attr(&start_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
    
                let length = match vd.lookup_field_attr(&length_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
        
                let mut dr = DependencyReport::success(ExprValue::Position(start + length));
                let end_id = field_id.clone().get_attr_ident(FieldAttrType::End).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([end_id]));
                Ok(dr)

            },
            _ => {
                todo!()
            }
        }       
    }

    fn try_get_def_search_start(&self, field_id: FieldIdent, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        if self.original.def_fields.contains_key(&field_id.id) {
            match &self.original.def_fields[&field_id.id] {
                DefField::Search{start, ..} => {
                    match start.evaluate_expect_position(lookup) {
                        Ok(value) => {
                            let mut dr = DependencyReport::success(ExprValue::Position(value));
                            let search_start_id = field_id.clone().get_attr_ident(FieldAttrType::SearchStart);
                            dr.add_pc_pairs(self.convert_expr_vars(start.vars())?, HashSet::from([search_start_id.to_abstract()]));
                            Ok(dr)
                        },
                        Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(start.vars())?)),
                        Err(err) => Err(err)
                    }
                },
                _ => Ok(DependencyReport::does_not_exist())
            }
        } else {
            todo!()
        }
    }

    fn try_get_def_search_n(&self, field_id: FieldIdent, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        if self.original.def_fields.contains_key(&field_id.id) {
            match &self.original.def_fields[&field_id.id] {
                DefField::Search{n, ..} => {
                    match n.evaluate_expect_integer(lookup) {
                        Ok(value) => {
                            let mut dr = DependencyReport::success(ExprValue::Integer(value));
                            let search_n_id = field_id.clone().get_attr_ident(FieldAttrType::SearchN);
                            dr.add_pc_pairs(self.convert_expr_vars(n.vars())?, HashSet::from([search_n_id.to_abstract()]));
                            Ok(dr)
                        },
                        Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(n.vars())?)),
                        Err(err) => Err(err)
                    }
                },
                _ => Ok(DependencyReport::does_not_exist())
            }
        } else {
            todo!()
        }
    }

    fn try_get_def_anchor_offset(&self, field_id: FieldIdent, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        if self.original.def_fields.contains_key(&field_id.id) {
            match &self.original.def_fields[&field_id.id] {
                DefField::Anchor{offset} => {
                    match offset.evaluate_expect_position(lookup) {
                        Ok(value) => {
                            let mut dr = DependencyReport::success(ExprValue::Position(value));
                            let anchor_offset_id = field_id.clone().get_attr_ident(FieldAttrType::AnchorOffset);
                            dr.add_pc_pairs(self.convert_expr_vars(offset.vars())?, HashSet::from([anchor_offset_id.to_abstract()]));
                            Ok(dr)
                        },
                        Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(offset.vars())?)),
                        Err(err) => Err(err)
                    }
                },
                _ => Ok(DependencyReport::does_not_exist())
            }
        } else {
            todo!()
        }
    }

    fn try_lookup_in_children(&mut self, key: AbstractIdent, vd: &ValueDictionary, prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {

        let mut current_position_key = self.id.get_attr_ident(StructureAttrType::Position);

        match self.stype.as_mut() {
            PartiallyResolvedStructureType::UnresolvedSection{..} => {
                
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
                        Err(ExprEvalError::LookupError{..}) => {todo!("Not sure if this is right...")},
                        Err(err) => return Err(err)
                    }
                    current_position_key = structure.borrow().id.get_attr_ident(StructureAttrType::End);
                }

                Ok(current_best)
            },
            PartiallyResolvedStructureType::Switch{value, cases, default, ref mut pss} => {
                if let Some(ref mut pss) = pss {
                    // If the swich case is known, then this should be populated. It is all we need
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
                        Err(ExprEvalError::LookupError{..}) => {todo!("Not sure if this is right...")},
                        Err(err) => return Err(err)
                    }
                    current_position_key = structure.borrow().id.get_attr_ident(StructureAttrType::End);
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
                    Err(ExprEvalError::LookupError{..}) => {todo!("Not sure if this is right...")},
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
                        Err(ExprEvalError::LookupError{..}) => {todo!("Not sure if this is right...")},
                        Err(err) => return Err(err)
                    }
                    current_position_key = structure.borrow().id.get_attr_ident(StructureAttrType::End);
                } 

                Ok(current_best)

            }
        }
    }

    fn try_get_switch_value(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::Switch{value, ..} => {
                let parents = self.convert_expr_vars(value.vars())?;
                match value.evaluate(lookup) {
                    Ok(v) => {

                        // let mut input = String::new();
                        // info!("Determined Chunk type is {:?}. Pausing", v);
                        // std::io::stdin().read_line(&mut input);


                        let mut dr = DependencyReport::success(v);
                        let switch_value_id = self.id.get_attr_ident(StructureAttrType::SwitchValue).to_abstract();
                        dr.add_pc_pairs(parents, HashSet::from([switch_value_id]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(parents)),
                    Err(err) => Err(err)
                }
            },
            _ => panic!("switch_value not valid for '{}'", self.id)
        }
    }

    fn try_get_switch_case(&self, i: usize, vd: &ValueDictionary, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        // info!("In try_get_switch_case");
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::Switch{cases, ..} => {

                if i < cases.len() {
                    let expr = &cases[i].0;
                    let mut parents = self.convert_expr_vars(expr.vars())?;

                    let switch_value_id = self.id.get_attr_ident(StructureAttrType::SwitchValue);
                    parents.insert(switch_value_id.clone().to_abstract());
                    let arg = match vd.lookup_struct_attr(&switch_value_id) {
                        Some(ev) => ev,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };

                    match expr.evaluate_with_args(&vec![arg], lookup) {
                        Ok(v) => {

                            // let mut input = String::new();
                            // info!("Determined switch case #{} is {:?}. Pausing", i, v);
                            // std::io::stdin().read_line(&mut input);

                            let mut dr = DependencyReport::success(ExprValue::Bool(v.expect_bool()?));
                            let case_id = self.id.get_attr_ident(StructureAttrType::SwitchCase(i)).to_abstract();
                            dr.add_pc_pairs(parents, HashSet::from([case_id]));
                            Ok(dr)
                        },
                        Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(parents)),
                        Err(err) => Err(err)
                    }
                } else {
                    panic!("Switch case out of bounds: {}", i);
                    Ok(DependencyReport::does_not_exist())
                }

            },
            _ => panic!("switch_case not valid for '{}'", self.id)
        }
    }

    fn try_get_switch_index(&mut self, vd: &ValueDictionary, prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        // This has to be factored weird to get past the borrow checker
        let n_cases = match self.stype.as_ref() {
            PartiallyResolvedStructureType::Switch{cases, ..} => {
                cases.len()

            },
            _ => panic!("switch_index not valid for '{}'", self.id)
        };

    
        let mut parents = HashSet::new();
        for i in 0..n_cases {
            let case_id = self.id.get_attr_ident(StructureAttrType::SwitchCase(i));
            parents.insert(case_id.clone().to_abstract());
            let check = match vd.lookup_struct_attr(&case_id) {
                Some(ev) => ev.expect_bool()?,
                None => return Ok(DependencyReport::incomplete(parents))
            };
            if check {
                let child_monikers = self.monikers_for_child();
                match self.stype.as_mut() {
                    PartiallyResolvedStructureType::Switch{cases, ref mut pss, ..} => {
                        let mut child_index = self.index_path.clone();
                        child_index.push(0); // Always 0 since switch only has one child
                        let mut prs = PartiallyResolvedStructure::new_indexed(cases[i].1.clone(), self.id.clone(), child_index, prs_map);
                        prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers);
                        *pss = Some(prs);
                    },
                    _ => unreachable!()
                };
                let mut dr = DependencyReport::success(ExprValue::Integer(i as i64));
                let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([switch_index_id]));
                return Ok(dr)
                
            }
        }
        let child_monikers = self.monikers_for_child();
        match self.stype.as_mut() {
            PartiallyResolvedStructureType::Switch{default, ref mut pss, ..} => {
                let mut child_index = self.index_path.clone();
                child_index.push(0); // Always 0 since switch only has one child
                let prs = PartiallyResolvedStructure::new_indexed(default.clone(), self.id.clone(), child_index, prs_map);
                prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers);
                *pss = Some(prs);
            },
            _ => unreachable!()
        };
        let mut dr = DependencyReport::success(ExprValue::Integer(n_cases as i64));
        let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex).to_abstract();
        dr.add_pc_pairs(parents, HashSet::from([switch_index_id]));
        Ok(dr)

    }

    fn try_get_break(&self, i: usize, vd: &ValueDictionary, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        if self.original.breaks.is_empty() {
            // If there are no breaks, return false to indicate that no break condition has been met
            return Ok(DependencyReport::success(ExprValue::Bool(false)))
        }
        let last_prs = match self.stype.as_ref() {
            PartiallyResolvedStructureType::RepeatUntil{ref seq, ..} => {
                seq[i].clone()
            },
            PartiallyResolvedStructureType::Chain{ref prs, ref seq, ..} => {
                if i == 0 {
                    prs.clone()
                } else {
                    // seq[i - 1].clone()
                    if let PartiallyResolvedStructureType::Addressed{pss, ..} = seq[i - 1].borrow().stype.as_ref() {
                        pss.clone()
                    } else {
                        unreachable!()
                    }
                }
            }
            _ => todo!()
        };

        // The break condition is evaluated using the context of the repeating structure
        let lookup = last_prs.borrow().get_lookup_fn(vd);
        let mut parents = HashSet::new();
        for b in &self.original.breaks {
            let expr = &b.condition;
            parents.extend(last_prs.borrow().convert_expr_vars(expr.vars())?);
        }

        for b in &self.original.breaks {
            let expr = &b.condition;
            match expr.evaluate(&lookup) {
                Ok(v) => {

                    // let mut input = String::new();
                    // info!("Determined switch case #{} is {:?}. Pausing", i, v);
                    // std::io::stdin().read_line(&mut input);
                    if v.expect_bool()? {
                        let mut dr = DependencyReport::success(ExprValue::Bool(true));
                        let break_id = self.id.get_attr_ident(StructureAttrType::Break(i)).to_abstract();
                        dr.add_pc_pairs(parents, HashSet::from([break_id]));
                        return Ok(dr)
                    }
                    
                },
                Err(ExprEvalError::LookupError{..}) => return Ok(DependencyReport::incomplete(parents)),
                Err(err) => return Err(err)
            }
        }

        let mut dr = DependencyReport::success(ExprValue::Bool(false));
        let break_id = self.id.get_attr_ident(StructureAttrType::Break(i)).to_abstract();
        dr.add_pc_pairs(parents, HashSet::from([break_id]));

        Ok(dr)
    }

    fn try_get_repetitions(&mut self, vd: &ValueDictionary, lookup: &dyn Fn(&str) -> Option<ExprValue>, prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        let end_id = self.id.get_attr_ident(StructureAttrType::End);
        let start_id = self.id.get_attr_ident(StructureAttrType::Start);

        let child_monikers = self.monikers_for_child();

        match self.stype.as_mut() {
            PartiallyResolvedStructureType::Repeat{n, structure, ref mut seq, ref mut finalized} => {
                let n = n.clone(); // Need this for the borrow checker
                match n.evaluate_expect_integer(lookup) {
                    Ok(v) => {
                        let mut struct_seq = Vec::new();
                        for i in 0..v {
                            let mut child_index = self.index_path.clone();
                            child_index.push(i as usize);
                            let mut prs = PartiallyResolvedStructure::new_indexed(structure.clone(), self.id.clone(), child_index, prs_map);
                            prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
                            struct_seq.push(prs);
                        }
                        *seq = struct_seq;
                        *finalized = true;
                        let mut dr = DependencyReport::success(ExprValue::Integer(v));
                        let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract();
                        let parents = self.convert_expr_vars(n.vars())?;
                        dr.add_pc_pairs(parents, HashSet::from([repetitions_id]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => {
                        let parents = self.convert_expr_vars(n.vars())?;
                        Ok(DependencyReport::incomplete(parents))
                    },
                    Err(err) => Err(err)
                }
            },
            PartiallyResolvedStructureType::RepeatUntil{end, structure, ref mut seq, ref mut finalized} => {

                // If the structure is already finalized, just return the length of the existing sequence
                if *finalized {
                    return Ok(DependencyReport::success(ExprValue::Integer(seq.len() as i64)));
                }

                let mut parents = HashSet::new();

                // If there is at least one member of the sequence, check if it has met a break condition
                if !seq.is_empty() {
                    let break_id = self.id.clone().get_attr_ident(StructureAttrType::Break(seq.len() - 1));
                    parents.insert(break_id.clone().to_abstract());
                    let is_break = match vd.lookup_struct_attr(&break_id) {
                        Some(ev) => ev.expect_bool()?,
                        None => return Ok(DependencyReport::incomplete(parents))
                    };
                    println!("Checking for break: {}", is_break);
                    if is_break {
                        *finalized = true;
                        let mut dr = DependencyReport::success(ExprValue::Integer(seq.len() as i64));
                        let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract();
                        dr.add_pc_pairs(parents, HashSet::from([break_id.to_abstract()]));
                        return Ok(dr)
                    }
                }

                // The end of the current repetition is either the start of the structure (if no repetitions have
                // been estabilished yet) or the end of the last established repetition
                let current_end_id = if seq.is_empty() {
                    start_id
                } else {
                    let last_index = seq.len() - 1;
                    seq[last_index].borrow().id.get_attr_ident(StructureAttrType::End)
                };
                
                parents.insert(current_end_id.clone().to_abstract());
                parents.insert(end_id.clone().to_abstract());

                let current_end = match vd.lookup_struct_attr(&current_end_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                let end = match  vd.lookup_struct_attr(&end_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };

                if current_end < end {
                    let mut child_index = self.index_path.clone();
                    child_index.push(seq.len());
                    let prs = PartiallyResolvedStructure::new_indexed(structure.clone(), self.id.clone(), child_index, prs_map);
                    prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
                    parents.insert(prs.borrow().id.get_attr_ident(StructureAttrType::End).to_abstract());
                    seq.push(prs);
                    Ok(DependencyReport::incomplete(parents))
                } else {
                    *finalized = true;

                    let mut dr = DependencyReport::success(ExprValue::Integer(seq.len() as i64));
                    let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract();
                    dr.add_pc_pairs(parents, HashSet::from([repetitions_id]));
                    Ok(dr)
                }
            },
            PartiallyResolvedStructureType::Chain{next, structure, prs, ref mut seq, ref mut finalized} => {
                let next = next.clone(); // Need this for the borrow checker

                // If the structure is already finalized, just return the length of the existing sequence
                if *finalized {
                    return Ok(DependencyReport::success(ExprValue::Integer(seq.len() as i64 + 1)));
                }

                let mut parents = HashSet::new();

                // Keep in mind prs is the "zeroth" sequence member, so this works even if the sequence is empty
                let break_id = self.id.clone().get_attr_ident(StructureAttrType::Break(seq.len()));
                parents.insert(break_id.clone().to_abstract());
                let is_break = match vd.lookup_struct_attr(&break_id) {
                    Some(ev) => ev.expect_bool()?,
                    None => return Ok(DependencyReport::incomplete(parents))
                };
                println!("Checking for break: {}", is_break);
                if is_break {
                    *finalized = true;
                    let mut dr = DependencyReport::success(ExprValue::Integer(seq.len() as i64));
                    let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract();
                    dr.add_pc_pairs(parents, HashSet::from([break_id.to_abstract()]));
                    return Ok(dr)
                }

                let last_member = if seq.is_empty() {
                    prs.clone()
                } else {
                    let n = seq.len() - 1;
                    if let PartiallyResolvedStructureType::Addressed{pss, ..} = seq[n].borrow().stype.as_ref() {
                        pss.clone()
                    } else {
                        unreachable!()
                    }
                };
                info!("Last member is {:?}", last_member.borrow().id);
                info!("Last member's import monikers: {:?}", last_member.borrow().import_monikers);
                parents.extend(last_member.borrow().convert_expr_vars(next.vars())?);

                let lookup = last_member.borrow().get_lookup_fn(vd);

                match next.evaluate_expect_position(&lookup) {
                    Ok(v) => {
                        // panic!("Next position: {:?} from {:?}", v, last_member.borrow().id);
                        if !seq.is_empty() {
                            panic!("Next position: {:?} from {:?}", v, last_member.borrow().id);
                        }
                        let mut child_index = self.index_path.clone();
                        child_index.push(seq.len() + 1); // + 1 to account for prs
                        let mut child = PartiallyResolvedStructure::new_indexed(structure.clone(), self.id.clone(), child_index, prs_map);
                        // panic!("PRS: {:?}", prs_map.get(&child.borrow().id).unwrap().borrow().id);
                        if let PartiallyResolvedStructureType::Addressed{ref mut position, ..} = child.borrow_mut().stype.as_mut() {
                            *position = Rc::new(Expr::Value(ExprValue::Position(v)));
                        } else {
                            unreachable!()
                        }
                        child.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
                        seq.push(child);

                        return Ok(DependencyReport::incomplete(parents));
                    },
                    Err(ExprEvalError::LookupError{..}) => {
                        Ok(DependencyReport::incomplete(parents))
                    },
                    Err(err) => Err(err)
                }
            },
            _ => Ok(DependencyReport::does_not_exist())
        }
    }

    fn try_get_position(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            // PartiallyResolvedStructureType::UnresolvedSection{initial, pss: Some(section)} => {
            //     Ok(DependencyReport::success(ExprValue::Position(section.get_position())))
            // },
            PartiallyResolvedStructureType::Addressed{position, ..} => {
                match position.evaluate_expect_position(lookup) {
                    Ok(value) => {
                        let mut dr = DependencyReport::success(ExprValue::Position(value));
                        let position_id = self.id.get_attr_ident(StructureAttrType::Position);
                        dr.add_pc_pairs(self.convert_expr_vars(position.vars())?, HashSet::from([position_id.to_abstract()]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(position.vars())?)),
                    Err(err) => Err(err)
                }
            },
            _ => {
                // Note: This is discounting the possibility that the start location could be determined by 
                // measuring backward from some known position, but that can be avoided by revising the file spec
                Ok(DependencyReport::does_not_exist())
            }
        }        
    }

    fn try_get_alignment(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.alignment.evaluate_expect_position(lookup) {
            Ok(value) => {
                let mut dr = DependencyReport::success(ExprValue::Position(value));
                let alignment_id = self.id.get_attr_ident(StructureAttrType::Align).to_abstract();
                dr.add_pc_pairs(self.convert_expr_vars(self.alignment.vars())?, HashSet::from([alignment_id]));
                Ok(dr)
            },
            Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(self.alignment.vars())?)),
            Err(err) => Err(err)
        }
    }

    fn try_get_alignment_base(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.alignment_base.evaluate_expect_position(lookup) {
            Ok(value) => {
                let mut dr = DependencyReport::success(ExprValue::Position(value));
                let alignment_base_id = self.id.get_attr_ident(StructureAttrType::AlignBase).to_abstract();
                dr.add_pc_pairs(self.convert_expr_vars(self.alignment_base.vars())?, HashSet::from([alignment_base_id]));
                Ok(dr)
            },
            Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(self.alignment_base.vars())?)),
            Err(err) => Err(err)
        }
    }

    fn try_get_start_pad(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, ExprEvalError> {

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

    fn try_get_start(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, ExprEvalError> {

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

    fn try_get_length(&self, vd: &ValueDictionary, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match &self.length {
            LengthPolicy::Expand => {
                // let spare_length_id = format!("{}.spare_length", self.parent_id).to_string(); // TODO: Make this not as janky
                let spare_length_id = self.parent_id.get_attr_ident(StructureAttrType::SpareLength);
                let length = match vd.lookup_struct_attr(&spare_length_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(HashSet::from([spare_length_id.to_abstract()])))
                };
                let mut dr = DependencyReport::success(ExprValue::Position(length));
                let length_id = self.id.get_attr_ident(StructureAttrType::Length).to_abstract();
                dr.add_pc_pairs(HashSet::from([spare_length_id.to_abstract()]), HashSet::from([length_id]));
                Ok(dr)
            },
            LengthPolicy::Expr(expr) => {
                let parents = self.convert_expr_vars(expr.vars())?;
                
                match expr.evaluate(lookup) {
                    Ok(v) => {
                        let mut dr = DependencyReport::success(v);
                        let length_id = self.id.get_attr_ident(StructureAttrType::Length).to_abstract();
                        dr.add_pc_pairs(parents, HashSet::from([length_id]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(parents)),
                    Err(err) => Err(err)
                }
            },
            LengthPolicy::FitContents => {
                let contents_length_id = self.id.get_attr_ident(StructureAttrType::ContentsLength);
                let contents_length = match vd.lookup_struct_attr(&contents_length_id) {
                    Some(ev) => ev.expect_position()?,
                    None => return Ok(DependencyReport::incomplete(HashSet::from([contents_length_id.to_abstract()])))
                };
                let mut dr = DependencyReport::success(ExprValue::Position(contents_length));
                let length_id = self.id.get_attr_ident(StructureAttrType::Length).to_abstract();
                dr.add_pc_pairs(HashSet::from([contents_length_id.to_abstract()]), HashSet::from([length_id]));
                Ok(dr)
            }
        }
    }

    fn try_get_spare_length(&self, vd: &ValueDictionary) -> Result<DependencyReport<ExprValue>, ExprEvalError> {

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

    fn try_resolve_import(&self, key: String) -> Result<DependencyReport<StructureIdent>, ExprEvalError> {
        info!("Trying to resolve import in {:?}", self.id);
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {

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
                            Err(ExprEvalError::LookupError{..}) => {todo!("Not sure if this is right...")},
                            Err(err) => return Err(err)
                        }
                    }
                }

                Ok(current_best)
            },
            PartiallyResolvedStructureType::Switch{value, cases, default, pss} => {

                if let Some(pss) = pss {
                    // If the swich case is known, then this should be populated. It is all we need
                    pss.borrow().try_resolve_import(key)

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

            }
        }
    }

    /// Tries to compute the structure length based on contents
    fn try_get_contents_length(&self, vd: &ValueDictionary, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                let section = initial;
                match &section.length {
                    LengthPolicy::Expand => {
                        return Ok(DependencyReport::does_not_exist())
                    },
                    LengthPolicy::Expr(expr) => {
                        match expr.evaluate_expect_position(lookup) {
                            Ok(value) => {
                                let mut dr = DependencyReport::success(ExprValue::Position(value));
                                let contents_length_id = self.id.get_attr_ident(StructureAttrType::ContentsLength).to_abstract();
                                dr.add_pc_pairs(self.convert_expr_vars(expr.vars())?, HashSet::from([contents_length_id]));
                                Ok(dr)
                            },
                            Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(expr.vars())?)),
                            Err(err) => Err(err)
                        }
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
                let mut dr = DependencyReport::success(ExprValue::Position(length));
                let contents_length_id = self.id.get_attr_ident(StructureAttrType::Position).to_abstract();
                dr.add_pc_pairs(parents, HashSet::from([contents_length_id]));
                Ok(dr)
            },
            PartiallyResolvedStructureType::Switch{value, cases, default, pss} => {
                // Note: This is discounting the possibility that the length could be determined
                // by finding that the lengths of all cases are identical
                let mut parents = HashSet::new();

                if let Some(pss) = pss {
                    let struct_length_id = pss.borrow().id.get_attr_ident(StructureAttrType::Length);
                    let struct_start_pad_id = pss.borrow().id.get_attr_ident(StructureAttrType::StartPad);
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
                    // todo!()
                    let switch_value_id = self.id.get_attr_ident(StructureAttrType::SwitchValue).to_abstract();
                    return Ok(DependencyReport::incomplete(HashSet::from([switch_value_id])))
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
        }        
    }

    fn try_get_end(&self, vd: &ValueDictionary, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::RepeatUntil{end, ..} => {
                match end.evaluate_expect_position(lookup) {
                    Ok(value) => {
                        let mut dr = DependencyReport::success(ExprValue::Position(value));
                        let end_id = self.id.get_attr_ident(StructureAttrType::End).to_abstract();
                        dr.add_pc_pairs(self.convert_expr_vars(end.vars())?, HashSet::from([end_id]));
                        Ok(dr)
                    },
                    Err(ExprEvalError::LookupError{..}) => Ok(DependencyReport::incomplete(self.convert_expr_vars(end.vars())?)),
                    Err(err) => Err(err)
                }
            },
            _ => {
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
    }

    fn unknowns(&self, vd: &ValueDictionary) -> Result<(HashSet<AbstractIdent>, HashSet<AbstractIdent>), ExprEvalError> { // (Unknowns that need to be found to resolve fields, unknowns that need to be found to get more unknowns)
        let mut unks = HashSet::new();
        let mut expand_deps = HashSet::new();

        unks.insert(self.id.get_attr_ident(StructureAttrType::Start).to_abstract());
        unks.insert(self.id.get_attr_ident(StructureAttrType::Length).to_abstract());

        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{..} => {
                
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
            PartiallyResolvedStructureType::Switch{value, cases, default, pss} => {
                let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex);
                expand_deps.insert(switch_index_id.to_abstract());
                if let Some(pss) = pss {
                    // If the swich case is known, then this should be populated. It is all we need
                    let (new_unks, new_expand_deps) = pss.borrow().unknowns(vd)?;
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
                                    // which case is correct. But if that were true, pss should have been
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
                    // the default case is correct. But if that were true, pss should have been
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
            PartiallyResolvedStructureType::RepeatUntil{end, structure, seq, finalized} => {
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
            PartiallyResolvedStructureType::Chain{next, structure, prs, seq, finalized} => {
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

            }
        }
        
        Ok((unks, expand_deps))
    }
}

pub struct XmlObject<'a> {
    element: MyStrSpan<'a>,
    attrs: HashMap<&'a str, MyStrSpan<'a>>,
    children: Vec<XmlObject<'a>>
}

#[derive(Clone)]
enum DefField {
    Search{start: Expr, find: String, n: Expr},
    Anchor{offset: Expr},
    Compute(Expr)
}

impl DefField {
    fn from_xml_object(mut obj: XmlObject) -> Result<DefField, ParseXmlError> {
        match obj.element.as_str() {
            "search" => {
                let start = Expr::from_str(&convert_xml_symbols(obj.attrs.remove("start").unwrap().as_str()))?;
                let find = convert_xml_symbols(obj.attrs.remove("find").unwrap().as_str());
                let n = match obj.attrs.remove("n") {
                    Some(expr) => Expr::from_str(&convert_xml_symbols(expr.as_str()))?,
                    None => Expr::Value(ExprValue::Integer(0))
                };
                Ok(DefField::Search{start, find, n})
            },
            "anchor" => {
                let offset = match obj.attrs.remove("offset") {
                    Some(expr) => Expr::from_str(&convert_xml_symbols(expr.as_str()))?,
                    None => Expr::Value(ExprValue::Position(BitIndex::zero()))
                };
                Ok(DefField::Anchor{offset})
            },
            d => panic!("Unrecognized def field: '{}'", d)
        }
    }
}

#[derive(Clone)]
struct Break {
    condition: Expr
}

#[derive(Clone)]
enum StructureType {
    Section(Rc<SectionSpec>),
    Addressed{position: Rc<Expr>, content: Rc<Structure>},
    Sequence(Vec<Rc<Structure>>),
    Switch{value: Rc<Expr>, cases: Vec<(Rc<Expr>, Rc<Structure>)>, default: Rc<Structure>},
    Repeat{n: Rc<Expr>, structure: Rc<Structure>},
    RepeatUntil{end: Rc<Expr>, structure: Rc<Structure>},
    Chain{next: Rc<Expr>, structure: Rc<Structure>}
}

#[derive(Clone)]
pub struct Structure {
    id: String,
    name: String,
    alignment: Expr,
    alignment_base: Expr,
    length: LengthPolicy,
    exports: HashMap<String, Option<Expr>>,
    def_fields: HashMap<String, DefField>,
    breaks: Vec<Break>,
    stype: StructureType
}

impl Structure {

    /// Returns the first child (or self) that has an id that matches the string supplied if
    /// one exists. Returns None otherwise
    fn get_child_by_id(self: &Rc<Self>, id: &str) -> Option<Rc<Structure>> {
        if self.id == id {
            Some(self.clone())
        } else {
            match &self.stype {
                StructureType::Section(_) => None,
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
                StructureType::Repeat{structure, ..} | StructureType::RepeatUntil{structure, ..} | StructureType::Chain{structure, ..} => {
                    structure.get_child_by_id(id)
                }
            }
        }
    }

    fn wrap_addressed(inner: Rc<Structure>, position: Expr) -> Structure {
        let id = inner.id.clone() + "#wrapper";
        let name = inner.name.clone();
        let alignment = Expr::Value(ExprValue::Position(BitIndex::bits(1))); // Smallest increment possible
        let alignment_base = Expr::Value(ExprValue::Position(BitIndex::zero())); // Doesn't matter
        let length = LengthPolicy::FitContents; // This won't work if inner's length is Expand, but that isn't really valid for chain children anyway
        let exports = inner.exports.clone(); // Need to have the same exports so they can tunnel through
        let def_fields = HashMap::new();
        let breaks = vec![];
        let stype = StructureType::Addressed{position: Rc::new(position), content: inner};

        Structure {
            id,
            name,
            alignment,
            alignment_base,
            length,
            exports,
            def_fields,
            breaks,
            stype
        }
    }

    fn from_xml_object(mut obj: XmlObject) -> Result<Structure, ParseXmlError> {
        if obj.element.as_str() == "use" {
            let url = convert_xml_symbols(obj.attrs.remove("url").unwrap().as_str());
            if let Some((fname, struct_id)) = url.split_once("#") {
                let text = std::fs::read_to_string(fname).unwrap();
                let s = Rc::new(parse_xml(&text).unwrap());
                if let Some(child) = s.get_child_by_id(struct_id) {
                    std::mem::drop(s);
                    return Ok(Rc::into_inner(child).unwrap())
                } else {
                    panic!("Structure does not contain id '{}'", struct_id)
                }
            } else {
                panic!("Unrecognized url format: '{}'", url)
            }
        }

        let length = match obj.attrs.remove("length-policy") {
            None => match obj.attrs.remove("length") {
                None => LengthPolicy::FitContents,
                Some(s) => LengthPolicy::Expr(Expr::from_str(&convert_xml_symbols(s.as_str()))?)
            },
            Some(s) => match convert_xml_symbols(s.as_str()).as_str() {
                "expr" => match obj.attrs.remove("length") {
                    None => panic!("Length policy is expr but no length specified"),
                    Some(s) => LengthPolicy::Expr(Expr::from_str(&convert_xml_symbols(s.as_str()))?)
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
            Some(s) => Expr::from_str(&convert_xml_symbols(s.as_str()))?,
            None => Expr::Value(ExprValue::Position(BitIndex::bytes(1)))
        };
        let alignment_base = match obj.attrs.remove("align-base") {
            Some(s) => Expr::from_str(&convert_xml_symbols(s.as_str()))?,
            None => Expr::Value(ExprValue::Position(BitIndex::zero()))
        };

        let mut exports = HashMap::new();
        let mut def_fields = HashMap::new();
        let mut breaks = Vec::new();
        let stype: StructureType;

        match obj.element.as_str() {
            "section" | "section-body" | "section-header" | "section-footer" => {
                let section_type = match obj.element.as_str() {
                    "section" | "section-body" => SectionType::Body,
                    "section-header" => SectionType::Header,
                    "section-footer" => SectionType::Footer,
                    _ => panic!("Unrecognized length policy")
                };

                let mut fields = vec![];
                let mut id_map = HashMap::new();

                for mut child in obj.children {
                    match child.element.as_str() {
                        "export" => {
                            let export_source = convert_xml_symbols(child.attrs.remove("source").unwrap().as_str());
                            let export_default = match obj.attrs.remove("default") {
                                None => None,
                                Some(expr)=> {
                                    Some(Expr::from_str(&convert_xml_symbols(expr.as_str()))?)
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
                        _ => {
                            let field = FieldSpec::from_xml_object(child)?;
                            id_map.insert(field.id.clone(), fields.len());
                            fields.push(field);
                        }
                    }
                    
                }

                stype = StructureType::Section(Rc::new(SectionSpec {
                    length: length.clone(),
                    section_type,
                    fields,
                    id_map
                }));


            },
            "addressed" => {
                if obj.children.len() != 1 {
                    panic!("addressed must have exactly one child")
                }
                let mut structure = None;
                for mut child in obj.children {
                    match child.element.as_str() {
                        "export" => {
                            let export_source = convert_xml_symbols(child.attrs.remove("source").unwrap().as_str());
                            let export_default = match obj.attrs.remove("default") {
                                None => None,
                                Some(expr)=> {
                                    Some(Expr::from_str(&convert_xml_symbols(expr.as_str()))?)
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
                        _ => {
                            if structure.is_none() {
                                structure = Some(Structure::from_xml_object(child)?);
                            } else {
                                panic!("'addressed' should only have one non-export child!");
                            }
                        }
                    }
                    
                }
                if let Some(structure) = structure {
                    let structure = Rc::new(structure);
                    match obj.attrs.remove("pos") {
                        None => panic!("'pos' must be specified for 'addressed' element"),
                        Some(expr)=> {
                            let pos = Expr::from_str(&convert_xml_symbols(expr.as_str()))?;
                            stype = StructureType::Addressed{position: Rc::new(pos), content: structure.clone()};
                        }
                    }
                } else {
                    panic!("'addressed' has no children!")
                }
            },
            "sequence" => {
                let mut seq = vec![];
                for mut child in obj.children {
                    match child.element.as_str() {
                        "export" => {
                            let export_source = convert_xml_symbols(child.attrs.remove("source").unwrap().as_str());
                            let export_default = match obj.attrs.remove("default") {
                                None => None,
                                Some(expr)=> {
                                    Some(Expr::from_str(&convert_xml_symbols(expr.as_str()))?)
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
                        _ => {
                            seq.push(Rc::new(Structure::from_xml_object(child)?));
                        }
                    }
                    
                }
                stype = StructureType::Sequence(seq);
            },
            "switch" => {
                let value = Expr::from_str(&convert_xml_symbols(obj.attrs.remove("value").unwrap().as_str()))?;
                let mut cases = vec![];
                let mut default = None;
                for mut child in obj.children {
                    match child.element.as_str() {
                        "export" => {
                            let export_source = convert_xml_symbols(child.attrs.remove("source").unwrap().as_str());
                            let export_default = match obj.attrs.remove("default") {
                                None => None,
                                Some(expr)=> {
                                    Some(Expr::from_str(&convert_xml_symbols(expr.as_str()))?)
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
                        "switch-case" => {
                            let check = Expr::from_str(&convert_xml_symbols(child.attrs.remove("check").unwrap().as_str()))?;
                            if child.children.len() != 1 {
                                panic!("switch-case must have exactly one child")
                            }
                            let s = Structure::from_xml_object(child.children.into_iter().nth(0).unwrap())?;
                            cases.push((Rc::new(check), Rc::new(s)));
                        },
                        "switch-else" => {
                            if default.is_some() {
                                panic!("More than one default specified")
                            }
                            if child.children.len() != 1 {
                                panic!("switch-else must have exactly one child")
                            }
                            let s = Structure::from_xml_object(child.children.into_iter().nth(0).unwrap())?;
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
                for mut child in obj.children {
                    match child.element.as_str() {
                        "export" => {
                            let export_source = convert_xml_symbols(child.attrs.remove("source").unwrap().as_str());
                            let export_default = match obj.attrs.remove("default") {
                                None => None,
                                Some(expr)=> {
                                    Some(Expr::from_str(&convert_xml_symbols(expr.as_str()))?)
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
                            let condition = Expr::from_str(&convert_xml_symbols(child.attrs.remove("condition").unwrap().as_str()))?;
                            breaks.push(Break{condition});
                        }
                        _ => {
                            if structure.is_none() {
                                structure = Some(Structure::from_xml_object(child)?);
                            } else {
                                panic!("'repeat' should only have one non-export child!");
                            }
                        }
                    }
                    
                }
                if let Some(structure) = structure {
                    let structure = Rc::new(structure);
                    match (obj.attrs.remove("until"), obj.attrs.remove("n")) {
                        (None, None) => panic!("'until' or 'n' must be specified for 'repeat' element"),
                        (Some(expr), None) => {
                            let end = Expr::from_str(&convert_xml_symbols(expr.as_str()))?;
                            stype = StructureType::RepeatUntil{end: Rc::new(end), structure};
                        },
                        (None, Some(expr)) => {
                            let n = Expr::from_str(&convert_xml_symbols(expr.as_str()))?;
                            stype = StructureType::Repeat{n: Rc::new(n), structure};
                        },
                        (Some(_), Some(_)) => panic!("Both 'until' and 'n' specified for 'repeat' element. One must be removed.")
                    }
                } else {
                    panic!("'repeat' has no children!")
                }
            },
            "chain" => {
                let mut structure = None;
                for mut child in obj.children {
                    match child.element.as_str() {
                        "export" => {
                            let export_source = convert_xml_symbols(child.attrs.remove("source").unwrap().as_str());
                            let export_default = match obj.attrs.remove("default") {
                                None => None,
                                Some(expr)=> {
                                    Some(Expr::from_str(&convert_xml_symbols(expr.as_str()))?)
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
                            let condition = Expr::from_str(&convert_xml_symbols(child.attrs.remove("condition").unwrap().as_str()))?;
                            breaks.push(Break{condition});
                        }
                        _ => {
                            if structure.is_none() {
                                structure = Some(Structure::from_xml_object(child)?);
                            } else {
                                panic!("'chain' should only have one non-export child!");
                            }
                        }
                    }
                    
                }
                if let Some(structure) = structure {
                    let structure = Rc::new(structure);
                    match obj.attrs.remove("next") {
                        None => panic!("'next' must be specified for 'chain' element"),
                        Some(expr) => {
                            let next = Expr::from_str(&convert_xml_symbols(expr.as_str()))?;
                            stype = StructureType::Chain{next: Rc::new(next), structure};
                        }
                    }
                } else {
                    panic!("'chain' has no children!")
                }
            },
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
                stype
            }
        )
    }
}

use std::cell::RefCell;

// Node of a Directed Acyclic Graph representy dependency relationships between fields
#[derive(Eq, PartialEq)]
struct DepNode {
    name: AbstractIdent,
    parents: Vec<Rc<RefCell<DepNode>>>, // Dependancies
    children: Vec<Rc<RefCell<DepNode>>> // Dependants
}

impl DepNode {
    fn new(id: AbstractIdent) -> DepNode {
        DepNode {
            name: id,
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
    lookup_self: Vec<usize>,
    lookup_path: Vec<String>,
    dep_roots: HashSet<AbstractIdent>,
    dep_map: HashMap<AbstractIdent, Rc<RefCell<DepNode>>>
}

impl DepGraph {
    fn new() -> DepGraph {
        DepGraph {
            lookup_self: Vec::new(),
            lookup_path: Vec::new(),
            dep_roots: HashSet::new(),
            dep_map: HashMap::new()
        }
    }

    fn reset(&mut self) {
        self.dep_roots.clear();
        self.dep_map.clear();
    }

    fn get_resolvable_unknowns(&self, vd: &ValueDictionary) -> Vec<AbstractIdent> {
        let mut result = Vec::new();
        for (s, node) in self.dep_map.iter() {
            if !vd.contains_key(s) {
                let mut resolvable = true;
                for parent in &node.borrow().parents {
                    if !vd.contains_key(&parent.borrow().name) {
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
    Spare(StructureIdent, std::ops::Range<BitIndex>)
}

pub struct FileMap<'a> {
    // structure: Structure,
    fields: Vec<UnparsedBinaryField>,
    dep_graph: DepGraph,
    value_dict: ValueDictionary,
    prs: Rc<RefCell<PartiallyResolvedStructure<'a>>>,
    prs_map: HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>
}

impl<'a> FileMap<'a> {

    pub fn new(structure: Rc<Structure>) -> FileMap<'a> {
        let file_struct = StructureIdent::new("file".to_string());
        let mut prs_map = HashMap::new();
        let prs = PartiallyResolvedStructure::new(structure.clone(), file_struct.clone(), &mut prs_map);

        let initial_monikers = HashMap::new();
        prs.borrow_mut().initialize_inherited_monikers(&prs_map, initial_monikers);

        FileMap {
            // structure,
            fields: vec![],
            dep_graph: DepGraph::new(),
            value_dict: ValueDictionary::new(),
            prs,
            prs_map
        }
    }

    pub fn region_at(&mut self, index: BitIndex, fm: &mut crate::FileManager) -> FileRegion {
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
                        _ => return fr
                    }
                },
                DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                    self.get_data(deps.into_iter().collect(), fm);
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
                            return fr
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
                return temp_result
            }
            self.get_data(targets, fm);
            targets = vec![];
        }
    }

    pub fn get_field(&mut self, field: FieldIdent, fm: &mut crate::FileManager) -> UnparsedBinaryField {
        let child = self.prs_map.get(&field.structure).unwrap().clone();

        loop {

            let dr = child.borrow().try_get_field(field.clone(), &self.value_dict, &self.prs_map).unwrap();
            match dr.result {
                DepResult::Success(ds) => {
                    match ds {
                        DataSource::Field(f) => return f,
                        _ => panic!("{:?} is not a field", field)
                    }
                },
                DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                    let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                    // std::mem::drop(dr);
                    self.get_data(deps.into_iter().collect(), fm);
                },
                DepResult::DoesNotExist => {
                    todo!("Field does not exist?")
                }
            }
        }
    }

    pub fn format_field_data(&self, field: FieldIdent, value: ExprValue) -> String {
        let s = self.prs_map[&field.structure].clone();
        let s_borrow = s.borrow();
        match s_borrow.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial} => {
                return initial.format_field_data(field.id.clone(), value)
                

            },
            _ => panic!("Cannot get a field for a non-section structure!")
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

    fn get_data(&mut self, targets: Vec<AbstractIdent>, fm: &mut crate::FileManager) {
        let mut dg = std::mem::take(&mut self.dep_graph);
        let mut vd = std::mem::take(&mut self.value_dict);
        let mut prs_map = std::mem::take(&mut self.prs_map);

        let mut targets = targets;

        let file_struct = StructureIdent::new("file".to_string());


        loop {

            for target in targets {

                info!("");
                info!("Lookup up {}", target);
                match target {
                    AbstractIdent::Field(ref fi) => {
                        info!("Target is a Field");

                        let child = prs_map.get(&fi.structure).unwrap();

                        // let field_id = struct_id.get_field_ident(fi.id);
                        let dr = child.borrow().try_get_field(fi.clone(), &vd, &prs_map).unwrap();
                        match dr.result {
                            DepResult::Success(ds) => {
                                match ds {
                                    DataSource::Field(f) => {
                                        let v = f.parse(fm).unwrap();
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
                                    },
                                    DataSource::Search{start, n, find} => {
                                        let bre = BinRegex::new(&find).unwrap();
                                        let span = BitIndex::bytes(100); // TODO: This only searches the next 100 bytes at the moment...
                                        let end = start + span;
                                        let mut buffer = vec![0; end.ceil().byte() - start.byte()];
                                        fm.get_bytes(start.byte(), &mut buffer).unwrap();
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
                                todo!("Field does not exist?")
                            }
                        }

                        continue;

                    },
                    AbstractIdent::FieldAttr(ref fai) => {
                        info!("Target is a Field");

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
                            },
                            DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                                info!("Lookup incomplete");
                                let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                                info!("Adding dependencies for {}: {:?}", target, deps);
                                dg.add_dependancies(target, deps.into_iter().collect());
                            },
                            DepResult::DoesNotExist => {
                                todo!("Field Attribute does not exist?");
                                
                                
                            }
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
                                },
                                StructureAttrType::Start => {
                                    vd.insert_struct_attr(sai.clone(), ExprValue::Position(BitIndex::zero()));
                                },
                                _ => panic!("Invalid file property: {}", sai.attr)
                            }
                            
                        } else {
                            //let struct_id = StructureIdent::new(structure_match.as_str().to_string());
                            let mut child = prs_map.get(&sai.structure).unwrap().clone();

                            loop {
                                let dr = child.borrow_mut().try_lookup(target.clone(), &vd, &mut prs_map).unwrap();
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
                                            } else if matches!(self.prs.borrow().length, LengthPolicy::Expand) {
                                                if target == self.prs.borrow().id.get_attr_ident(StructureAttrType::Length).to_abstract() {
                                                    let start_id = self.prs.borrow().id.get_attr_ident(StructureAttrType::Start);
                                                    if let Some(start) = vd.lookup_struct_attr(&start_id) {
                                                        let start = start.clone().expect_position().unwrap();
                                                        let end = BitIndex::bytes(fm.len());
                                                        //dg.lookup_values.insert(target, ExprValue::Position(end - start));
                                                        vd.insert_struct_attr(sai.clone(), ExprValue::Position(end - start))
                                                    } else {
                                                        dg.add_dependancies(target, vec![start_id.to_abstract()]);
                                                    }
                                                    
                                                } else {
                                                    panic!("Invalid file property: {}", target)
                                                }
                                            } else {
                                                panic!("Invalid file property: {}", target)
                                            }
                                            break;
                                        } else {
                                            child = prs_map.get(&new_child_id).unwrap().clone();
                                        }
                                        
                                    }
                                }
                            }
                        }
                    },
                    AbstractIdent::Field(ref f) => {
                        panic!("Invalid target: {}", target);
                    }
                }


            }

            targets = dg.get_resolvable_unknowns(&vd);//Vec::new();

            info!("{:?}", targets);
            if targets.is_empty() {
                break;
            }
        }

        self.dep_graph = dg;
        self.value_dict = vd;
        self.prs_map = prs_map;
    }

    pub fn initialize(&mut self, fm: &mut crate::FileManager) {
        let mut vd = std::mem::take(&mut self.value_dict);
        let (unk0, unk1) = self.prs.borrow().unknowns(&vd).unwrap();
        self.value_dict = vd;

        info!("Unknowns 0: {:?}", unk0);
        info!("Unknowns 1: {:?}", unk1);

        let mut targets: Vec<AbstractIdent> = unk0.into_iter().collect();
        targets.extend(unk1);

        let mut interval = 0;
        loop {
            let prev_prs_map: HashSet<StructureIdent> = self.prs_map.keys().cloned().collect();

            self.get_data(targets, fm);

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

            targets = self.dep_graph.get_resolvable_unknowns(&self.value_dict);

            if targets.is_empty() {
                break;
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
    let s = std::fs::read_to_string("png-spec.xml").unwrap();
    parse_xml(&s).unwrap()
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

    fn start_file<'a>(filename: &'a str) -> (FileMap<'a>, crate::FileManager<'a>) {
        // pretty_env_logger::init();
        let s = Rc::new(make_png());
        let mut png = FileMap::new(s);
        let mut fm = crate::FileManager::new(filename.to_string(), crate::FileManagerType::ReadOnly, false).unwrap();
        png.initialize(&mut fm);
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

        png.get_data(targets.clone(), fm);

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

        png.get_data(targets.clone(), fm);

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

        png.get_data(targets.clone(), fm);

        assert_eq!(png.value_dict.lookup_any(&plte_reps).unwrap(), ExprValue::Integer(rgb.len() as i64));

        let mut targets = vec![];
        for i in 0..rgb.len() {
            let plte_pallette = StructureIdent::new_indexed("plte_pallette".to_string(), vec![1, index, 1, 0, i]);
            targets.push(plte_pallette.clone().get_field_ident("plte_red".to_string()).to_abstract());
            targets.push(plte_pallette.clone().get_field_ident("plte_green".to_string()).to_abstract());
            targets.push(plte_pallette.clone().get_field_ident("plte_blue".to_string()).to_abstract());
        }

        png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

                assert_eq!(png.value_dict.lookup_any(&trns_reps).unwrap(), ExprValue::Integer(v.len() as i64));

                let mut targets = vec![];
                for i in 0..v.len() {
                    let trns_pallette = StructureIdent::new_indexed("trns_pallette".to_string(), vec![1, index, 1, 0, 0, i]);
                    let trns_alpha = trns_pallette.get_field_ident("trns_alpha".to_string()).to_abstract();
                    targets.push(trns_alpha)
                }

                png.get_data(targets.clone(), fm);

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

        png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

                png.get_data(targets.clone(), fm);

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

        png.get_data(targets.clone(), fm);

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

        png.get_data(targets.clone(), fm);

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

        png.get_data(targets.clone(), fm);

        assert_eq!(png.value_dict.lookup_any(&hist_reps).unwrap(), ExprValue::Integer(freqs.len() as i64));

        let mut targets = vec![];
        for i in 0..freqs.len() {
            let hist_entry = StructureIdent::new_indexed("hist_entry".to_string(), vec![1, index, 1, 0, i]);
            targets.push(hist_entry.get_field_ident("hist_freq".to_string()).to_abstract());
        }

        png.get_data(targets.clone(), fm);

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

        png.get_data(targets.clone(), fm);

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

        png.get_data(targets.clone(), fm);

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
            png.get_data(targets.clone(), fm);
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
                        png.get_data(targets.clone(), fm);
                        assert_eq!(png.value_dict.lookup_any(&ifd_entry_addr).unwrap(), ExprValue::Integer(addr as i64));
                        assert_eq!(png.value_dict.lookup_any(&ifd_entry_value).unwrap(), ExprValue::String(s.clone()));
                    } else {
                        // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_ascii_body".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0]);
                        let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_ascii_body".to_string(), vec![0, 0]);
                        targets.push(ifd_entry_data.get_field_ident("exif_ifd_ascii".to_string()).to_abstract());
                        png.get_data(targets.clone(), fm);
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::String(s.clone()));
                    }
                },
                ExifIfdData::UShort(v) => {
                    if let Some(addr) = ifd.address {
                        let ifd_entry_addr_section = ifd_entry_body.get_child_ident("exif_ifd_address_u16_section".to_string(), vec![0, 0, 0]);
                        let ifd_entry_addr = ifd_entry_addr_section.get_field_ident("exif_ifd_address_u16".to_string()).to_abstract();
                        targets.push(ifd_entry_addr);
                        for (j, x) in v.iter().enumerate() {
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_u16_body_addressed".to_string(), vec![0, 0, 1, 0, j]);
                            let ifd_entry_value = ifd_entry_data.get_field_ident("exif_ifd_u16".to_string()).to_abstract();
                            targets.push(ifd_entry_value);
                        }
                        png.get_data(targets.clone(), fm);
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::Integer(addr as i64));
                        for (target, x) in targets.iter().skip(1).zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target).unwrap(), ExprValue::Integer(*x as i64));
                        }
                    } else {
                        for (j, x) in v.iter().enumerate() {
                            let field_tag = match ifd.tag {
                                259 => "exif_compression".to_string(),
                                _ => "exif_ifd_u16".to_string()
                            };
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_u16_body".to_string(), vec![0, 0, 0, j]);
                            targets.push(ifd_entry_data.get_field_ident(field_tag).to_abstract());
                        }
                        png.get_data(targets.clone(), fm);
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
                        for (j, x) in v.iter().enumerate() {
                            // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_u32_body_addressed".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 1, 0, j]);
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_u32_body_addressed".to_string(), vec![0, 0, 1, 0, j]);
                            let ifd_entry_value = ifd_entry_data.get_field_ident("exif_ifd_u32".to_string()).to_abstract();
                            targets.push(ifd_entry_value);
                        }
                        png.get_data(targets.clone(), fm);
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::Integer(addr as i64));
                        for (target, x) in targets.iter().skip(1).zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target).unwrap(), ExprValue::Integer(*x as i64));
                        }
                    } else {
                        for (j, x) in v.iter().enumerate() {
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
                        png.get_data(targets.clone(), fm);
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
                        for (j, x) in v.iter().enumerate() {
                            // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_urat_body_addressed".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 1, 0, j]);
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_urat_body_addressed".to_string(), vec![0, 0, 1, 0, j]);
                            let ifd_num_entry_value = ifd_entry_data.get_field_ident("exif_ifd_urat_num".to_string()).to_abstract();
                            let ifd_den_entry_value = ifd_entry_data.get_field_ident("exif_ifd_urat_den".to_string()).to_abstract();
                            targets.push(ifd_num_entry_value);
                            targets.push(ifd_den_entry_value);
                        }
                        png.get_data(targets.clone(), fm);
                        assert_eq!(png.value_dict.lookup_any(&targets[0]).unwrap(), ExprValue::Integer(addr as i64));
                        for (target, (num, den)) in targets[1..].chunks(2).zip(v.iter()) {
                            assert_eq!(png.value_dict.lookup_any(&target[0]).unwrap(), ExprValue::Integer(*num as i64));
                            assert_eq!(png.value_dict.lookup_any(&target[1]).unwrap(), ExprValue::Integer(*den as i64));
                        }
                    } else {
                        for (j, x) in v.iter().enumerate() {
                            // let ifd_entry_data = StructureIdent::new_indexed("exif_ifd_urat_body".to_string(), vec![1, index, 1, 0, 1, 0, 1, i, 1, 0, 0, 0, j]);
                            let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_urat_body".to_string(), vec![0, 0, 0, j]);
                            targets.push(ifd_entry_data.get_field_ident("exif_ifd_urat_num".to_string()).to_abstract());
                            targets.push(ifd_entry_data.get_field_ident("exif_ifd_urat_den".to_string()).to_abstract());
                        }
                        png.get_data(targets.clone(), fm);
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
                        png.get_data(targets.clone(), fm);
                        assert_eq!(png.value_dict.lookup_any(&ifd_entry_addr).unwrap(), ExprValue::Integer(addr as i64));
                        assert_eq!(png.value_dict.lookup_any(&ifd_entry_value).unwrap(), ExprValue::Bytes(bf));
                    } else {
                        let ifd_entry_data = ifd_entry_body.get_child_ident("exif_ifd_unk_body".to_string(), vec![0, 0]);
                        targets.push(ifd_entry_data.get_field_ident("exif_ifd_unk".to_string()).to_abstract());
                        png.get_data(targets.clone(), fm);
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

pub fn parse_xml(input: &str) -> Result<Structure, ParseXmlError> {
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
                                // let message = format!("Element end '{}' does not match start '{}'", sspan2.as_str(), name);
                                // return Err(ParseXplError {
                                //     details: message.to_string(),
                                //     error_type: ParseXplErrorType::MalformedXml,
                                //     index: vec![(name.start(), name.end(), Some("Element start is here".to_string())), 
                                //                 (sspan2.start(), sspan2.end(), Some("Element end is here".to_string()))],
                                //     help_text: None
                                // });
                                //self.malformed_xml.handle_error(ErrorAcceptanceLevel::Compliant, message.as_str())?;
                                // panic!("{}", message);
                                panic!("Malformed XML")
                            },
                            Some((mut obj, _)) => {
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
                // match err {
                //     xmlparser::Error::InvalidAttribute(stream_err, pos) => {
                //         match stream_err {
                //             xmlparser::StreamError::InvalidSpace(c, pos) => {
                //                 let i = get_index_from_row_col(input, pos.row as usize, pos.col as usize);
                //                 return Err(ParseXplError {
                //                     details: format!("Encountered unexpected character: '{}' (expected space)", c as char).to_string(),
                //                     error_type: ParseXplErrorType::MalformedXml,
                //                     index: vec![(i, i+1, None)],
                //                     help_text: None
                //                 })
                //             },
                //             xmlparser::StreamError::InvalidChar(c1, c2, pos) => {
                //                 let i = get_index_from_row_col(input, pos.row as usize, pos.col as usize);
                //                 return Err(ParseXplError {
                //                     details: format!("Encountered unexpected character: '{}' (expected '{}')", c1 as char, c2 as char).to_string(),
                //                     error_type: ParseXplErrorType::MalformedXml,
                //                     index: vec![(i, i+1, None)],
                //                     help_text: None
                //                 })
                //             },
                //             _ => todo!("{:?}", stream_err)
                //         }
                //     },
                //     _ => todo!("{:?}", err)
                // }
                todo!("{:?}", err)
                // panic!("Malformed XML");
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
        panic!("Encountered EOF with unclosed elements");
    }

    if objects.len() == 1 {
        Structure::from_xml_object(objects.into_iter().nth(0).unwrap())
    } else {
        panic!("Wrong number of objects in XML!")
    }
    // Ok(svg)
}