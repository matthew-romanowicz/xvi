use std::collections::HashSet;
use std::str::FromStr;
use std::str::CharIndices;

use log::*;
use bitutils2::{BitIndex, BitField};

use lazy_static::lazy_static;
lazy_static! {
    static ref EXPR_ARG_RE: regex::Regex = regex::Regex::new(r"^\$[0-9]+").unwrap();
    static ref EXPR_VAR_RE: regex::Regex = regex::Regex::new(r"^[_a-zA-Z][_a-zA-Z0-9\.]*").unwrap();
    static ref EXPR_INT_RE: regex::Regex = regex::Regex::new(r"^[0-9]+").unwrap();
    static ref EXPR_BP_RE: regex::Regex = regex::Regex::new(r"[\-\+]?(([0-9]+B)?[0-9]+b)|([0-9]+B)").unwrap();
    static ref EXPR_FLOAT_RE: regex::Regex = regex::Regex::new(r"^[0-9]*((\.[0-9]*)|((\.[0-9]*)?([eE][\-\+]?[0-9]+)))").unwrap();
    static ref EXPR_OP_RE: regex::Regex = regex::Regex::new(r"^[^0-9a-zA-Z_ \$]+").unwrap();
}

#[derive(Clone, Debug, PartialEq)]
pub struct MyStrSpan<'a> {
    start: usize,
    text: &'a str
}

impl<'a> MyStrSpan<'a> {
    pub fn new(start: usize, text: &'a str) -> MyStrSpan<'a> {
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
pub enum Expr {
    Op{op: ExprOp, args: Vec<Expr>},
    Value(ExprValue),
    Var(String),
    Arg(usize),
    Empty
}

impl Expr {
    pub fn evaluate(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<ExprValue, ExprEvalError> {
        self.evaluate_with_args(&vec![], lookup)
    }

    pub fn evaluate_with_args(&self, arguments: &Vec<ExprValue>, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<ExprValue, ExprEvalError> {
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

    pub fn evaluate_expect_position(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BitIndex, ExprEvalError> {
        match self.evaluate(lookup)? {
            ExprValue::Position(bp) => Ok(bp),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "Position".to_string()})
        }
    }

    pub fn evaluate_expect_integer(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<i64, ExprEvalError> {
        self.evaluate(lookup)?.expect_integer()
    }

    pub fn evaluate_expect_bool(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<bool, ExprEvalError> {
        match self.evaluate(lookup)? {
            ExprValue::Bool(b) => Ok(b),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "Bool".to_string()})
        }
    }

    pub fn vars(&self) -> HashSet<String> {
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

    pub fn from_str_span(input: MyStrSpan) -> Result<Expr, ParseExprError> {
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