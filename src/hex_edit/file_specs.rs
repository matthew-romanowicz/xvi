use std::str::FromStr;
use std::str::CharIndices;

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

use std::ops::Neg;
use std::cmp::Ord;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Sign {
    Negative,
    Positive,
}


impl std::ops::Neg for Sign {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Sign::Negative => Sign::Positive,
            Sign::Positive => Sign::Negative,
        }
    }
}

#[derive(Clone, Debug)]
struct ParseBitPositionError {
    details: String
}

impl ParseBitPositionError {
    fn new(details: String) -> ParseBitPositionError {
        ParseBitPositionError {details}
    }
}

impl std::fmt::Display for ParseBitPositionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.details)
    }    
}

impl From<std::num::ParseIntError> for ParseBitPositionError {
    fn from(err: std::num::ParseIntError) -> Self {
        ParseBitPositionError {
            details: format!("Error parsing integer: {}", err).to_string()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BitPosition {
    sign: Sign,
    byte: u64,
    bit: u8
}

impl BitPosition {
    fn new(byte: u64, bit: u8) -> BitPosition {
        BitPosition {sign: Sign::Positive, byte, bit}
    }

    fn is_zero(&self) -> bool {
        self.byte == 0 && self.bit == 0
    }

    fn zero() -> BitPosition {
        BitPosition::new(0, 0)
    }

    fn bytes(byte: u64) -> BitPosition {
        BitPosition::new(byte, 0)
    }

    fn rem_euclid(&self, rhs: &BitPosition) -> BitPosition {
        let mut lhs_total_bits = self.byte as i128 * 8 + self.bit as i128;
        let mut rhs_total_bits = rhs.byte as i128 * 8 + rhs.bit as i128;

        if matches!(self.sign, Sign::Negative) {
            lhs_total_bits *= -1;
        }

        if matches!(rhs.sign, Sign::Negative) {
            rhs_total_bits *= -1;
        }

        let mut total_bits = lhs_total_bits.rem_euclid(rhs_total_bits);
        if total_bits < 0 {
            total_bits *= -1;
            -BitPosition::new((total_bits / 8) as u64, (total_bits % 8) as u8)
        } else {
            BitPosition::new((total_bits / 8) as u64, (total_bits % 8) as u8)
        }

    }
}



impl FromStr for BitPosition {
    type Err = ParseBitPositionError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let bp_re = regex::Regex::new(r"^(?<sign>[\+\-])?((?<byte>[0-9]+)B)?((?<bit>[0-9]+)b)?$").unwrap();
        if let Some(caps) = bp_re.captures(s) {
            let mut valid = false;
            let mut byte = match caps.name("byte") {
                Some(m) => {
                    valid = true;
                    u64::from_str(m.as_str())?
                },
                None => 0
            };
            let bit = match caps.name("bit") {
                Some(m) => {
                    valid = true;
                    u64::from_str(m.as_str())?
                },
                None => 0
            };
            if !valid {
                ParseBitPositionError::new("Value must be supplied for byte or bit".to_string());
            }
            let sign = match caps.name("sign") {
                Some(m) => match m.as_str() {
                    "+" => Sign::Positive,
                    "-" => Sign::Negative,
                    _ => unreachable!()
                },
                None => Sign::Positive
            };
            byte += bit >> 3;
            let bit = (bit & 0b111) as u8;
            Ok(BitPosition{sign, byte, bit})
        } else {
            Err(ParseBitPositionError::new(format!("Invalid format for bit position: '{}'", s).to_string()))
        }
    }
}

impl std::cmp::PartialOrd for BitPosition {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for BitPosition {
    fn cmp(&self, other: &BitPosition) -> std::cmp::Ordering {
        match (&self.sign, &other.sign) {
            (Sign::Positive, Sign::Positive) => {
                match self.byte.cmp(&other.byte) {
                    std::cmp::Ordering::Equal => {
                        self.bit.cmp(&other.bit)
                    },
                    other_cmp => other_cmp
                }
            },
            (Sign::Positive, Sign::Negative) => {
                std::cmp::Ordering::Greater
            },
            (Sign::Negative, Sign::Negative) => {
                std::cmp::Ordering::Less
            },
            (Sign::Negative, Sign::Positive) => {
                (-self).cmp(&-other).reverse()
            }
        }
    }
}

impl std::ops::Neg for BitPosition {
    type Output = BitPosition;
    fn neg(self) -> Self::Output {
        BitPosition {
            sign: -self.sign, 
            byte: self.byte, 
            bit: self.bit
        }
    }
}

impl std::ops::Neg for &BitPosition {
    type Output = BitPosition;
    fn neg(self) -> Self::Output {
        BitPosition {
            sign: -self.sign.clone(), 
            byte: self.byte, 
            bit: self.bit
        }
    }
}

impl std::ops::Add<BitPosition> for BitPosition {
    type Output = BitPosition;
    fn add(self, rhs: BitPosition) -> Self::Output {
        match (&self.sign, &rhs.sign) {
            (Sign::Positive, Sign::Positive) => {
                let bits = self.bit as u64 + rhs.bit as u64;
                BitPosition::new(self.byte + rhs.byte + bits / 8, (bits % 8) as u8)
            },
            (Sign::Positive, Sign::Negative) => {
                // 5B2b - 4B3b
                let mut bits = self.bit as i64 - rhs.bit as i64;
                let mut lhs_byte = self.byte;
                let mut rhs_byte = rhs.byte;
                if bits >= 0 {
                    lhs_byte += bits as u64 / 8;
                } else {
                    rhs_byte += bits.abs() as u64 / 8;
                };

                if lhs_byte > rhs_byte {
                    if bits >= 0 {
                        BitPosition::new(lhs_byte - rhs_byte, (bits % 8) as u8)
                    } else {
                        BitPosition::new(lhs_byte - rhs_byte - 1, ((bits + 8) % 8) as u8)
                    }
                    
                } else if lhs_byte < rhs_byte {
                    if bits <= 0 {
                        -BitPosition::new(rhs_byte - lhs_byte, (bits.abs() % 8) as u8)
                    } else {
                        -BitPosition::new(rhs_byte - (lhs_byte + 1), ((bits - 8).abs() % 8) as u8)
                    }
                } else {
                    if bits < 0 {
                        -BitPosition::new(0, (bits.abs() % 8) as u8)
                    } else {
                        BitPosition::new(0, (bits % 8) as u8)
                    }
                }
            },
            (Sign::Negative, Sign::Negative) => {
                (self.neg() + rhs.neg()).neg()
            },
            (Sign::Negative, Sign::Positive) => {
                (self.neg() + rhs.neg()).neg()
            },
        }
        
    }
}

impl std::ops::Sub<BitPosition> for BitPosition {
    type Output = BitPosition;
    fn sub(self, rhs: BitPosition) -> Self::Output {
        self + -rhs
        
    }
}

impl<'a, 'b> std::ops::Add<&'a BitPosition> for &'b BitPosition {
    type Output = BitPosition;
    fn add(self, rhs: &'a BitPosition) -> Self::Output {
        self.clone() + rhs.clone()
        
    }
}

impl<'a, 'b> std::ops::Sub<&'a BitPosition> for &'b BitPosition {
    type Output = BitPosition;
    fn sub(self, rhs: &'a BitPosition) -> Self::Output {
        self.clone() + (-rhs).clone()
        
    }
}

#[cfg(test)]
mod bitposition_tests {
    use super::*;

    #[test]
    fn parse_test() {
        assert_eq!(BitPosition::new(15, 4), BitPosition::from_str("15B4b").unwrap());
        assert_eq!(BitPosition::new(1300, 4), BitPosition::from_str("1300B4b").unwrap());
        assert_eq!(BitPosition::new(0, 4), BitPosition::from_str("4b").unwrap());
        assert_eq!(BitPosition::new(5, 5), BitPosition::from_str("45b").unwrap());
        assert_eq!(BitPosition::new(4, 0), BitPosition::from_str("4B").unwrap());
        assert_eq!(BitPosition::new(0, 0), BitPosition::from_str("0B").unwrap());
        assert_eq!(BitPosition::new(15, 3), BitPosition::from_str("10B43b").unwrap());
    }

    #[test]
    fn add_test() {
        assert_eq!(BitPosition::new(5, 1) + BitPosition::new(3, 2), BitPosition::new(8, 3));
        assert_eq!(BitPosition::new(5, 1) + BitPosition::new(3, 7), BitPosition::new(9, 0));
        assert_eq!(BitPosition::new(5, 3) + BitPosition::new(3, 7), BitPosition::new(9, 2));
    }

    #[test]
    fn sub_test() {
        assert_eq!(BitPosition::new(5, 1) - BitPosition::new(3, 2), BitPosition::new(1, 7));
        assert_eq!(BitPosition::new(5, 5) - BitPosition::new(5, 2), BitPosition::new(0, 3));
        assert_eq!(BitPosition::new(5, 2) - BitPosition::new(5, 5), -BitPosition::new(0, 3));
        assert_eq!(BitPosition::new(3, 2) - BitPosition::new(5, 5), -BitPosition::new(2, 3));
        assert_eq!(BitPosition::new(3, 5) - BitPosition::new(5, 3), -BitPosition::new(1, 6));
    }
}

#[derive(Debug, PartialEq)]
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
    List,
    Function(String)
}

impl ExprOp {
    fn priority(&self) -> u8 {
        match self {
            ExprOp::Function(_) => 0,
            ExprOp::Mult => 1,
            ExprOp::Div => 1,
            ExprOp::Add => 2,
            ExprOp::Sub => 2,
            ExprOp::Eq => 3,
            ExprOp::List => 5,
            _ => todo!()
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
                // let mut args_iter = args.iter();
                // let init = args_iter.next().unwrap().clone();
                // Ok(args_iter.fold(init, |a, b| (a * b)))
                todo!()
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
            }
            _ => todo!()
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ExprValue {
    Integer(i64),
    Position(BitPosition),
    Bool(bool),
    String(String)
}

impl ExprValue {
    pub fn datatype_as_string(&self) -> String {
        match self {
            ExprValue::Integer(_) => "Integer".to_string(),
            ExprValue::Position(_) => "Position".to_string(),
            ExprValue::Bool(_) => "Boolean".to_string(),
            ExprValue::String(_) => "String".to_string()
        }
    }

    pub fn expect_integer(self) -> Result<i64, ExprEvalError> {
        match self {
            ExprValue::Integer(value) => Ok(value),
            other => Err(ExprEvalError::DataTypeMismatch{found: other.datatype_as_string(), expected: "Integer".to_string()})
        }
    }

    pub fn expect_position(self) -> Result<BitPosition, ExprEvalError> {
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
            ExprValue::String(v) => todo!()
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

impl<'a, 'b> std::ops::Add<&'a ExprValue> for &'b ExprValue {
    type Output = ExprValue;
    fn add(self, rhs: &'a ExprValue) -> Self::Output {
        self + rhs
        
    }
}

impl<'a, 'b> std::ops::Sub<&'a ExprValue> for &'b ExprValue {
    type Output = ExprValue;
    fn sub(self, rhs: &'a ExprValue) -> Self::Output {
        self + rhs
        
    }
}

#[derive(Debug)]
pub struct ParseExprError {
    details: String
}

impl ParseExprError {
    pub fn new(msg: String) -> ParseExprError {
        ParseExprError{details: msg}
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

    fn from_undelimited_sspan(mut sspan: MyStrSpan) -> Vec<ExprToken> {
        println!("Parsing section: {}", sspan.as_str());
        let mut tokens = Vec::<ExprToken>::new();
        while !sspan.as_str().trim().is_empty() {
            if let Some(m) = EXPR_VAR_RE.find(sspan.as_str()) {
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                // match m.as_str() {
                //     "atan2" => tokens.push(ExprToken::Operator(ExprOp::Function("atan2".to_string()), token_sspan.range())),
                //     _ => {
                //         println!("Var token: {}", m.as_str());
                //         tokens.push(ExprToken::Parsed(Expr::Var(m.as_str().to_string()), token_sspan.range()));
                //     }
                // }
                println!("Var token: {}", m.as_str());
                tokens.push(ExprToken::Var(m.as_str().to_string(), token_sspan.range()));
                sspan = sspan.slice(sspan.start() + m.end(), sspan.end()).trim();
            } else if let Some(m) = EXPR_ARG_RE.find(sspan.as_str()) {
                if let Ok(i) = usize::from_str(&m.as_str()[1..]) {
                    println!("Arg token: {}", m.as_str());
                    let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                    tokens.push(ExprToken::Parsed(Expr::Arg(i), token_sspan.range()));
                    sspan = sspan.slice(sspan.start() + m.end(), sspan.end()).trim();
                } else {
                    panic!("Unable to parse token: {}", m.as_str())
                }
            } else if let Some(m) = EXPR_BP_RE.find(sspan.as_str()) {
                println!("BitPosition token: {}", m.as_str());
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                tokens.push(ExprToken::Parsed(Expr::Value(ExprValue::Position(BitPosition::from_str(m.as_str()).unwrap())), token_sspan.range()));
                sspan = sspan.slice(sspan.start() + m.end(), sspan.end()).trim();
            } else if let Some(m) = EXPR_FLOAT_RE.find(sspan.as_str()) {
                println!("Float token: {}", m.as_str());
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                todo!();
                //tokens.push(ExprToken::Parsed(Expr::Value(ExprValue::Float(f64::from_str(m.as_str()))), token_sspan.range()));
                sspan = sspan.slice(sspan.start() + m.end(), sspan.end()).trim();
            } else if let Some(m) = EXPR_INT_RE.find(sspan.as_str()) {
                println!("Int token: {}", m.as_str());
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                tokens.push(ExprToken::Parsed(Expr::Value(ExprValue::Integer(i64::from_str(m.as_str()).unwrap())), token_sspan.range()));
                sspan = sspan.slice(sspan.start() + m.end(), sspan.end()).trim();
            } else if let Some(m) = EXPR_OP_RE.find(sspan.as_str()) {
                println!("Op token: {}", m.as_str());
                let op = match m.as_str() {
                    "+" => ExprOp::Add,
                    "-" => ExprOp::Sub,
                    "*" => ExprOp::Mult,
                    "/" => ExprOp::Div,
                    "==" => ExprOp::Eq,
                    "," => ExprOp::List,
                    _ => panic!("Unable to parse operator: '{}'", m.as_str())
                };
                let token_sspan = sspan.slice(sspan.start() + m.start(), sspan.start() + m.end());
                tokens.push(ExprToken::Operator(op, token_sspan.range()));
                sspan = sspan.slice(sspan.start() + m.end(), sspan.end()).trim();
            } else {
                println!("Unrecognized token: {}", sspan.as_str());
            }
        }

        tokens
    }

    // fn from_undelimited_sspan(sspan: MyStrSpan) -> ExprToken {
    //     match sspan.as_str().trim() {
    //         "+" => ExprToken::Operator(ExprOp::Add, sspan.range()),
    //         "-" => ExprToken::Operator(ExprOp::Sub, sspan.range()),
    //         "*" => ExprToken::Operator(ExprOp::Mult, sspan.range()),
    //         "/" => ExprToken::Operator(ExprOp::Div, sspan.range()),
    //         "==" => ExprToken::Operator(ExprOp::Eq, sspan.range()),
    //         "," => ExprToken::Operator(ExprOp::List, sspan.range()),
    //         // // TODO
    //         // "date" => ExprToken::Operator(ExprOp::DateLiteral, sspan.range()),
    //         // "cos" => ExprToken::Operator(ExprOp::Function("cos".to_string()), sspan.range()),
    //         // "sin" => ExprToken::Operator(ExprOp::Function("sin".to_string()), sspan.range()),
    //         // "atan2" => ExprToken::Operator(ExprOp::Function("atan2".to_string()), sspan.range()),
    //         // "clamp" => ExprToken::Operator(ExprOp::Function("clamp".to_string()), sspan.range()),
    //         // "sum" => ExprToken::Operator(ExprOp::Function("sum".to_string()), sspan.range()),
    //         _ => ExprToken::Unidentified(sspan.range())
    //     }
    // }
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

    fn evaluate_expect_position(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BitPosition, ExprEvalError> {
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

    fn from_tokens(input: &MyStrSpan, mut tokens: Vec<ExprToken>) -> Result<Expr, ParseExprError> {
        println!("Parsing Tokens: {:?}", tokens);

        if tokens.len() == 0 {
            return Ok(Expr::Empty);
        } else if tokens.len() == 1 {
            match tokens.pop().unwrap() {
                ExprToken::Delimited(rng) => {
                    let sspan = MyStrSpan::from_range(input, rng);
                    return Expr::from_str_span(sspan)
                },
                ExprToken::Operator(rng, _) => {
                    panic!("Unexpected operator encountered")
                },
                ExprToken::Parsed(expr, _) => {
                    return Ok(expr)
                },
                ExprToken::Var(s, _) => {
                    return Ok(Expr::Var(s))
                }
            }
        } else if tokens.len() == 2 {
            let token_1 = tokens.pop().unwrap();
            let token_0 = tokens.pop().unwrap();
            match (token_0, token_1) {
                (ExprToken::Var(s, _), ExprToken::Delimited(rng)) => {
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
                (token_0, token_1) => tokens = vec![token_0, token_1]
            }
        }

        let mut op_priority = 0;
        let mut op_present = false;
        for token in &tokens {
            if let ExprToken::Operator(op, _) = token {
                op_priority = std::cmp::max(op_priority, op.priority());
                op_present = true;
            }
        }
        if !op_present {
            return Err(ParseExprError::new("Expression does not contain operator".to_string()))
        }

        //let mut token_groups = Vec::<Vec<ExprToken>::new();
        let mut current_group = Vec::<ExprToken>::new();
        let mut current_op: Option<ExprOp> = None;
        let mut expr_args = Vec::<Expr>::new();

        let mut token_iter = tokens.into_iter().rev();
        let mut token_option = token_iter.next();

        while token_option.is_some() {
            // let token = token_option.unwrap();

            match &current_op {
                None => {
                    match token_option {
                        Some(ExprToken::Operator(op, _)) if op.priority() == op_priority => {
                            expr_args.push(Expr::from_tokens(input, current_group.into_iter().rev().collect())?);
                            current_group = vec![];
                            current_op = Some(op);
                        },
                        Some(token) => {
                            current_group.push(token);
                        },
                        None => unreachable!()
                    }
                },
                Some(current_op) => {
                    match &token_option {
                        Some(ExprToken::Operator(op, _)) if op == current_op => {
                            expr_args.push(Expr::from_tokens(input, current_group.into_iter().rev().collect())?);
                            current_group = vec![];
                        },
                        Some(ExprToken::Operator(op, _)) if op.priority() == op_priority => {
                            current_group.push(token_option.unwrap());
                            current_group.extend(token_iter.collect::<Vec<ExprToken>>());
                            expr_args.push(Expr::from_tokens(input, current_group.into_iter().rev().collect())?);
                            current_group = vec![];
                            break;
                        },
                        Some(_) => {
                            current_group.push(token_option.unwrap());
                        },
                        None => unreachable!()
                    }
                }
            }

            token_option = token_iter.next();
        }

        // let empty = current_group.is_empty();
        if !current_group.is_empty() {
            expr_args.push(Expr::from_tokens(input, current_group.into_iter().rev().collect())?);
        }

        expr_args = expr_args.into_iter().rev().collect();


        Ok(Expr::Op{op: current_op.unwrap(), args: expr_args})
    }

    fn tokens_from_str_span(input: &MyStrSpan) -> Result<Vec<ExprToken>, ParseExprError> {
        println!("Input: {}", input.as_str());
        // Regex matches any of the following:
        //      Variables/Constants: A letter or underscore followed by any combination of letters, digits, and underscores
        //      Integers/Floats: Either of the following, which may optionally be followed by e or E, possibly +/-, then one or more digits
        //          0 or more digits, a decimal point, then zero or more digits
        //          1 or more digits
        //      Operators: Any combination of one or more characters that are not digits, letters, underscores, or spaces.
        // let token_re = regex::Regex::new(r"^(([\$_a-zA-Z][_a-zA-Z0-9\.]*)|([0-9]*([0-9]|(\.[0-9]*))([eE][\-\+]?[0-9]+)?)|([^0-9a-zA-Z_ \$']+)|('[^']*')|([\+\-]?([0-9]+[Bb])|([0-9]+B[0-9]+b)))").unwrap();
        // assert!(token_re.is_match("5B3b"));


        let mut delimiter_stack = Vec::<MyStrSpan>::new();
        let mut in_string_literal = false;
        let mut escaped = false;
        let mut tokens = Vec::<ExprToken>::new();
        let mut current_token_start = input.start();
        // let mut was_token_match = false;

        let mut chars_iter = input.chars();

        while let Some(ch) = chars_iter.next() {
            let ch_str = ch.as_str();
            // println!("ch: {}", ch_str);
            match ch_str {
                "'" => if !escaped {
                    if in_string_literal {
                        in_string_literal = false;
                        let string_literal_sspan = input.slice(current_token_start, ch.start());
                        println!("String literal token: '{}'", string_literal_sspan.as_str());
                        tokens.push(ExprToken::Parsed(Expr::Value(ExprValue::String(string_literal_sspan.as_str().to_string())), string_literal_sspan.range()));
                    } else {
                        let mut undelimited_sspan = input.slice(current_token_start, ch.start()).trim();
                        tokens.extend(ExprToken::from_undelimited_sspan(undelimited_sspan));
                        in_string_literal = true;
                    }
                    current_token_start = ch.end();
                },                
                "(" | "[" | "{" if !in_string_literal => {
                    if delimiter_stack.is_empty() {
                        // let token_sspan = MyStrSpan::new(current_token_start, &input.as_str()[current_token_start..ch.start()]);
                        let mut undelimited_sspan = input.slice(current_token_start, ch.start()).trim();
                        tokens.extend(ExprToken::from_undelimited_sspan(undelimited_sspan));
                        // if !undelimited_sspan.as_str().trim().is_empty() {
                        //     while !undelimited_sspan.as_str().trim().is_empty() {
                        //         if let Some(m) = token_re.find(undelimited_sspan.as_str()) {
                        //             let token_sspan = undelimited_sspan.slice(undelimited_sspan.start() + m.start(), undelimited_sspan.start() + m.end());
                        //             println!("Token: '{}'", token_sspan.as_str());
                        //             tokens.push(ExprToken::from_undelimited_sspan(token_sspan));
                        //             undelimited_sspan = undelimited_sspan.slice(undelimited_sspan.start() + m.end(), undelimited_sspan.end()).trim();
                        //         } else {
                        //             panic!("Unrecognized token: {}", undelimited_sspan.as_str());
                        //         }
                        //     }
                        //     // tokens.push(ExprToken::from_undelimited_sspan(token_sspan));
                        // }
                        current_token_start = ch.end();
                        // was_token_match = false;
                    }
                    delimiter_stack.push(ch)
                },
                ")" | "]" | "}" if !in_string_literal => match (ch_str, delimiter_stack.pop().map(|d| d.as_str())) {
                    (")", Some("(")) | ("]", Some("[")) | ("}", Some("{")) => {
                        if delimiter_stack.is_empty() {
                            //let token_sspan = MyStrSpan::new(current_token_start, &input.as_str()[current_token_start..ch.start()]);
                            let token_sspan = input.slice(current_token_start, ch.start());
                            println!("Delimited: '{}'", token_sspan.as_str());
                            tokens.push(ExprToken::Delimited(token_sspan.range()));
                            current_token_start = ch.end();
                            // was_token_match = false;
                        }
                    },
                    (_, None) => return Err(ParseExprError::new("Mismatched delimiters 1".to_string())),
                    (_, Some(d)) => return Err(ParseExprError::new(format!("Mismatched delimiters: {}, {}", ch_str, d).to_string()))
                },
                _ if delimiter_stack.is_empty() => {
                    // let token_str = input.slice(current_token_start, ch.end()).as_str();


                    // if token_re.is_match(token_str.trim()) {
                    //     was_token_match = true;
                    // } else if was_token_match {
                    //     let token_sspan = input.slice(current_token_start, ch.start()).trim();
                    //     if !token_sspan.as_str().is_empty() {
                    //         println!("Token: '{}'", token_sspan.as_str());
                    //         tokens.push(ExprToken::from_undelimited_sspan(token_sspan));
                    //     }
                    //     current_token_start = ch.start();
                    //     was_token_match = false;
                        
                    // }
                },
                _ => {
                    // Do nothing
                }

            }
        }

        if in_string_literal {
            panic!("Encountered end of input while parsing string literal")
        }

        if delimiter_stack.is_empty() {
            // let token_sspan = input.slice(current_token_start, input.end()).trim();
            // if !token_sspan.as_str().is_empty() {
            //     tokens.push(ExprToken::from_undelimited_sspan(token_sspan));
            // }

            let mut undelimited_sspan = input.slice(current_token_start, input.end()).trim();
            tokens.extend(ExprToken::from_undelimited_sspan(undelimited_sspan));
            // if !undelimited_sspan.as_str().trim().is_empty() {
            //     while !undelimited_sspan.as_str().trim().is_empty() {
            //         if let Some(m) = token_re.find(undelimited_sspan.as_str()) {
            //             let token_sspan = undelimited_sspan.slice(undelimited_sspan.start() + m.start(), undelimited_sspan.start() + m.end());
            //             println!("Token: '{}'", token_sspan.as_str());
            //             tokens.push(ExprToken::from_undelimited_sspan(token_sspan));
            //             undelimited_sspan = undelimited_sspan.slice(undelimited_sspan.start() + m.end(), undelimited_sspan.end()).trim();
            //         } else {
            //             panic!("Unrecognized token: {}", undelimited_sspan.as_str());
            //         }
            //     }
            //     // tokens.push(ExprToken::from_undelimited_sspan(token_sspan));
            // }
            
        } else {
            return Err(ParseExprError::new("Mismatched delimiters".to_string()));
        }
        println!("{:?}", tokens);
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
        let bp1 = Expr::Value(ExprValue::Position(BitPosition::new(5, 3)));
        assert_eq!(Expr::from_str("5B3b").unwrap(), bp1);
        let atan2 = Expr::Op{op: ExprOp::Function("atan2".to_string()), args: vec![add_arg2, Expr::Var("y".to_string())]};
        assert_eq!(Expr::from_str("atan2(3 * x + 5 * $12 * (x + 5), y)").unwrap(), atan2);
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

use crate::Endianness;
use crate::{BinaryFormat, UIntFormat};

struct UnparsedBinaryField {
    start: BitPosition,
    span: BitPosition,
    dtype: String,
    endian: Endianness
}

impl UnparsedBinaryField {
    fn parse(&self, fm: &mut crate::hex_edit::FileManager) -> std::io::Result<ExprValue> {
        assert_eq!(self.start.bit, 0);
        assert_eq!(self.span.bit, 0);
        let mut buffer = vec![0; self.span.byte as usize];
        fm.get_bytes(self.start.byte as usize, &mut buffer)?;

        if self.dtype.as_str() == "S" {
            let s = std::str::from_utf8(&buffer).unwrap().to_string();
            return Ok(ExprValue::String(s))
        }


        let format = match self.dtype.as_str() {
            "U" => BinaryFormat::UInt(match self.span.byte {
                1 => UIntFormat::U8,
                2 => UIntFormat::U16,
                4 => UIntFormat::U32,
                8 => UIntFormat::U64,
                _ => todo!()
            }),
            _ => todo!()
        };

        match format {
            BinaryFormat::UInt(UIntFormat::U32) => {
                let v = u32::from_be_bytes([buffer[0], buffer[1], buffer[2], buffer[3]]);
                //return Ok(ExprValue::Integer(v as i64))
                return Ok(ExprValue::Position(BitPosition::bytes(v as u64)))
            },
            _ => todo!()
        }
    }
}

struct FieldSpec {
    id: String,
    name: String,
    offset: Expr,
    length: Expr,
    alignment: Expr,
    alignment_base: Expr,
    dtype: String,
    endian: Endianness
}

impl FieldSpec {
    fn parse(&self, position: BitPosition, fm: &mut crate::hex_edit::FileManager, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BinaryField, ExprEvalError> {
        let alignment_base = self.alignment_base.evaluate_expect_position(lookup)?;
        let alignment = self.alignment.evaluate_expect_position(lookup)?;
        let length = self.length.evaluate_expect_position(lookup)?;
        let offset = self.offset.evaluate_expect_position(lookup)?;

        let remainder = (&position + &offset - alignment_base).rem_euclid(&alignment);
        let position = position + offset + remainder;


        todo!()
    }

    fn get_position(&self, initial_position: BitPosition, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BitPosition, ExprEvalError> {
        let alignment_base = self.alignment_base.evaluate_expect_position(lookup)?;
        let alignment = self.alignment.evaluate_expect_position(lookup)?;
        let length = self.length.evaluate_expect_position(lookup)?;
        let position = initial_position + self.offset.evaluate_expect_position(lookup)?;

        println!("alignment_base={:?}, alignment={:?} length={:?}, position={:?}", alignment_base, alignment, length, position);

        println!("diff: {:?}", &position - &alignment_base);

        let remainder = (&position - &alignment_base).rem_euclid(&alignment);

        println!("remainder: {:?}", remainder);
        if remainder.is_zero() {
            Ok(position)
        } else {
            Ok(position + alignment - remainder)
        }
    }

    fn get_length(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<BitPosition, ExprEvalError> {
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
}

enum SectionType {
    Header,
    Body,
    Footer
}

struct PositionedSectionSpec<'a> {
    parent: &'a SectionSpec,
    position: BitPosition, // Position == start since alignment is already done
    resolved_length: BitPosition,
    field_offsets: Vec<BitPosition>,
    field_lengths: Vec<BitPosition>
}

impl<'a> PositionedSectionSpec<'a> {
    pub fn new(position: BitPosition, parent: &'a SectionSpec) -> PositionedSectionSpec {
        let n_fields = parent.fields.len();
        PositionedSectionSpec {
            parent,
            position,
            resolved_length: BitPosition::new(0, 0),
            field_offsets: Vec::new(),
            field_lengths: Vec::new()
        }
    }

    fn get_position(&self) -> BitPosition { 
        self.position.clone()
    }

    fn try_get_position(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        return Ok(DependencyReport::success(ExprValue::Position(self.position.clone())))

    }

    fn contains_value(&self, key: String) -> bool {
        self.parent.contains_value(key)
    }

}

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

    fn get_field_dtype(&self, field_id: String) -> Result<(String, Endianness), ()> {
        match self.id_map.get(&field_id) {
            Some(index) => Ok((self.fields[*index].dtype.clone(), self.fields[*index].endian)),
            None => Err(()) 
        }
    }

    /// Returns the actual start position of the structure given the "desired" position. This
    /// primarily is intended to account for alignment.
    fn try_get_start(&self, position: BitPosition, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<BitPosition>, ExprEvalError> {
        todo!()
    }

    fn contains_value(&self, key: String) -> bool {
        self.id_map.contains_key(&key)
    }

    fn try_get_length(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<BitPosition>, ExprEvalError> {
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

    fn try_get_end(&self, position: BitPosition, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<Option<BitPosition>, ExprEvalError> {
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
            "var1" => Some(ExprValue::Position(BitPosition::new(4, 0))),
            "var2" => Some(ExprValue::Position(BitPosition::new(2, 0))),
            "var3" => Some(ExprValue::Position(BitPosition::new(8, 0))),
            _ => None
        }
    }

    #[test]
    fn simple() {
        let f1 = FieldSpec {
            id: "f1".to_string(),
            name: "Field 1".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::new(0, 0))),
            length: Expr::Value(ExprValue::Position(BitPosition::new(4, 0))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::new(1, 0))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::new(0, 0))),
            dtype: "u32".to_string(),
            endian: Endianness::Big
        };

        let f2 = FieldSpec {
            id: "f2".to_string(),
            name: "Field 2".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::new(0, 0))),
            length: Expr::Var("var1".to_string()),
            alignment: Expr::Value(ExprValue::Position(BitPosition::new(1, 0))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::new(0, 0))),
            dtype: "u32".to_string(),
            endian: Endianness::Big
        };

        let f3 = FieldSpec {
            id: "f3".to_string(),
            name: "Field 3".to_string(),
            offset: Expr::Var("var2".to_string()),
            length: Expr::Var("var3".to_string()),
            alignment: Expr::Value(ExprValue::Position(BitPosition::new(1, 0))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::new(0, 0))),
            dtype: "u64".to_string(),
            endian: Endianness::Big
        };

        let fields = vec![f1, f2, f3];

        let mut id_map = HashMap::new();
        id_map.insert("f1".to_string(), 0);
        id_map.insert("f2".to_string(), 1);
        id_map.insert("f3".to_string(), 2);

        let ss = Rc::new(SectionSpec {
            length: LengthPolicy::FitContents,
            section_type: SectionType::Body,
            fields,
            id_map
        });

        let mut section = PositionedSectionSpec::new(BitPosition::new(10, 0),&ss);

        // assert_eq!(section.try_resolve(&lookup1).unwrap(), 3);
        // println!("{:?}", section.field_offsets);
        // assert_eq!(section.try_get_field_address("f1").unwrap(), (BitPosition::new(10, 0), BitPosition::new(4, 0)));
        // assert_eq!(section.try_get_field_address("f2").unwrap(), (BitPosition::new(14, 0), BitPosition::new(4, 0)));
        // assert_eq!(section.try_get_field_address("f3").unwrap(), (BitPosition::new(20, 0), BitPosition::new(8, 0)));
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
struct StructureIdent {
    id: String
}

impl std::fmt::Display for StructureIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl StructureIdent {
    fn new(id: String) -> StructureIdent {
        StructureIdent{id}
    }

    fn get_field_ident(&self, field_id: String) -> FieldIdent {
        FieldIdent {structure: self.clone(), id: field_id}
    }

    fn get_attr_ident(&self, attr: StructureAttrType) -> StructureAttrIdent {
        StructureAttrIdent::new(self.clone(), attr)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct FieldIdent {
    structure: StructureIdent,
    id: String
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
    SwitchCase(usize)
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
            StructureAttrType::SwitchCase(n) => write!(f, "switch_case[{}]", n)
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
    End
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


enum PartiallyResolvedStructureType<'a> {
    UnresolvedSection{initial: &'a SectionSpec, pss: Option<PositionedSectionSpec<'a>>},
    ResolvedSection(PositionedSectionSpec<'a>),
    Addressed{position: &'a Expr, content: &'a Structure, pss: Option<Rc<RefCell<PartiallyResolvedStructure<'a>>>>},
    Sequence(Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>),
    Switch{value: &'a Expr, cases: Vec<(&'a Expr, &'a Structure)>, default: &'a Structure, pss: Option<Rc<RefCell<PartiallyResolvedStructure<'a>>>>},
    Repeat{n: &'a Expr, structure: &'a Structure, seq: Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>, finalized: bool}, // seq is used to store the actual structure instances once "n" is determined
    RepeatUntil{end: &'a Expr, structure: &'a Structure, seq: Vec<Rc<RefCell<PartiallyResolvedStructure<'a>>>>, finalized: bool},
}

struct PartiallyResolvedStructure<'a> {
    id: StructureIdent,
    parent_id: StructureIdent,
    alignment: Expr,
    alignment_base: Expr,
    // start: Option<BitPosition>,
    length: LengthPolicy,
    // end: Option<BitPosition>,
    stype: Box<PartiallyResolvedStructureType<'a>>,
    import_monikers: HashMap<String, Option<StructureIdent>>,
    exports: HashSet<String>,
    inherited_monikers: HashMap<String, FieldIdent>,
    index_path: Vec<usize>
}

impl<'a> PartiallyResolvedStructure<'a> {

    fn new(original: &'a Structure, parent_id: StructureIdent, 
            prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Rc<RefCell<PartiallyResolvedStructure<'a>>> {
        
        let id = StructureIdent::new(original.id.clone());

        let mut import_monikers = HashMap::new();
        let stype = match &original.stype {
            StructureType::Section(section) => {
                PartiallyResolvedStructureType::UnresolvedSection{initial: &section, pss: None}
            },
            StructureType::Addressed{position, content} => {
                for name in &content.exports {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::Addressed{position: &position, content: &content, pss: None}
            },
            StructureType::Sequence(seq) => {
                for structure in seq {
                    for name in &structure.exports {
                        import_monikers.insert(name.clone(), None);
                    }
                }
                
                PartiallyResolvedStructureType::Sequence(seq.iter().map(|s| PartiallyResolvedStructure::new(s, id.clone(), prs_map)).collect())
            },
            StructureType::Switch{value, cases, default} => {
                for (_, structure) in cases {
                    for name in &structure.exports {
                        import_monikers.insert(name.clone(), None);
                    }
                }
                for name in &default.exports {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::Switch{value: &value, cases: cases.iter().map(|(e, c)| (e, c)).collect(), default: &default, pss: None}
            },
            StructureType::Repeat{n, structure} => {
                for name in &structure.exports {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::Repeat{n: &n, structure: &structure, seq: Vec::new(), finalized: false}
            },
            StructureType::RepeatUntil{end, structure} => {
                for name in &structure.exports {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::RepeatUntil{end: &end, structure: &structure, seq: Vec::new(), finalized: false}
            }
        };

        let prs = Rc::new(RefCell::new(PartiallyResolvedStructure {
            id: id.clone(),
            parent_id,
            alignment: original.alignment.clone(),
            alignment_base: original.alignment_base.clone(),
            length: original.length.clone(),
            stype: Box::new(stype),
            import_monikers,
            exports: original.exports.clone(),
            inherited_monikers: HashMap::new(),
            index_path: Vec::new()
        }));

        prs_map.insert(id.clone(), prs.clone());
        prs
    }

    fn new_indexed(original: &'a Structure, parent_id: StructureIdent, index: Vec<usize>, prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Rc<RefCell<PartiallyResolvedStructure<'a>>> {
        let mut id_string = original.id.clone();
        for i in &index {
            id_string.push_str(&format!("_{}", i));
        }

        let id = StructureIdent::new(id_string);

        let mut import_monikers = HashMap::new();
        let stype = match &original.stype {
            StructureType::Section(section) => {
                PartiallyResolvedStructureType::UnresolvedSection{initial: &section, pss: None}
            },
            StructureType::Addressed{position, content} => {
                for name in &content.exports {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::Addressed{position: &position, content: &content, pss: None}
            },
            StructureType::Sequence(seq) => {
                for structure in seq {
                    for name in &structure.exports {
                        import_monikers.insert(name.clone(), None);
                    }
                }
                
                PartiallyResolvedStructureType::Sequence(seq.iter().map(|s| PartiallyResolvedStructure::new_indexed(s, id.clone(), index.clone(), prs_map)).collect())
            },
            StructureType::Switch{value, cases, default} => {
                for (_, structure) in cases {
                    for name in &structure.exports {
                        import_monikers.insert(name.clone(), None);
                    }
                }
                for name in &default.exports {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::Switch{value: &value, cases: cases.iter().map(|(e, c)| (e, c)).collect(), default: &default, pss: None}
            },
            StructureType::Repeat{n, structure} => {
                for name in &structure.exports {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::Repeat{n: &n, structure: &structure, seq: Vec::new(), finalized: false}
            },
            StructureType::RepeatUntil{end, structure} => {
                for name in &structure.exports {
                    import_monikers.insert(name.clone(), None);
                }
                PartiallyResolvedStructureType::RepeatUntil{end: &end, structure: &structure, seq: Vec::new(), finalized: false}
            }
        };

        let prs = Rc::new(RefCell::new(PartiallyResolvedStructure {
            id: id.clone(),
            parent_id,
            alignment: original.alignment.clone(),
            alignment_base: original.alignment_base.clone(),
            length: original.length.clone(),
            stype: Box::new(stype),
            import_monikers,
            exports: original.exports.clone(),
            inherited_monikers: HashMap::new(),
            index_path: index
        }));

        prs_map.insert(id.clone(), prs.clone());
        prs
    }

    fn monikers_for_child(&self) -> HashMap<String, FieldIdent> {
        let mut child_monikers = self.inherited_monikers.clone();
        for key in self.import_monikers.keys().clone() {
            child_monikers.insert(key.to_string(), self.id.get_field_ident(key.clone()));
        }
        child_monikers
    }

    fn initialize_inherited_monikers(&mut self, prs_map: &HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>,
            monikers: HashMap<String, FieldIdent>) {
        
        self.inherited_monikers = monikers;
        let child_monikers = self.monikers_for_child();
        
        match self.stype.as_mut() {
            PartiallyResolvedStructureType::Sequence(ref mut seq) => {
                for structure in seq.iter_mut() {
                    structure.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
                }
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
                    if self.import_monikers.contains_key(&s) {
                        new.insert(self.id.get_field_ident(s).to_abstract());
                    } else {
                        new.insert(AbstractIdent::from_str(&s)?);
                    }
                }
            }
        }
        Ok(new)
    }

    fn try_get_field(&self, field_id: FieldIdent, vd: &ValueDictionary,
            prs_map: &HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<UnparsedBinaryField>, ExprEvalError> {
        println!("Import monikers: {:?}", self.import_monikers);
        if self.import_monikers.contains_key(&field_id.id) {
            match &self.import_monikers[&field_id.id] {
                Some(source_id) => {
                    // Grab value from the import source
                    println!("Trying to import {} from {:?}", field_id, source_id);
                    let source = prs_map.get(source_id).unwrap();
                    let new_field_id = source_id.get_field_ident(field_id.id);
                    source.borrow().try_get_field(new_field_id, vd, prs_map)
                },
                None => {
                    let dr = self.try_resolve_import(field_id.id.clone())?;
                    match dr.result {
                        DepResult::Success(source_id) => {
                            println!("Successfully resolved import");
                            let source = prs_map.get(&source_id).unwrap();
                            let new_field_id = source_id.get_field_ident(field_id.id);
                            source.borrow().try_get_field(new_field_id, vd, prs_map)
                        },
                        DepResult::Incomplete(vars) => {
                            println!("Import incomplete");
                            let mut new_dr = DependencyReport::incomplete(vars);
                            new_dr.parents_children.extend(dr.parents_children);
                            Ok(new_dr)
                        },
                        DepResult::MightExist(vars) => {
                            println!("Import might exist: {:?}", vars);
                            let mut new_dr = DependencyReport::might_exist(vars);
                            new_dr.parents_children.extend(dr.parents_children);
                            Ok(new_dr)
                        }
                        _ => todo!()
                    }

                }
            }

        } else if field_id.structure == self.id {
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
                PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
                    match initial.get_field_dtype(field_id.id.clone()) {
                        Ok((dtype, endian)) => {
                            let field = UnparsedBinaryField {
                                start, span, dtype, endian
                            };

                            let mut dr = DependencyReport::success(field);
                            dr.add_pc_pairs(parents, HashSet::from([field_id.to_abstract()]));
                            Ok(dr)
                        },
                        Err(()) => panic!("Structure did not contain the field!!!")
                    }
                    
    
                },
                _ => panic!("Cannot get a field for a non-section structure!")
            }
        } else {
            panic!("Field {} does not exist!", field_id);

        }
    }

    fn try_lookup(&mut self, key: AbstractIdent, vd: &ValueDictionary, prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        let monikers = self.inherited_monikers.clone();
        println!("Looking for {} in {}", key, self.id);
        println!("Monikers: {:?}", monikers);
        let lookup = move |alias: &str| match monikers.get(alias) {
            Some(fi) => vd.lookup_field(fi),
            None => vd.lookup_str(alias).unwrap_or(None)
        };
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
                        StructureAttrType::SwitchCase(i) => self.try_get_switch_case(i, vd, &lookup)
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
                                println!("Trying to import {} from {:?}", fai.field.id, source_id);
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
                                        println!("Trying to import {} from {:?}", fai.field.id, source_id);
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
                        }
                    }
                } else {
                    println!("\t Searching in children");
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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {

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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {

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
                    BitPosition::zero()
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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
                let field_index = match initial.id_map.get(&field_id.id) {
                    Some(i) => i,
                    None => return Ok(DependencyReport::does_not_exist())
                };
                let field = &initial.fields[*field_index];
                match field.length.evaluate_expect_position(lookup) {
                    Ok(length) => {
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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {

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

    fn try_lookup_in_children(&mut self, key: AbstractIdent, vd: &ValueDictionary, prs_map: &mut HashMap<StructureIdent, Rc<RefCell<PartiallyResolvedStructure<'a>>>>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {

        let mut current_position_key = self.id.get_attr_ident(StructureAttrType::Position);

        match self.stype.as_mut() {
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
                
                Ok(DependencyReport::does_not_exist())

            },
            PartiallyResolvedStructureType::ResolvedSection(section) => {
                todo!()
            },
            PartiallyResolvedStructureType::Addressed{position, content, ref mut pss, ..} => {

                let position = position.clone(); // Need this for the borrow checker
                //content.try_lookup(key)
                if let Some(ref mut pss) = pss {
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
                    Ok(DependencyReport::might_exist(self.convert_expr_vars(position.vars())?))
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

            }
        }
    }

    fn try_get_switch_value(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match *(self.stype) {
            PartiallyResolvedStructureType::Switch{value, ..} => {
                let parents = self.convert_expr_vars(value.vars())?;
                match value.evaluate(lookup) {
                    Ok(v) => {

                        // let mut input = String::new();
                        // println!("Determined Chunk type is {:?}. Pausing", v);
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
        // println!("In try_get_switch_case");
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
                            // println!("Determined switch case #{} is {:?}. Pausing", i, v);
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
                        let mut prs = PartiallyResolvedStructure::new_indexed(&cases[i].1, self.id.clone(), self.index_path.clone(), prs_map);
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
                let prs = PartiallyResolvedStructure::new_indexed(&default, self.id.clone(), self.index_path.clone(), prs_map);
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
                            let mut prs = PartiallyResolvedStructure::new_indexed(structure, self.id.clone(), child_index, prs_map);
                            prs.borrow_mut().initialize_inherited_monikers(prs_map, child_monikers.clone());
                            struct_seq.push(prs);
                        }
                        *seq = struct_seq;
                        *finalized = true;
                        let mut dr = DependencyReport::success(ExprValue::Integer(v));
                        let switch_value_id = self.id.get_attr_ident(StructureAttrType::SwitchValue).to_abstract();
                        let parents = self.convert_expr_vars(n.vars())?;
                        dr.add_pc_pairs(parents, HashSet::from([switch_value_id]));
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
                if *finalized {
                    return Ok(DependencyReport::success(ExprValue::Integer(seq.len() as i64)));
                }
                let current_end_id = if seq.is_empty() {
                    start_id
                } else {
                    let last_index = seq.len() - 1;
                    seq[last_index].borrow().id.get_attr_ident(StructureAttrType::End)
                };
                

                let mut parents = HashSet::from([current_end_id.clone().to_abstract(), end_id.clone().to_abstract()]);

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
                    let prs = PartiallyResolvedStructure::new_indexed(structure, self.id.clone(), child_index, prs_map);
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
            _ => Ok(DependencyReport::does_not_exist())
        }
    }

    fn try_get_position(&self, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss: Some(section)} => {
                Ok(DependencyReport::success(ExprValue::Position(section.get_position())))
            },
            PartiallyResolvedStructureType::ResolvedSection(section) => {
                Ok(DependencyReport::success(ExprValue::Position(section.get_position())))
            },
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
            BitPosition::zero()
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

        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {

                if self.exports.contains(&key) {
                    Ok(DependencyReport::success(self.id.clone()))
                } else {
                    Ok(DependencyReport::does_not_exist())
                }
                
            },
            PartiallyResolvedStructureType::ResolvedSection(section) => {
                todo!()
            },
            PartiallyResolvedStructureType::Addressed{position, content, pss, ..} => {

                if content.exports.contains(&key) {

                    if let Some(pss) = pss {
                        pss.borrow().try_resolve_import(key)

                    } else {
                        Ok(DependencyReport::might_exist(self.convert_expr_vars(position.vars())?))
                    }
                } else {
                    Ok(DependencyReport::does_not_exist())
                }
                
            },
            PartiallyResolvedStructureType::Sequence(structures) => {
                let mut current_best = DependencyReport::does_not_exist();
                for structure in structures {
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

                Ok(current_best)
            },
            PartiallyResolvedStructureType::Switch{value, cases, default, pss} => {

                if let Some(pss) = pss {
                    // If the swich case is known, then this should be populated. It is all we need
                    pss.borrow().try_resolve_import(key)

                } else {
                    
                    if default.exports.contains(&key) {
                        let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex).to_abstract();
                        return Ok(DependencyReport::might_exist(HashSet::from([switch_index_id])))
                    }

                    for (check, case) in cases {
                        if case.exports.contains(&key) {
                            let switch_index_id = self.id.get_attr_ident(StructureAttrType::SwitchIndex).to_abstract();
                            return Ok(DependencyReport::might_exist(HashSet::from([switch_index_id])))
                        }
                    }

                    Ok(DependencyReport::does_not_exist())

                }
            },
            PartiallyResolvedStructureType::Repeat{structure, seq, finalized, ..} | PartiallyResolvedStructureType::RepeatUntil{structure, seq, finalized, ..} => {
                let mut current_best;

                if structure.exports.contains(&key) {
                    if *finalized {
                        println!("Current best is DNE");
                        current_best = DependencyReport::does_not_exist()
                    } else {
                        println!("Current best is ME");
                        let repetitions_id = self.id.get_attr_ident(StructureAttrType::Repetitions).to_abstract();
                        current_best = DependencyReport::might_exist(HashSet::from([repetitions_id]));
                    }
                } else {
                    return Ok(DependencyReport::does_not_exist())
                }

                for structure in seq {
                    let dr = structure.borrow().try_resolve_import(key.clone())?;
                    println!("result: {:?}", dr.result);
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

    /// Tries to compute the structure length based on contents. the function SHOULD NOT BE CALLED outside of 
    /// its call in try_get_length. The return value will not be valid outside of that context.
    fn try_get_contents_length(&self, vd: &ValueDictionary, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match self.stype.as_ref() {
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
                match pss {
                    Some(section) => {
                        let length_id = self.id.get_attr_ident(StructureAttrType::Length);
                        match vd.lookup_struct_attr(&length_id) {
                            Some(value) => {
                                let mut dr = DependencyReport::success(value);
                                let contents_length_id = self.id.get_attr_ident(StructureAttrType::ContentsLength).to_abstract();
                                dr.add_pc_pairs(HashSet::from([length_id.to_abstract()]), HashSet::from([contents_length_id]));
                                Ok(dr)
                            },
                            None => Ok(DependencyReport::incomplete(HashSet::from([length_id.to_abstract()])))
                        }
                    },
                    None => {
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
                                    Ok(DependencyReport::success(ExprValue::Position(BitPosition::zero())))
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
                        // let section = initial;
                        // match lookup(&section.length_id()) {
                        //     Some(value) => {
                        //         let mut dr = DependencyReport::success(value);
                        //         dr.add_pc_pairs(HashSet::from([section.length_id()]), HashSet::from([self.contents_length_id()]));
                        //         Ok(dr)
                        //     },
                        //     None => Ok(DependencyReport::incomplete(HashSet::from([section.length_id()])))
                        // }
                    }
                }

            },
            PartiallyResolvedStructureType::ResolvedSection(section) => {
                let section = section;
                let length_id = self.id.get_attr_ident(StructureAttrType::Length);
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
            PartiallyResolvedStructureType::Addressed{content, pss, ..} => {
                // let content = *content;
                if let Some(pss) = pss {
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
                } else {
                    todo!()
                }

            },
            PartiallyResolvedStructureType::Sequence(structures) => {
                let mut length = BitPosition::zero();
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
                    todo!()
                }

            },
            PartiallyResolvedStructureType::Repeat{seq, finalized, ..} => {
                let mut parents = HashSet::new();

                let mut length = BitPosition::zero();

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

            }
        }        
    }

    fn try_get_end(&self, vd: &ValueDictionary, lookup: &dyn Fn(&str) -> Option<ExprValue>) -> Result<DependencyReport<ExprValue>, ExprEvalError> {
        match *(self.stype) {
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
            PartiallyResolvedStructureType::UnresolvedSection{initial, pss} => {
                
                for field in &initial.fields {
                    let field_id = self.id.get_field_ident(field.id.clone());
                    unks.insert(field_id.clone().get_attr_ident(FieldAttrType::Start).to_abstract());
                    unks.insert(field_id.get_attr_ident(FieldAttrType::Length).to_abstract());
                }

            },
            PartiallyResolvedStructureType::ResolvedSection(section) => {
                todo!()
            },
            PartiallyResolvedStructureType::Addressed{content, pss, ..} => {
                if let Some(structure) = pss {
                    let (new_unks, new_expand_deps) = structure.borrow().unknowns(vd)?;
                    unks.extend(new_unks);
                    expand_deps.extend(new_expand_deps);
                } else {
                    // If pss is not Some(_), then we must not have the start position yet.
                    // It is needed to get more information.
                    expand_deps.insert(self.id.get_attr_ident(StructureAttrType::Start).to_abstract());
                }
                
            },
            PartiallyResolvedStructureType::Sequence(structures) => {
                for structure in structures {
                    let (new_unks, new_expand_deps) = structure.borrow().unknowns(vd)?;
                    unks.extend(new_unks);
                    expand_deps.extend(new_expand_deps);
                }
            },
            PartiallyResolvedStructureType::Switch{value, cases, default, pss} => {
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

            }
        }
        
        Ok((unks, expand_deps))
    }
}

enum StructureType {
    Section(SectionSpec),
    Addressed{position: Expr, content: Box<Structure>},
    Sequence(Vec<Structure>),
    Switch{value: Expr, cases: Vec<(Expr, Structure)>, default: Box<Structure>},
    Repeat{n: Expr, structure: Box<Structure>},
    RepeatUntil{end: Expr, structure: Box<Structure>}
}

struct Structure {
    id: String,
    alignment: Expr,
    alignment_base: Expr,
    length: LengthPolicy,
    exports: HashSet<String>,
    stype: StructureType
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

struct FileMap {
    structure: Structure,
}

impl FileMap {

    fn new(structure: Structure) -> FileMap {
        FileMap {
            structure,
        }
    }

    fn resolve_structure(&mut self, fm: &mut crate::FileManager) {

        let file_struct = StructureIdent::new("file".to_string());

        let mut prs_map = HashMap::new();
        let mut vd = ValueDictionary::new();

        let mut s = PartiallyResolvedStructure::new(&self.structure, file_struct.clone(), &mut prs_map);
        let initial_monikers = HashMap::new();
        s.borrow_mut().initialize_inherited_monikers(&prs_map, initial_monikers);

        let mut dg = DepGraph::new();
        dg.lookup_self = vec![];
        dg.lookup_path = vec![s.borrow().id.id.clone()];

        let (unk0, unk1) = s.borrow().unknowns(&vd).unwrap();

        println!("Unknowns 0: {:?}", unk0);
        println!("Unknowns 1: {:?}", unk1);

        let mut targets: Vec<AbstractIdent> = unk0.into_iter().collect();//vec![s.end_id()];
        targets.extend(unk1);

        let width_id = s.borrow().id.clone().get_field_ident("IHDR_width".to_string()).to_abstract();
        targets.push(width_id.clone());

        let mut interval = 0;
        loop {

            let prev_prs_map: HashSet<StructureIdent> = prs_map.keys().cloned().collect();

            for target in targets {

                println!("");
                println!("Lookup up {}", target);
                match target {
                    AbstractIdent::Field(ref fi) => {
                        println!("Target is a Field");

                        let child = prs_map.get(&fi.structure).unwrap();

                        // let field_id = struct_id.get_field_ident(fi.id);
                        let dr = child.borrow().try_get_field(fi.clone(), &vd, &prs_map).unwrap();
                        match dr.result {
                            DepResult::Success(f) => {
                                let v = f.parse(fm).unwrap();
                                println!("Field parsed! Value is {:?}", v);
                                //dg.lookup_values.insert(target.clone(), v);
                                vd.insert_field(fi.clone(), v);
                                for pc in dr.parents_children.into_iter() {
                                    let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                    for c in pc.1 {
                                        println!("Adding dependencies for {}: {:?}", c, deps);
                                        dg.add_dependancies(c, deps.clone());
                                    }
                                }

                            },
                            DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                                println!("Lookup incomplete");
                                let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                                println!("Adding dependencies for {}: {:?}", target, deps);
                                dg.add_dependancies(target.clone(), deps.into_iter().collect());
                            },
                            DepResult::DoesNotExist => {
                                todo!("Field does not exist?")
                            }
                        }

                        continue;

                    },
                    AbstractIdent::FieldAttr(ref fai) => {
                        println!("Target is a Field");

                        let child = prs_map.get(&fai.field.structure).unwrap().clone();

                        let dr = child.borrow_mut().try_lookup(target.clone(), &vd, &mut prs_map).unwrap();
                        match dr.result {
                            DepResult::Success(v) => {
                                //dg.lookup_values.insert(target.clone(), v);
                                vd.insert_field_attr(fai.clone(), v);
                                println!("Adding value for {}", target);
                                for pc in dr.parents_children.into_iter() {
                                    let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                    for c in pc.1 {
                                        println!("Adding dependencies for {}: {:?}", c, deps);
                                        dg.add_dependancies(c, deps.clone());
                                    }
                                }
                            },
                            DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                                println!("Lookup incomplete");
                                let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                                println!("Adding dependencies for {}: {:?}", target, deps);
                                dg.add_dependancies(target, deps.into_iter().collect());
                            },
                            DepResult::DoesNotExist => {
                                todo!("Field Attribute does not exist?");
                                
                                
                            }
                        }

                        continue;
                    }
                    AbstractIdent::StructureAttr(ref sai) => {

                        println!("Targt is a Structure Attribute");
                        //if let (Some(structure_match), Some(attr_match)) = (caps.name("structure"), caps.name("attribute")) {

                        if sai.structure == file_struct {
                            match sai.attr {
                                StructureAttrType::End | StructureAttrType::SpareLength | StructureAttrType::Length => {
                                    vd.insert_struct_attr(sai.clone(), ExprValue::Position(BitPosition::bytes(fm.len() as u64)));
                                },
                                StructureAttrType::Start => {
                                    vd.insert_struct_attr(sai.clone(), ExprValue::Position(BitPosition::zero()));
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
                                        println!("Adding value for {}", target);
                                        for pc in dr.parents_children.into_iter() {
                                            let deps: Vec<AbstractIdent> = pc.0.clone().into_iter().collect();
                                            for c in pc.1 {
                                                println!("Adding dependencies for {}: {:?}", c, deps);
                                                dg.add_dependancies(c, deps.clone());
                                            }
                                        }
                                        break;
                                    },
                                    DepResult::Incomplete(deps) | DepResult::MightExist(deps) => {
                                        println!("Lookup incomplete");
                                        let deps: Vec<AbstractIdent> = deps.into_iter().collect();
                                        println!("Adding dependencies for {}: {:?}", target, deps);
                                        dg.add_dependancies(target, deps.into_iter().collect());
                                        break;
                                    },
                                    DepResult::DoesNotExist => {
                                        println!("Field does not exist. Searching in parent");
                                        let new_child_id = child.borrow().parent_id.clone();
                                        if new_child_id == file_struct {
                                            if *sai == s.borrow().id.get_attr_ident(StructureAttrType::Position) {
                                                //dg.lookup_values.insert(target, ExprValue::Position(BitPosition::zero()));
                                                vd.insert_struct_attr(sai.clone(), ExprValue::Position(BitPosition::zero()));
                                            } else if matches!(s.borrow().length, LengthPolicy::Expand) {
                                                if target == s.borrow().id.get_attr_ident(StructureAttrType::Length).to_abstract() {
                                                    let start_id = s.borrow().id.get_attr_ident(StructureAttrType::Start);
                                                    if let Some(start) = vd.lookup_struct_attr(&start_id) {
                                                        let start = start.clone().expect_position().unwrap();
                                                        let end = BitPosition::bytes(fm.len() as u64);
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
                        // } else {
                        //     panic!("Invalid target: {}", target);
                        // }
                    },
                    AbstractIdent::Field(ref f) => {
                        panic!("Invalid target: {}", target);
                    }
                }


            }

            // Discover new unkowns
            for k in prs_map.keys() {
                if !prev_prs_map.contains(k) {
                    let (mut unk0, unk1) = prs_map[k].borrow().unknowns(&vd).unwrap();
                    unk0.extend(unk1);
                    for ident in unk0 {
                        if !dg.dep_map.contains_key(&ident) {
                            dg.dep_map.insert(ident.clone(), Rc::new(RefCell::new(DepNode::new(ident.clone()))));
                            dg.dep_roots.insert(ident);
                        }
                    }
                }
            }

            targets = dg.get_resolvable_unknowns(&vd);//Vec::new();

            println!("{:?}", targets);
            if targets.is_empty() {
                break;
            }
            interval += 1;
            println!("============= INTERVAL #{} ==============", interval);
            println!("Knowns: {}, Total: {}, Targets: {}", vd.len(), dg.dep_map.len(), targets.len());
            if interval > 7000 {
                println!("Width: {:?}", vd.lookup_any(&width_id).unwrap());

                panic!()
            }
        }

        // println!("Fields Attributes: {:?}", vd.field_attr_values);
        println!("Width: {:?}", vd.lookup_any(&width_id).unwrap());
        
        // println!("{:?}", dg.dep_map);
        // println!("results: {:?}", dg.lookup_values);
        // println!("Dependencies:");
        // for value in dg.dep_map.values() {
        //     println!("\t{:?}", value);
        // }
        // println!("Values:");
        // for (key, value) in dg.lookup_values.iter() {
        //     println!("{}: {:?}", key, value);
        // }
        todo!()

    }
}

#[cfg(test)]
mod file_tests {
    use super::*;

    fn make_png() -> FileMap {
        let PNG_signature = FieldSpec {
            id: "signature".to_string(),
            name: "PNG Signature".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(8))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "B".to_string(),
            endian: Endianness::Big
        };

        let mut signature_id_map = HashMap::new();
        signature_id_map.insert("signature".to_string(), 0);

        let sig_section = SectionSpec {
            length: LengthPolicy::FitContents,
            section_type: SectionType::Header,
            fields: vec![
                PNG_signature
            ],
            id_map: signature_id_map, // Map of ID to index in fields array
        };

        let sig_struct = Structure {
            id: "png_signature".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::from_str("1B").unwrap())),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::from_str("0B").unwrap())),
            length: LengthPolicy::FitContents,
            exports: HashSet::new(),
            stype: StructureType::Section(sig_section)
        };

        let IHDR_width = FieldSpec {
            id: "IHDR_width".to_string(),
            name: "IHDR Width".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(4))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "U".to_string(),
            endian: Endianness::Big
        };

        let IHDR_height = FieldSpec {
            id: "IHDR_height".to_string(),
            name: "IHDR Height".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(4))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(4))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "U".to_string(),
            endian: Endianness::Big
        };

        let IHDR_bit_depth = FieldSpec {
            id: "IHDR_bit_depth".to_string(),
            name: "IHDR Bit Depth".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(8))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "U".to_string(),
            endian: Endianness::Big
        };

        let IHDR_color_type = FieldSpec {
            id: "IHDR_color_type".to_string(),
            name: "IHDR Color Type".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(9))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "U".to_string(),
            endian: Endianness::Big
        };

        let IHDR_compression_method = FieldSpec {
            id: "IHDR_compression_method".to_string(),
            name: "IHDR Compression Method".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(10))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "U".to_string(),
            endian: Endianness::Big
        };

        let IHDR_filter_method = FieldSpec {
            id: "IHDR_filter_method".to_string(),
            name: "IHDR Filter Method".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(11))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "U".to_string(),
            endian: Endianness::Big
        };

        let IHDR_interlace_method = FieldSpec {
            id: "IHDR_interlace_method".to_string(),
            name: "IHDR Interlace Method".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(12))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "U".to_string(),
            endian: Endianness::Big
        };

        let mut IHDR_id_map = HashMap::new();
        IHDR_id_map.insert("IHDR_width".to_string(), 0);
        IHDR_id_map.insert("IHDR_height".to_string(), 1);
        IHDR_id_map.insert("IHDR_bit_depth".to_string(), 2);
        IHDR_id_map.insert("IHDR_color_type".to_string(), 3);
        IHDR_id_map.insert("IHDR_compression_method".to_string(), 4);
        IHDR_id_map.insert("IHDR_filter_method".to_string(), 5);
        IHDR_id_map.insert("IHDR_interlace_method".to_string(), 6);

        let IHDR_section = SectionSpec {
            length: LengthPolicy::Expand,
            section_type: SectionType::Body,
            fields: vec![
                IHDR_width,
                IHDR_height,
                IHDR_bit_depth,
                IHDR_color_type,
                IHDR_compression_method,
                IHDR_filter_method,
                IHDR_interlace_method
            ],
            id_map: IHDR_id_map // Map of ID to index in fields array
        };

        let IHDR_struct = Structure {
            id: "ihdr_body".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::zero())),
            length: LengthPolicy::Expand,
            exports: HashSet::from(["IHDR_width".to_string(), "IHDR_height".to_string()]),
            stype: StructureType::Section(IHDR_section)
        };

        // CHUNK HEADER

        let chunk_length = FieldSpec {
            id: "chunk_length".to_string(),
            name: "Chunk Length".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(4))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "U".to_string(),
            endian: Endianness::Big            
        };

        let chunk_type = FieldSpec {
            id: "chunk_type".to_string(),
            name: "Chunk Type".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(4))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "S".to_string(),
            endian: Endianness::Big            
        };

        let mut chunk_header_id_map = HashMap::new();
        chunk_header_id_map.insert("chunk_length".to_string(), 0);
        chunk_header_id_map.insert("chunk_type".to_string(), 1);

        let chunk_header_section = SectionSpec {
            length: LengthPolicy::FitContents,
            section_type: SectionType::Header,
            fields: vec![
                chunk_length,
                chunk_type
            ],
            id_map: chunk_header_id_map // Map of ID to index in fields array
        };

        let chunk_header_struct = Structure {
            id: "chunk_header".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::zero())),
            length: LengthPolicy::FitContents,
            exports: HashSet::from(["chunk_length".to_string(), "chunk_type".to_string()]),
            stype: StructureType::Section(chunk_header_section)
        };

        // CHUNK FOOTER

        let chunk_checksum = FieldSpec {
            id: "chunk_crc32".to_string(),
            name: "Chunk CRC32".to_string(),
            offset: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            length: Expr::Value(ExprValue::Position(BitPosition::bytes(4))),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::bytes(0))),
            dtype: "B".to_string(),
            endian: Endianness::Big            
        };

        let mut chunk_footer_id_map = HashMap::new();
        chunk_footer_id_map.insert("chunk_crc32".to_string(), 0);

        let chunk_footer_section = SectionSpec {
            length: LengthPolicy::FitContents,
            section_type: SectionType::Header,
            fields: vec![
                chunk_checksum
            ],
            id_map: chunk_footer_id_map // Map of ID to index in fields array
        };

        let chunk_footer_struct = Structure {
            id: "chunk_footer".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::zero())),
            length: LengthPolicy::FitContents,
            exports: HashSet::new(),
            stype: StructureType::Section(chunk_footer_section)
        };

        // CHUNK BODY

        let unidentified_section = SectionSpec {
            length: LengthPolicy::Expand,
            section_type: SectionType::Body,
            fields: Vec::new(),
            id_map: HashMap::new()
        };

        let default_struct = Structure {
            id: "default_chunk_body".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::zero())),
            length: LengthPolicy::Expand,
            exports: HashSet::new(),
            stype: StructureType::Section(unidentified_section)
        };

        let chunk_body_struct = Structure {
            id: "chunk_body".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::zero())),
            length: LengthPolicy::Expr(Expr::Var("chunk_length".to_string())),
            exports: HashSet::from(["IHDR_width".to_string(), "IHDR_height".to_string()]),
            stype: StructureType::Switch {
                value: Expr::Var("chunk_type".to_string()),
                cases: vec![
                    (Expr::Op{op: ExprOp::Eq, args: vec![Expr::Arg(0), Expr::Value(ExprValue::String("IHDR".to_string()))]},
                        IHDR_struct)
                ],
                default: Box::new(default_struct)
            }
        };

        // CHUNK

        let chunk_struct = Structure {
            id: "png_chunk".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::zero())),
            length: LengthPolicy::FitContents,
            exports: HashSet::from(["IHDR_width".to_string(), "IHDR_height".to_string()]),
            stype: StructureType::Sequence (vec![
                chunk_header_struct,
                chunk_body_struct,
                chunk_footer_struct
            ])
        };

        let chunks_struct = Structure {
            id: "png_chunks".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::zero())),
            length: LengthPolicy::Expand,
            exports: HashSet::from(["IHDR_width".to_string(), "IHDR_height".to_string()]),
            stype: StructureType::RepeatUntil {
                end: Expr::Var("file.end".to_string()),
                structure: Box::new(chunk_struct)
            }
        };

        FileMap::new(Structure {
            id: "PNG".to_string(),
            alignment: Expr::Value(ExprValue::Position(BitPosition::bytes(1))),
            alignment_base: Expr::Value(ExprValue::Position(BitPosition::zero())),
            length: LengthPolicy::Expand,
            exports: HashSet::new(),
            stype: StructureType::Sequence (
                vec![sig_struct, chunks_struct]
            )
        })
    }

    #[test]
    fn png_test() {
        let mut png_sc = make_png();
        let filename = "test_file.png".to_string();
        let mut fm = crate::FileManager::new(filename, crate::FileManagerType::ReadOnly, false).unwrap();
        png_sc.resolve_structure(&mut fm);
    }
}