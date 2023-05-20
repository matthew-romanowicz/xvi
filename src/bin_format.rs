pub enum Endianness {
    Big,
    Little,
    Network
}

pub enum UIntFormat {
    U8,
    U16,
    U32,
    U64
}

pub enum IIntFormat {
    I8,
    I16,
    I32,
    I64
}

pub enum FloatFormat {
    F32,
    F64
}

pub enum BinaryFormat {
    UInt(UIntFormat),
    IInt(IIntFormat),
    Float(FloatFormat)
}

pub struct DataType {
    pub fmt: BinaryFormat,
    pub end: Endianness
}

fn parse_uint(input: &Vec<char>, uint_format: &UIntFormat) -> Result<u64, String> {

    let max_value = match uint_format {
        UIntFormat::U8 => std::u8::MAX as u64,
        UIntFormat::U16 => std::u16::MAX as u64,
        UIntFormat::U32 => std::u32::MAX as u64,
        UIntFormat::U64 => std::u64::MAX
    };

    let mut res: u64 = 0;
    for c in input {
        match c {
            '0'..='9' => {
                if res > (max_value - ((*c as u64) - 48)) / 10 {
                    return Err("Value out of bounds for data type".to_string())
                }
                res *= 10;
                res += (*c as u64) - 48;
            },
            _ => {
                return Err(format!("Invalid character in unsigned uint: {}", c))
            }
        }
    }
    return Ok(res)
}

fn parse_iint(input: &Vec<char>, iint_format: &IIntFormat) -> Result<i64, String> {

    let mut input_iter = input.iter().peekable();

    let neg = match input_iter.peek() {
        Some('-') => {
            input_iter.next();
            true
        },
        Some('+') => {
            input_iter.next();
            false
        },
        _ => {
            false
        }
    };

    let max_value = match neg {
        true => {
            match iint_format {
                IIntFormat::I8 => std::i8::MIN as i64,
                IIntFormat::I16 => std::i16::MIN as i64,
                IIntFormat::I32 => std::i32::MIN as i64,
                IIntFormat::I64 => std::i64::MIN
            }
        },
        false => {
            match iint_format {
                IIntFormat::I8 => -(std::i8::MAX as i64),
                IIntFormat::I16 => -(std::i16::MAX as i64),
                IIntFormat::I32 => -(std::i32::MAX as i64),
                IIntFormat::I64 => -std::i64::MAX
            }
        }
    };

    let mut res: i64 = 0;
    for c in input_iter {
        match c {
            '0'..='9' => {
                if res < (max_value + ((*c as i64) - 48)) / 10 {
                    return Err("Value out of bounds for data type".to_string())
                }
                res *= 10;
                res -= (*c as i64) - 48; // Need to use negative since the negative bound is greater
            },
            _ => {
                return Err(format!("Invalid character in unsigned uint: {}", c))
            }
        }
    }

    Ok(match neg {true => res, false => -res})
}

pub fn to_bytes(input: &Vec<char>, datatype: DataType) -> Result<Vec<u8>, String> {
    match datatype.fmt {
        BinaryFormat::UInt(uint_format) => {
            match parse_uint(input, &uint_format) {
                Ok(n) => {
                    match datatype.end {
                        Endianness::Big => Ok(n.to_be_bytes().to_vec()),
                        Endianness::Little => Ok(n.to_le_bytes().to_vec()),
                        Endianness::Network => Ok(n.to_ne_bytes().to_vec())
                    }
                },
                Err(msg) => Err(msg)
            }
        },
        BinaryFormat::IInt(iint_format) => {
            match parse_iint(input, &iint_format) {
                Ok(n) => {
                    match datatype.end {
                        Endianness::Big => Ok(n.to_be_bytes().to_vec()),
                        Endianness::Little => Ok(n.to_le_bytes().to_vec()),
                        Endianness::Network => Ok(n.to_ne_bytes().to_vec())
                    }
                },
                Err(msg) => Err(msg)
            }
        },
        _ => todo!()
    }
}