#[derive(Copy, Clone)]
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

pub fn fmt_length(fmt: &BinaryFormat) -> usize {
    match fmt {
        BinaryFormat::UInt(UIntFormat::U8)  => 1,
        BinaryFormat::UInt(UIntFormat::U16) => 2,
        BinaryFormat::UInt(UIntFormat::U32) => 4,
        BinaryFormat::UInt(UIntFormat::U64) => 8,
        BinaryFormat::IInt(IIntFormat::I8)  => 1,
        BinaryFormat::IInt(IIntFormat::I16) => 2,
        BinaryFormat::IInt(IIntFormat::I32) => 4,
        BinaryFormat::IInt(IIntFormat::I64) => 8,
        BinaryFormat::Float(FloatFormat::F32) => 4,
        BinaryFormat::Float(FloatFormat::F64) => 8
    }
}

#[macro_export]
macro_rules! str_res {
    ( $x:expr ) => {
        {
            match $x {
                Ok(a) => Ok(a),
                Err(a) => Err(a.to_string())
            }
        }
    };
}

pub fn to_bytes(input: &Vec<char>, datatype: DataType) -> Result<Vec<u8>, String> {
    let s: String = input.iter().collect::<String>();
    println!("Number: {:?}", str_res!(s.parse::<f32>()));
    match &datatype.end {
        Endianness::Big => {
            match &datatype.fmt {
                BinaryFormat::UInt(UIntFormat::U8)  => Ok((str_res!(s.parse::<u8>())?).to_be_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U16) => Ok((str_res!(s.parse::<u16>())?).to_be_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U32) => Ok((str_res!(s.parse::<u32>())?).to_be_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U64) => Ok((str_res!(s.parse::<u64>())?).to_be_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I8)  => Ok((str_res!(s.parse::<i8>())?).to_be_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I16) => Ok((str_res!(s.parse::<i16>())?).to_be_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I32) => Ok((str_res!(s.parse::<i32>())?).to_be_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I64) => Ok((str_res!(s.parse::<i64>())?).to_be_bytes().to_vec()),
                BinaryFormat::Float(FloatFormat::F32) => Ok((str_res!(s.parse::<f32>())?).to_be_bytes().to_vec()),
                BinaryFormat::Float(FloatFormat::F64) => Ok((str_res!(s.parse::<f64>())?).to_be_bytes().to_vec())
            }
        },
        Endianness::Little => {
            match &datatype.fmt {
                BinaryFormat::UInt(UIntFormat::U8)  => Ok((str_res!(s.parse::<u8>())?).to_le_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U16) => Ok((str_res!(s.parse::<u16>())?).to_le_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U32) => Ok((str_res!(s.parse::<u32>())?).to_le_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U64) => Ok((str_res!(s.parse::<u64>())?).to_le_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I8)  => Ok((str_res!(s.parse::<i8>())?).to_le_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I16) => Ok((str_res!(s.parse::<i16>())?).to_le_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I32) => Ok((str_res!(s.parse::<i32>())?).to_le_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I64) => Ok((str_res!(s.parse::<i64>())?).to_le_bytes().to_vec()),
                BinaryFormat::Float(FloatFormat::F32) => Ok((str_res!(s.parse::<f32>())?).to_le_bytes().to_vec()),
                BinaryFormat::Float(FloatFormat::F64) => Ok((str_res!(s.parse::<f64>())?).to_le_bytes().to_vec())
            }
        },
        Endianness::Network => {
            match &datatype.fmt {
                BinaryFormat::UInt(UIntFormat::U8)  => Ok((str_res!(s.parse::<u8>())?).to_ne_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U16) => Ok((str_res!(s.parse::<u16>())?).to_ne_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U32) => Ok((str_res!(s.parse::<u32>())?).to_ne_bytes().to_vec()),
                BinaryFormat::UInt(UIntFormat::U64) => Ok((str_res!(s.parse::<u64>())?).to_ne_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I8)  => Ok((str_res!(s.parse::<i8>())?).to_ne_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I16) => Ok((str_res!(s.parse::<i16>())?).to_ne_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I32) => Ok((str_res!(s.parse::<i32>())?).to_ne_bytes().to_vec()),
                BinaryFormat::IInt(IIntFormat::I64) => Ok((str_res!(s.parse::<i64>())?).to_ne_bytes().to_vec()),
                BinaryFormat::Float(FloatFormat::F32) => Ok((str_res!(s.parse::<f32>())?).to_ne_bytes().to_vec()),
                BinaryFormat::Float(FloatFormat::F64) => Ok((str_res!(s.parse::<f64>())?).to_ne_bytes().to_vec())
            }
        },
    }
}

fn vec_to_1(input: &Vec<u8>) -> Result<[u8; 1], String> {
    if input.len() >= 1 {
        Ok([input[0]])
    } else {
        Err("Not enough bytes for data type".to_string())
    }
}

fn vec_to_2(input: &Vec<u8>) -> Result<[u8; 2], String> {
    if input.len() >= 2 {
        Ok([input[0], input[1]])
    } else {
        Err("Not enough bytes for data type".to_string())
    }
}

fn vec_to_4(input: &Vec<u8>) -> Result<[u8; 4], String> {
    if input.len() >= 2 {
        Ok([input[0], input[1], input[2], input[3]])
    } else {
        Err("Not enough bytes for data type".to_string())
    }
}

fn vec_to_8(input: &Vec<u8>) -> Result<[u8; 8], String> {
    if input.len() >= 2 {
        Ok([input[0], input[1], input[2], input[3], input[4], input[5], input[6], input[7]])
    } else {
        Err("Not enough bytes for data type".to_string())
    }
}

pub fn from_bytes(input: &Vec<u8>, datatype: DataType) -> Result<String, String> {
    match &datatype.end {
        Endianness::Big => {
            match &datatype.fmt {
                BinaryFormat::UInt(UIntFormat::U8)  => Ok(format!("{}", u8::from_be_bytes(vec_to_1(input)?))),
                BinaryFormat::UInt(UIntFormat::U16) => Ok(format!("{}", u16::from_be_bytes(vec_to_2(input)?))),
                BinaryFormat::UInt(UIntFormat::U32) => Ok(format!("{}", u32::from_be_bytes(vec_to_4(input)?))),
                BinaryFormat::UInt(UIntFormat::U64) => Ok(format!("{}", u64::from_be_bytes(vec_to_8(input)?))),
                BinaryFormat::IInt(IIntFormat::I8)  => Ok(format!("{}", i8::from_be_bytes(vec_to_1(input)?))),
                BinaryFormat::IInt(IIntFormat::I16) => Ok(format!("{}", i16::from_be_bytes(vec_to_2(input)?))),
                BinaryFormat::IInt(IIntFormat::I32) => Ok(format!("{}", i32::from_be_bytes(vec_to_4(input)?))),
                BinaryFormat::IInt(IIntFormat::I64) => Ok(format!("{}", i64::from_be_bytes(vec_to_8(input)?))),
                BinaryFormat::Float(FloatFormat::F32) => Ok(format!("{}", f32::from_be_bytes(vec_to_4(input)?))),
                BinaryFormat::Float(FloatFormat::F64) => Ok(format!("{}", f64::from_be_bytes(vec_to_8(input)?)))
            }
        },
        Endianness::Little => {
            match &datatype.fmt {
                BinaryFormat::UInt(UIntFormat::U8)  => Ok(format!("{}", u8::from_le_bytes(vec_to_1(input)?))),
                BinaryFormat::UInt(UIntFormat::U16) => Ok(format!("{}", u16::from_le_bytes(vec_to_2(input)?))),
                BinaryFormat::UInt(UIntFormat::U32) => Ok(format!("{}", u32::from_le_bytes(vec_to_4(input)?))),
                BinaryFormat::UInt(UIntFormat::U64) => Ok(format!("{}", u64::from_le_bytes(vec_to_8(input)?))),
                BinaryFormat::IInt(IIntFormat::I8)  => Ok(format!("{}", i8::from_le_bytes(vec_to_1(input)?))),
                BinaryFormat::IInt(IIntFormat::I16) => Ok(format!("{}", i16::from_le_bytes(vec_to_2(input)?))),
                BinaryFormat::IInt(IIntFormat::I32) => Ok(format!("{}", i32::from_le_bytes(vec_to_4(input)?))),
                BinaryFormat::IInt(IIntFormat::I64) => Ok(format!("{}", i64::from_le_bytes(vec_to_8(input)?))),
                BinaryFormat::Float(FloatFormat::F32) => Ok(format!("{}", f32::from_le_bytes(vec_to_4(input)?))),
                BinaryFormat::Float(FloatFormat::F64) => Ok(format!("{}", f64::from_le_bytes(vec_to_8(input)?)))
            }
        },
        Endianness::Network => {
            match &datatype.fmt {
                BinaryFormat::UInt(UIntFormat::U8)  => Ok(format!("{}", u8::from_ne_bytes(vec_to_1(input)?))),
                BinaryFormat::UInt(UIntFormat::U16) => Ok(format!("{}", u16::from_ne_bytes(vec_to_2(input)?))),
                BinaryFormat::UInt(UIntFormat::U32) => Ok(format!("{}", u32::from_ne_bytes(vec_to_4(input)?))),
                BinaryFormat::UInt(UIntFormat::U64) => Ok(format!("{}", u64::from_ne_bytes(vec_to_8(input)?))),
                BinaryFormat::IInt(IIntFormat::I8)  => Ok(format!("{}", i8::from_ne_bytes(vec_to_1(input)?))),
                BinaryFormat::IInt(IIntFormat::I16) => Ok(format!("{}", i16::from_ne_bytes(vec_to_2(input)?))),
                BinaryFormat::IInt(IIntFormat::I32) => Ok(format!("{}", i32::from_ne_bytes(vec_to_4(input)?))),
                BinaryFormat::IInt(IIntFormat::I64) => Ok(format!("{}", i64::from_ne_bytes(vec_to_8(input)?))),
                BinaryFormat::Float(FloatFormat::F32) => Ok(format!("{}", f32::from_ne_bytes(vec_to_4(input)?))),
                BinaryFormat::Float(FloatFormat::F64) => Ok(format!("{}", f64::from_ne_bytes(vec_to_8(input)?)))
            }
        },
    }
}