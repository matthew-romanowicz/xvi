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

// pub fn to_bytes(input: &Vec<char>, datatype: DataType) -> Result<Vec<u8>, String> {
//     match input.iter().collect::<String>().parse::<i16>() {
//         Ok(n) => println!("Number: {}", n),
//         Err(msg) => println!("Error: {}", msg.to_string())
//     };
//     match datatype.fmt {
//         BinaryFormat::UInt(uint_format) => {
//             match parse_uint(input, &uint_format) {
//                 Ok(n) => {
//                     Ok(uint_to_bytes(n, &datatype.end, &uint_format))
//                 },
//                 Err(msg) => Err(msg)
//             }
//         },
//         BinaryFormat::IInt(iint_format) => {
//             match parse_iint(input, &iint_format) {
//                 Ok(n) => {
//                     Ok(iint_to_bytes(n, &datatype.end, &iint_format))
//                 },
//                 Err(msg) => Err(msg)
//             }
//         },
//         _ => todo!()
//     }
// }