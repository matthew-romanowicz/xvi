pub enum CommandKeyword {
    Set,
    Caps,
    Hex,
    Ascii,
    Filename,
    LNum,
    CNum,
    CLevel,
    Line,
    Chunk,
    Undo,
    Syntax,
    Fill,
    Swap,
    Deflate,
    Inflate,
    Not,
    RShift,
    LShift,
    Slice,
    Icase,
    Regex,
    Endian,
    Clear,
    Ins,
    Ovr,
    Print,
    PrintSeek,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F16,
    F32,
    F64,
    On,
    Off,
    BigEndian,
    LittleEndian,
    NetworkEndian,
    Dec,
    Save,
    Quit,
    ForceQuit,
    SaveAndQuit,
    Open,
    Cat,
    And,
    Or,
    Nand,
    Nor,
    Xor,
    Xnor,
    Manual,
    Refresh
}

fn parse_command_keyword(word: &Vec<char>) -> Option<CommandKeyword> {
    let s: String = word.iter().collect();
    match s.as_str() {
        "set" => Some(CommandKeyword::Set),
        "caps" => Some(CommandKeyword::Caps),
        "hex" => Some(CommandKeyword::Hex),
        "ascii" => Some(CommandKeyword::Ascii),
        "fname" => Some(CommandKeyword::Filename),
        "lnum" => Some(CommandKeyword::LNum),
        "cnum" => Some(CommandKeyword::CNum),
        "clevel" => Some(CommandKeyword::CLevel),
        "line" => Some(CommandKeyword::Line),
        "chunk" => Some(CommandKeyword::Chunk),
        "undo" => Some(CommandKeyword::Undo),
        "syntax" => Some(CommandKeyword::Syntax),
        "fill" => Some(CommandKeyword::Fill),
        "swap" => Some(CommandKeyword::Swap),
        "deflate" => Some(CommandKeyword::Deflate),
        "inflate" => Some(CommandKeyword::Inflate),
        "not" => Some(CommandKeyword::Not),
        "rshft" => Some(CommandKeyword::RShift),
        "lshft" => Some(CommandKeyword::LShift),
        "slice" => Some(CommandKeyword::Slice),
        "icase" => Some(CommandKeyword::Icase),
        "regex" => Some(CommandKeyword::Regex),
        "endian" => Some(CommandKeyword::Endian),
        "clear" => Some(CommandKeyword::Clear),
        "ins" => Some(CommandKeyword::Ins),
        "ovr" => Some(CommandKeyword::Ovr),
        "p" => Some(CommandKeyword::Print),
        "ps" => Some(CommandKeyword::PrintSeek),
        "u8" => Some(CommandKeyword::U8),
        "u16" => Some(CommandKeyword::U16),
        "u32" => Some(CommandKeyword::U32),
        "u64" => Some(CommandKeyword::U64),
        "i8" => Some(CommandKeyword::I8),
        "i16" => Some(CommandKeyword::I16),
        "i32" => Some(CommandKeyword::I32),
        "i64" => Some(CommandKeyword::I64),
        "f16" => Some(CommandKeyword::F16),
        "f32" => Some(CommandKeyword::F32),
        "f64" => Some(CommandKeyword::F64),
        "on" => Some(CommandKeyword::On),
        "off" => Some(CommandKeyword::Off),
        "dec" => Some(CommandKeyword::Dec),
        "be" => Some(CommandKeyword::BigEndian),
        "le" => Some(CommandKeyword::LittleEndian),
        "ne" => Some(CommandKeyword::NetworkEndian),
        "w" => Some(CommandKeyword::Save),
        "q" => Some(CommandKeyword::Quit),
        "q!" => Some(CommandKeyword::ForceQuit),
        "x" | "wq" => Some(CommandKeyword::SaveAndQuit),
        "open" => Some(CommandKeyword::Open),
        "cat" => Some(CommandKeyword::Cat),
        "and" => Some(CommandKeyword::And),
        "or" => Some(CommandKeyword::Or),
        "nand" => Some(CommandKeyword::Nand),
        "nor" => Some(CommandKeyword::Nor),
        "xor" => Some(CommandKeyword::Xor),
        "xnor" => Some(CommandKeyword::Xnor),
        "man" => Some(CommandKeyword::Manual),
        "refresh" => Some(CommandKeyword::Refresh),
        _ => None
    }
}

pub fn parse_bytes(word: &Vec<char>) -> Result<Vec<u8>, String> { 
    let mut word_iter = word.iter();
    if *word_iter.next().unwrap() == 'x'{
        let mut buffer = Vec::<u8>::new();
        let mut left = true;
        let mut n: u8 = 0;
        for c in word_iter {
            match c {
                '0'..='9' => {
                    if left {
                        n = ((*c as u8) -  48) << 4;
                    } else {
                        buffer.push(n + ((*c as u8) -  48));
                        n = 0;
                    }
                },
                'a'..='f' => {
                    if left {
                        n = ((*c as u8) -  87) << 4;
                    } else {
                        buffer.push(n + ((*c as u8) -  87));
                        n = 0;
                    }
                },
                'A'..='F' => {
                    if left {
                        n = ((*c as u8) -  55) << 4;
                    } else {
                        buffer.push(n + ((*c as u8) -  55));
                        n = 0;
                    }
                },
                _ => return Err("Invalid character in hex string".to_string())
            }
            left = !left;
        }

        Ok(buffer)
    } else {
        Err("Bytes literals must start with character 'x'".to_string())
    }
}

//#[derive(Copy, Clone)]
pub enum CommandToken {
    Word(Vec<char>),
    Integer(Vec<char>, usize),
    Register(usize),
    Keyword(CommandKeyword)
}

pub fn parse_command_token(v: &Vec<char>) -> Option<CommandToken> {
    if v.len() == 0 {
        return None;
    } else if let Ok(n) = v.iter().collect::<String>().parse::<usize>() {
        return Some(CommandToken::Integer(v.to_vec(), n));
    } else if v[0] == 'r' {
        if let Ok(n) = v[1..v.len()].iter().collect::<String>().parse::<usize>() {
            return Some(CommandToken::Register(n));
        }
    }

    if let Some(kwrd) = parse_command_keyword(v) {
        Some(CommandToken::Keyword(kwrd))
    } else {
        Some(CommandToken::Word(v.to_vec()))
    }
}

pub fn parse_command(command: &Vec<char>) -> Vec<CommandToken> {
    let mut result = Vec::<CommandToken>::new();
    let mut current_word = Vec::<char>::new();

    for c in command {
        match c {
            ' ' => {
                match parse_command_token(&current_word) {
                    Some(token) => result.push(token),
                    None => ()
                };
                current_word = Vec::<char>::new();
            },
            _ => current_word.push(*c)
        }
    }

    match parse_command_token(&current_word) {
        Some(token) => result.push(token),
        None => ()
    }

    result
}

#[derive(Copy, Clone)]
pub enum KeystrokeToken {
    Character(char),
    Integer(usize)
}

pub fn parse_keystroke(keystroke: &Vec<char>) -> Vec<KeystrokeToken> {
    let mut result = Vec::<KeystrokeToken>::new();
    let mut current_int: usize = 0;
    let mut building_int: bool = false;
    for c in keystroke {
        match c {
            '0'..='9' => {
                building_int = true;
                current_int *= 10;
                current_int += (*c as usize) - 48;
            },
            _ => {
                if building_int {
                    result.push(KeystrokeToken::Integer(current_int));
                    current_int = 0;
                    building_int = false;
                } 
                result.push(KeystrokeToken::Character(*c));
            }
        }
    }

    result
}