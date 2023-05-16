use crate::hex_edit::FillType;

pub enum CommandKeyword {
    Set,
    Caps,
    Hex,
    Ascii,
    Line,
    Fill,
    Icase,
    Ins,
    Ovr,
    Fmt,
    On,
    Off,
    Save,
    Quit,
    ForceQuit,
    SaveAndQuit
}

fn parse_command_keyword(word: Vec<char>) -> Option<CommandKeyword> {
    let s: String = word.iter().collect();
    match s.as_str() {
        "set" => Some(CommandKeyword::Set),
        "caps" => Some(CommandKeyword::Caps),
        "hex" => Some(CommandKeyword::Hex),
        "ascii" => Some(CommandKeyword::Ascii),
        "line" => Some(CommandKeyword::Line),
        "fill" => Some(CommandKeyword::Fill),
        "icase" => Some(CommandKeyword::Icase),
        "ins" => Some(CommandKeyword::Ins),
        "ovr" => Some(CommandKeyword::Ovr),
        "fmt" => Some(CommandKeyword::Fmt),
        "on" => Some(CommandKeyword::On),
        "off" => Some(CommandKeyword::Off),
        "w" => Some(CommandKeyword::Save),
        "q" => Some(CommandKeyword::Quit),
        "q!" => Some(CommandKeyword::ForceQuit),
        "x" | "wq" => Some(CommandKeyword::SaveAndQuit),
        _ => None
    }
}

pub fn parse_filltype(word: Vec<char>) -> Result<FillType, String> {
    let mut word_iter = word.iter();
    match word_iter.next().unwrap() {
        'r' => {
            let mut n: usize = 0;
            for c in word_iter {
                match c {
                    '0'..='9' => {
                        n *= 10;
                        n += *c as usize - 48;
                    },
                    _ => return Err("Invalid character in register number".to_string())
                }
            }
            if n < 256 {
                return Ok(FillType::Register(n as u8));
            } else {
                return Err("Register number greater than maximum".to_string())
            }
        },
        'x' => {
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
            return Ok(FillType::Bytes(buffer));
        },
        _ => return Err("Invalid first character in fill string (only 'r' and 'x' accepted)".to_string())
    }
}

//#[derive(Copy, Clone)]
pub enum CommandToken {
    Word(Vec<char>),
    Integer(Vec<char>, usize),
    Keyword(CommandKeyword)
}

pub fn parse_command(command: &Vec<char>) -> Vec<CommandToken> {
    let mut result = Vec::<CommandToken>::new();
    let mut current_int: usize = 0;
    let mut building_int: bool = false;
    let mut current_word = Vec::<char>::new();
    let mut building_word: bool = false;

    //let mut command_iter = command.iter();
    //result.push(CommandToken::Start(*command_iter.next().unwrap()));

    for c in command {
        match c {
            ' ' => {
                if building_int {
                    result.push(CommandToken::Integer(current_word, current_int));
                    current_int = 0;
                    building_int = false;
                } else if building_word {
                    match parse_command_keyword(current_word.to_vec()) {
                        Some(kw) => result.push(CommandToken::Keyword(kw)),
                        None => result.push(CommandToken::Word(current_word))
                    };
                    building_word = false;
                }

                current_word = Vec::<char>::new();
            },
            '0'..='9' if !building_word => {
                building_int = true;
                current_word.push(*c);
                current_int *= 10;
                current_int += (*c as usize) - 48;
            },
            _ if !building_int => {
                building_word = true;
                current_word.push(*c);
            },
            _ => {
                println!("An unexpected error occurred");
            }
        }
    }

    if building_int {
        result.push(CommandToken::Integer(current_word, current_int));
    } else if building_word {
        match parse_command_keyword(current_word.to_vec()) {
            Some(kw) => result.push(CommandToken::Keyword(kw)),
            None => result.push(CommandToken::Word(current_word))
        };
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