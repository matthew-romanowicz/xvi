

use std::io::SeekFrom;

use bitutils2::{BitIndex, BitField, BitIndexable};

use crate::{Endianness, FindDirection, BinaryFormat, UIntFormat, IIntFormat, FloatFormat};
use crate::common::{BinaryLogicOp, DecHexOff, DataSource, FullDataSource, RangeSize, FullRangeSize, Seek, FullSeek, FillType, FullFillType, MacroId, MarkId, FullMarkId, RegisterId, FullRegisterId};

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
    Refresh,
    Step
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
        "step" => Some(CommandKeyword::Step),
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
    Register(FullRegisterId),
    Keyword(CommandKeyword)
}

pub fn parse_command_token(v: &Vec<char>) -> Result<Option<CommandToken>, String> {
    if v.len() == 0 {
        return Ok(None);
    } else if let Ok(n) = v.iter().collect::<String>().parse::<usize>() {
        return Ok(Some(CommandToken::Integer(v.to_vec(), n)));
    } else if v[0] == 'r' {
        if let Ok(n) = v[1..v.len()].iter().collect::<String>().parse::<usize>() {
            match FullRegisterId::new(n) {
                Ok(reg_id) => return Ok(Some(CommandToken::Register(reg_id))),
                Err(n) => return Err(format!("Register indices must be less than {}", n).to_string())
            }
        }
    }

    if let Some(kwrd) = parse_command_keyword(v) {
        Ok(Some(CommandToken::Keyword(kwrd)))
    } else {
        Ok(Some(CommandToken::Word(v.to_vec())))
    }
}

fn command_kwrd_to_fmt(kwrd: &CommandKeyword) -> Result<BinaryFormat, String> {
    match kwrd {
        CommandKeyword::U8 => Ok(BinaryFormat::UInt(UIntFormat::U8)),
        CommandKeyword::U16 => Ok(BinaryFormat::UInt(UIntFormat::U16)),
        CommandKeyword::U32 => Ok(BinaryFormat::UInt(UIntFormat::U32)),
        CommandKeyword::U64 => Ok(BinaryFormat::UInt(UIntFormat::U64)),
        CommandKeyword::I8 => Ok(BinaryFormat::IInt(IIntFormat::I8)),
        CommandKeyword::I16 => Ok(BinaryFormat::IInt(IIntFormat::I16)),
        CommandKeyword::I32 => Ok(BinaryFormat::IInt(IIntFormat::I32)),
        CommandKeyword::I64 => Ok(BinaryFormat::IInt(IIntFormat::I64)),
        CommandKeyword::F16 => Ok(BinaryFormat::Float(FloatFormat::F16)),
        CommandKeyword::F32 => Ok(BinaryFormat::Float(FloatFormat::F32)),
        CommandKeyword::F64 => Ok(BinaryFormat::Float(FloatFormat::F64)),
        _ => Err("Command not recognized".to_string())
    }
}

pub enum UnaryRegisterOp {
    Swap,
    Not,
    Inflate,
    Deflate,
    Shift{bits: isize},
    Slice{start: BitIndex, stop: BitIndex},
    Clear
}

impl BinaryLogicOp {
    fn from_keyword(kw: &CommandKeyword) -> Option<BinaryLogicOp> {
        match kw {
            CommandKeyword::And => Some(BinaryLogicOp::And),
            CommandKeyword::Or => Some(BinaryLogicOp::Or),
            CommandKeyword::Nand => Some(BinaryLogicOp::Nand),
            CommandKeyword::Nor => Some(BinaryLogicOp::Nor),
            CommandKeyword::Xor => Some(BinaryLogicOp::Xor),
            CommandKeyword::Xnor => Some(BinaryLogicOp::Xnor),
            _ => None
        }
    }
}

pub enum BinaryRegisterOp {
    Cat,
    LogicOp(BinaryLogicOp)
}

pub enum OnOffSetting {
    Caps,
    Hex,
    Ascii,
    Filename,
    Icase,
    Regex
}




impl DecHexOff {
    fn from_keyword(kw: &CommandKeyword) -> Result<DecHexOff, String> {
        match kw {
            CommandKeyword::Dec => Ok(DecHexOff::Dec),
            CommandKeyword::Hex => Ok(DecHexOff::Hex),
            CommandKeyword::Off => Ok(DecHexOff::Off),
            _ => Err("Invalid option. Expected 'dec', 'hex', or 'off'".to_string())
        }
    }
}

impl Endianness {
    fn from_keyword(kw: &CommandKeyword) -> Result<Endianness, String> {
        match kw {
            CommandKeyword::LittleEndian => Ok(Endianness::Little),
            CommandKeyword::BigEndian => Ok(Endianness::Big),
            CommandKeyword::NetworkEndian => Ok(Endianness::Network),
            _ => Err("Invalid option. Expected 'le', 'be', or 'ne'".to_string())
        }
    }
}


pub enum DecHexOffSetting {
    CNum,
    LNum
}

pub enum UIntSetting {
    Line,
    Chunk,
    CLevel,
    Undo,

}

pub enum Command {
    SaveAndOrQuit{save: bool, quit: bool, forced: bool},
    Manual,
    Refresh,
    Step,
    ClearUndo,
    Open{path: String},
    UnaryRegisterOp{op: UnaryRegisterOp, reg: FullRegisterId},
    BinaryRegisterOp{op: BinaryRegisterOp, reg: FullRegisterId, other: FullFillType},
    Insert{value: String, fmt: BinaryFormat, overwrite: bool},
    Print{fmt: BinaryFormat, seek: bool},
    SetFill{fill: FullFillType},
    SetOnOff{setting: OnOffSetting, value: bool},
    SetDecHexOff{setting: DecHexOffSetting, value: DecHexOff},
    SetEndian{value: Endianness},
    SetUInt{setting: UIntSetting, value: usize},
    SetSyntax{value: Option<String>},
    Find{expr: Vec<char>, binary: bool, direction: FindDirection}
}

pub fn parse_command_tokens(command: &Vec<char>) -> Result<Vec::<CommandToken>, String> {
    let mut tokens = Vec::<CommandToken>::new();
    let mut current_word = Vec::<char>::new();

    for c in command {
        match c {
            ' ' => {
                match parse_command_token(&current_word)? {
                    Some(token) => tokens.push(token),
                    None => ()
                };
                current_word = Vec::<char>::new();
            },
            _ => current_word.push(*c)
        }
    }

    match parse_command_token(&current_word)? {
        Some(token) => tokens.push(token),
        None => ()
    }

    Ok(tokens)
}


pub fn parse_command(command: &Vec<char>) -> Result<Command, String> {

    match command[0] {
        ':' => {
            let tokens = parse_command_tokens(&command[1..].to_vec())?;

            match tokens.len() {
                1 => {
                    // let hm = &mut editor_stack.editors[editor_stack.current];
                    match &tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Save) => {
                            Ok(Command::SaveAndOrQuit{save: true, quit: false, forced: false})
                        },
                        CommandToken::Keyword(CommandKeyword::SaveAndQuit) => {
                            Ok(Command::SaveAndOrQuit{save: true, quit: true, forced: false})
                        },
                        CommandToken::Keyword(CommandKeyword::ForceQuit) => {
                            Ok(Command::SaveAndOrQuit{save: false, quit: true, forced: true})
                        },
                        CommandToken::Keyword(CommandKeyword::Quit) => {
                            Ok(Command::SaveAndOrQuit{save: false, quit: true, forced: false})
                        },
                        CommandToken::Keyword(CommandKeyword::Manual) => {
                            Ok(Command::Manual)
                        },
                        CommandToken::Keyword(CommandKeyword::Refresh) => {
                            Ok(Command::Refresh)
                        },
                        CommandToken::Keyword(CommandKeyword::Step) => {
                            Ok(Command::Step)
                        },
                        _ => Err("Command not recognized".to_string())
                    }
                },
                2 => {
                    match (&tokens[0], &tokens[1]) {
                        (CommandToken::Keyword(CommandKeyword::Swap), CommandToken::Register(reg)) => {
                            Ok(Command::UnaryRegisterOp{op: UnaryRegisterOp::Swap, reg: *reg})
                        },
                        (CommandToken::Keyword(CommandKeyword::Not),  CommandToken::Register(reg)) => {
                            Ok(Command::UnaryRegisterOp{op: UnaryRegisterOp::Not, reg: *reg})
                        },
                        (CommandToken::Keyword(CommandKeyword::Deflate), CommandToken::Register(reg)) => {
                            Ok(Command::UnaryRegisterOp{op: UnaryRegisterOp::Deflate, reg: *reg})
                        },
                        (CommandToken::Keyword(CommandKeyword::Inflate), CommandToken::Register(reg)) => {
                            Ok(Command::UnaryRegisterOp{op: UnaryRegisterOp::Inflate, reg: *reg})
                        },
                        (CommandToken::Keyword(CommandKeyword::Clear), CommandToken::Register(reg)) => {
                            Ok(Command::UnaryRegisterOp{op: UnaryRegisterOp::Clear, reg: *reg})
                        },
                        (CommandToken::Keyword(CommandKeyword::Clear), CommandToken::Keyword(CommandKeyword::Undo)) => {
                            Ok(Command::ClearUndo)
                        },
                        (CommandToken::Keyword(CommandKeyword::Open), CommandToken::Word(word)) => {
                            Ok(Command::Open{path: word.iter().collect()})
                        },
                        (CommandToken::Keyword(kwrd), _) if matches!(kwrd, CommandKeyword::Print | CommandKeyword::PrintSeek) => { // :p []
                            // let hm = &mut editor_stack.editors[editor_stack.current];

                            let fmt = match &tokens[1] {
                                CommandToken::Keyword(fmt_kwrd) => {
                                    match command_kwrd_to_fmt(fmt_kwrd) {
                                        Ok(f) => f,
                                        _ => return Err("Command not recognized".to_string())
                                    }
                                },
                                _ => return Err("Command not recognized".to_string())
                            };

                            match kwrd {
                                CommandKeyword::Print => Ok(Command::Print{fmt, seek: false}),
                                CommandKeyword::PrintSeek => Ok(Command::Print{fmt, seek: true}),
                                _ => Err("Impossible state".to_string())
                            }

                        },
                        _ => Err("Command not recognized".to_string())
                    }
                },
                3 => {
                    match &tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Set) => { // :set [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Keyword(CommandKeyword::Caps), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    Ok(Command::SetOnOff{setting: OnOffSetting::Caps, value: matches!(kwrd, CommandKeyword::On)})
                                },
                                (CommandToken::Keyword(CommandKeyword::Hex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    Ok(Command::SetOnOff{setting: OnOffSetting::Hex, value: matches!(kwrd, CommandKeyword::On)})
                                },
                                (CommandToken::Keyword(CommandKeyword::Ascii), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    Ok(Command::SetOnOff{setting: OnOffSetting::Ascii, value: matches!(kwrd, CommandKeyword::On)})
                                },
                                (CommandToken::Keyword(CommandKeyword::Filename), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    Ok(Command::SetOnOff{setting: OnOffSetting::Filename, value: matches!(kwrd, CommandKeyword::On)})
                                },
                                (CommandToken::Keyword(CommandKeyword::Icase), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    Ok(Command::SetOnOff{setting: OnOffSetting::Icase, value: matches!(kwrd, CommandKeyword::On)})
                                },
                                (CommandToken::Keyword(CommandKeyword::Regex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    Ok(Command::SetOnOff{setting: OnOffSetting::Regex, value: matches!(kwrd, CommandKeyword::On)})
                                },
                                (CommandToken::Keyword(CommandKeyword::LNum), CommandToken::Keyword(kwrd)) => {
                                    Ok(Command::SetDecHexOff{setting: DecHexOffSetting::LNum, value: DecHexOff::from_keyword(kwrd)?})
                                },
                                (CommandToken::Keyword(CommandKeyword::CNum), CommandToken::Keyword(kwrd)) => {
                                    Ok(Command::SetDecHexOff{setting: DecHexOffSetting::CNum, value: DecHexOff::from_keyword(kwrd)?})
                                },
                                (CommandToken::Keyword(CommandKeyword::Endian), CommandToken::Keyword(kwrd)) => {
                                    Ok(Command::SetEndian{value: Endianness::from_keyword(kwrd)?})
                                },
                                (CommandToken::Keyword(CommandKeyword::Line), CommandToken::Integer(_, value)) => {
                                    Ok(Command::SetUInt{setting: UIntSetting::Line, value: *value})
                                },
                                (CommandToken::Keyword(CommandKeyword::Chunk), CommandToken::Integer(_, value)) => {
                                    Ok(Command::SetUInt{setting: UIntSetting::Chunk, value: *value})
                                },
                                (CommandToken::Keyword(CommandKeyword::CLevel), CommandToken::Integer(_, value)) => {
                                    Ok(Command::SetUInt{setting: UIntSetting::CLevel, value: *value})
                                },
                                (CommandToken::Keyword(CommandKeyword::Undo), CommandToken::Integer(_, value)) => {
                                    Ok(Command::SetUInt{setting: UIntSetting::Undo, value: *value})
                                },
                                (CommandToken::Keyword(CommandKeyword::Syntax), CommandToken::Word(word)) => {
                                    Ok(Command::SetSyntax{value: Some(word.into_iter().collect())})
                                },
                                (CommandToken::Keyword(CommandKeyword::Syntax), CommandToken::Keyword(CommandKeyword::Off)) => {
                                    Ok(Command::SetSyntax{value: None})
                                },
                                (CommandToken::Keyword(CommandKeyword::Fill), CommandToken::Register(reg_id)) => {
                                    Ok(Command::SetFill{fill: FullFillType::Register(*reg_id)})
                                },
                                (CommandToken::Keyword(CommandKeyword::Fill), CommandToken::Word(word)) => {
                                    let v = parse_bytes(word)?;
                                    Ok(Command::SetFill{fill: FullFillType::Bytes(BitField::from_vec(v))})
                                },
                                _ => Err("Command not recognized".to_string())
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::Cat) => { // :cat [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Register(reg), CommandToken::Register(reg2)) => {
                                    Ok(Command::BinaryRegisterOp{op: BinaryRegisterOp::Cat, reg: *reg, other: FullFillType::Register(*reg2)})
                                },
                                (CommandToken::Register(reg), CommandToken::Word(word)) => {
                                    let v = parse_bytes(word)?;
                                    Ok(Command::BinaryRegisterOp{op: BinaryRegisterOp::Cat, reg: *reg, other: FullFillType::Bytes(BitField::from_vec(v))})
                                },
                                _ => Err("Command not recognized".to_string())
                            }
                        },
                        CommandToken::Keyword(kwrd) if matches!(kwrd, CommandKeyword::Ins | CommandKeyword::Ovr) => { // :ins | ovr [] []

                            let value = match &tokens[2] {
                                CommandToken::Integer(word, _) => word,
                                CommandToken::Word(word) => word,
                                _ => return Err("Command not recognized".to_string())
                            };

                            let fmt = match &tokens[1] {
                                CommandToken::Keyword(fmt_kwrd) => {
                                    match command_kwrd_to_fmt(fmt_kwrd) {
                                        Ok(f) => f,
                                        _ => return Err("Command not recognized".to_string())
                                    }
                                },
                                _ => return Err("Command not recognized".to_string())
                            };

                            Ok(Command::Insert{value: value.iter().collect(), fmt, overwrite: matches!(kwrd, CommandKeyword::Ovr)})
                        },
                        CommandToken::Keyword(CommandKeyword::RShift | CommandKeyword::LShift) => { // :rshft | lshft [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Register(reg), CommandToken::Integer(_, n)) => {
                                    let shift = match &tokens[0] {
                                        CommandToken::Keyword(CommandKeyword::RShift) => *n as isize,
                                        CommandToken::Keyword(CommandKeyword::LShift) => -(*n as isize),
                                        _ => return Err("Impossible state".to_string())
                                    };

                                    Ok(Command::UnaryRegisterOp{op: UnaryRegisterOp::Shift{bits: shift}, reg: *reg})
                                },
                                _ => Err("Command not recognized".to_string())
                            }
                        },
                        CommandToken::Keyword(kwrd) => {
                            if let Some(op) = BinaryLogicOp::from_keyword(kwrd) { // :and [] []
                                match (&tokens[1], &tokens[2]) {
                                    (CommandToken::Register(reg), CommandToken::Register(reg2)) => {
                                        Ok(Command::BinaryRegisterOp{op: BinaryRegisterOp::LogicOp(op), reg: *reg, other: FullFillType::Register(*reg2)})
                                    },
                                    (CommandToken::Register(reg), CommandToken::Word(word)) => {
                                        let v = parse_bytes(word)?;
                                        Ok(Command::BinaryRegisterOp{op: BinaryRegisterOp::LogicOp(op), reg: *reg, other: FullFillType::Bytes(BitField::from_vec(v))})
                                    },
                                    _ => Err("Command not recognized".to_string())
                                }
                            } else {
                                Err("Command not recognized".to_string())
                            }
                        },
                        _ => Err("Command not recognized".to_string())
                    }
                },
                4 => {
                    match (&tokens[0], &tokens[1], &tokens[2], &tokens[3]) {
                        (CommandToken::Keyword(CommandKeyword::Slice), CommandToken::Register(reg), 
                                CommandToken::Integer(_, n1), CommandToken::Integer(_, n2)) => {
                            Ok(Command::UnaryRegisterOp{op: UnaryRegisterOp::Slice{start: BitIndex::bytes(*n1), stop: BitIndex::bytes(*n2)}, reg: *reg})
                        },
                        _ => Err("Command not recognized".to_string())
                    }
                }
                _ => Err("Command not recognized".to_string())
            }
        },
        // FIND IN FILE
        '/' => {
            Ok(Command::Find{expr: command[1..].to_vec(), binary: false, direction: FindDirection::Forward})
        },
        '?' => {
            Ok(Command::Find{expr: command[1..].to_vec(), binary: false, direction: FindDirection::Backward})
        },
        '\\' => {
            Ok(Command::Find{expr: command[1..].to_vec(), binary: true, direction: FindDirection::Forward})
        },
        '|' => {
            Ok(Command::Find{expr: command[1..].to_vec(), binary: true, direction: FindDirection::Backward})
        },
        _ => Err("Command not recognized".to_string())
    }
}

#[derive(Copy, Clone)]
pub enum KeystrokeToken {
    Character(char),
    Integer(usize)
}

pub enum KeystrokeCommand {
    Seek{from: FullSeek},
    SeekFindResult{reversed: bool},
    Yank{register: FullRegisterId, size: FullRangeSize},
    Mark{mark_id: FullMarkId},
    Insert{source: FullDataSource},
    Overwrite{source: FullDataSource},
    Delete{bytes: usize},
    Swap{bytes: usize},
    Undo,
    Redo,
    FinishRecordingMacro,
    StartRecordingMacro{macro_id: MacroId},
    RunMacro{macro_id: MacroId},
}

pub fn parse_keystroke(keystroke: &Vec<char>) -> Result<Option<KeystrokeCommand>, String> {

    if let Some(c) = keystroke.iter().last() {
        if !matches!(c, 'g' | 'G' | 'm' | 'f' | 'F' | 'd' | 'y' | 'p' | 'P' | 'u' | 'U' | 'n' | 'N' | 's' | 'q' | 'Q') {
            return Ok(None)
        }
    } else {
        return Ok(None)
    }

    

    let mut tokens = Vec::<KeystrokeToken>::new();
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
                    tokens.push(KeystrokeToken::Integer(current_int));
                    current_int = 0;
                    building_int = false;
                } 
                tokens.push(KeystrokeToken::Character(*c));
            }
        }
    }

    match tokens.len() {
    
        1 => {
            // let hm = &mut self.editors.editors[self.editors.current];
            match tokens[0] {
                KeystrokeToken::Character('g') => {
                    Ok(Some(KeystrokeCommand::Seek{from: FullSeek::FromStart(0)}))
                },
                KeystrokeToken::Character('G') => {
                    Ok(Some(KeystrokeCommand::Seek{from: FullSeek::FromEnd(0)}))
                },
                KeystrokeToken::Character('n') => {
                    Ok(Some(KeystrokeCommand::SeekFindResult{reversed: false}))
                },
                KeystrokeToken::Character('N') => {
                    Ok(Some(KeystrokeCommand::SeekFindResult{reversed: true}))
                },
                KeystrokeToken::Character('m') => {
                    Ok(Some(KeystrokeCommand::Mark{mark_id: FullMarkId::zero()}))
                },
                KeystrokeToken::Character('u') => {
                    Ok(Some(KeystrokeCommand::Undo))
                },
                KeystrokeToken::Character('U') => {
                    Ok(Some(KeystrokeCommand::Redo))
                },
                KeystrokeToken::Character('p') => {
                    Ok(Some(KeystrokeCommand::Insert{source: FullDataSource::Register(FullRegisterId::zero())}))
                },
                KeystrokeToken::Character('P') => {
                    Ok(Some(KeystrokeCommand::Overwrite{source: FullDataSource::Register(FullRegisterId::zero())}))
                },
                KeystrokeToken::Character('Q') => {
                    Ok(Some(KeystrokeCommand::FinishRecordingMacro))
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    Err(format!("Command not recognized: '{}'", s))
                }
            }
        },

        2 => {
            match (tokens[0], tokens[1]) {
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    Ok(Some(KeystrokeCommand::Seek{from: FullSeek::FromStart(n as u64)}))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('G')) => {
                    Ok(Some(KeystrokeCommand::Seek{from: FullSeek::FromEnd(n as i64)}))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('m')) => {
                    match FullMarkId::new(n) {
                        Ok(mark_id) => Ok(Some(KeystrokeCommand::Mark{mark_id})),
                        Err(n) => Err(format!("Mark indices must be less than {}", n).to_string())
                    }
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('f')) => {
                    Ok(Some(KeystrokeCommand::Insert{source: FullDataSource::Fill(n)}))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('F')) => {
                    Ok(Some(KeystrokeCommand::Overwrite{source: FullDataSource::Fill(n)}))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('d')) => {
                    Ok(Some(KeystrokeCommand::Delete{bytes: n}))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('s')) => {
                    Ok(Some(KeystrokeCommand::Swap{bytes: n}))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('y')) => {
                    Ok(Some(KeystrokeCommand::Yank{register: FullRegisterId::zero(), size: FullRangeSize::Bytes(n)}))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('p')) => {
                    match FullRegisterId::new(n) {
                        Ok(reg_id) => Ok(Some(KeystrokeCommand::Insert{source: FullDataSource::Register(reg_id)})),
                        Err(n) => Err(format!("Register indices must be less than {}", n).to_string())
                    }
                    
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('P')) => {
                    match FullRegisterId::new(n) {
                        Ok(reg_id) => Ok(Some(KeystrokeCommand::Overwrite{source: FullDataSource::Register(reg_id)})),
                        Err(n) => Err(format!("Register indices must be less than {}", n).to_string())
                    }
                    
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('q')) => {
                    match MacroId::new(n) {
                        Ok(macro_id) => Ok(Some(KeystrokeCommand::RunMacro{macro_id})),
                        Err(max) => Err(format!("Macro indices must be less than {}", max))
                    }
                    
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('Q')) => {
                    match MacroId::new(n) {
                        Ok(macro_id) => Ok(Some(KeystrokeCommand::StartRecordingMacro{macro_id})),
                        Err(max) => Err(format!("Macro indices must be less than {}", max))
                    }
                    
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    Err(format!("Command not recognized: '{}'", s))
                }
            }
        },

        3 => {
            match (tokens[0], tokens[1], tokens[2]) {
                (KeystrokeToken::Character('+'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    Ok(Some(KeystrokeCommand::Seek{from: FullSeek::FromCurrent(n as i64)}))
                },
                (KeystrokeToken::Character('-'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    Ok(Some(KeystrokeCommand::Seek{from: FullSeek::FromCurrent(-(n as i64))}))
                },
                (KeystrokeToken::Character('`'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    match FullMarkId::new(n) {
                        Ok(mark_id) => Ok(Some(KeystrokeCommand::Seek{from: FullSeek::Mark(mark_id)})),
                        Err(n) => Err(format!("Mark indices must be less than {}", n).to_string())
                    }
                },
                (KeystrokeToken::Character('`'), KeystrokeToken::Integer(n), KeystrokeToken::Character('y')) => {
                    match FullMarkId::new(n) {
                        Ok(mark_id) => Ok(Some(KeystrokeCommand::Yank{register: FullRegisterId::zero(), size: FullRangeSize::UntilMark(mark_id)})),
                        Err(n) => Err(format!("Mark indices must be less than {}", n).to_string())
                    }
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    Err(format!("Command not recognized: '{}'", s))
                }
            }
        },

        4 => {
            match (tokens[0], tokens[1], tokens[2], tokens[3]) {
                (KeystrokeToken::Integer(n1), KeystrokeToken::Character('r'), KeystrokeToken::Integer(n2), KeystrokeToken::Character('y')) => {
                    match FullRegisterId::new(n1) {
                        Ok(reg_id) => Ok(Some(KeystrokeCommand::Yank{register: reg_id, size: FullRangeSize::Bytes(n2)})),
                        Err(n) => Err(format!("Register indices must be less than {}", n).to_string())
                    }
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    Err(format!("Command not recognized: '{}'", s))
                }
            }
        },

        5 => {
            match (tokens[0], tokens[1], tokens[2], tokens[3], tokens[4]) {
                (KeystrokeToken::Integer(n1), KeystrokeToken::Character('r'), KeystrokeToken::Character('`'), KeystrokeToken::Integer(n2), KeystrokeToken::Character('y')) => {
                    match FullRegisterId::new(n1) {
                        Ok(reg_id) => {
                            match FullMarkId::new(n2) {
                                Ok(mark_id) => Ok(Some(KeystrokeCommand::Yank{register: reg_id, size: FullRangeSize::UntilMark(mark_id)})),
                                Err(n) => Err(format!("Mark indices must be less than {}", n).to_string())
                            }
                        },
                        Err(n) => Err(format!("Register indices must be less than {}", n).to_string())
                    }
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    Err(format!("Command not recognized: '{}'", s))
                }
            }
        },
        _ => {
            let s: String = keystroke.iter().collect();
            Err(format!("Command not recognized: '{}'", s))
        }
    }
}