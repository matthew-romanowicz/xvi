use std::rc::Rc;
use std::io::SeekFrom;
use std::io::Write;

use flate2::write::DeflateEncoder;
use flate2::write::DeflateDecoder;
use flate2::Compression;

extern crate pancurses;
use pancurses::{initscr, endwin, Input, noecho, Window, resize_term, start_color, init_color, init_pair, COLORS, COLOR_PAIRS};

mod line_entry;
use crate::line_entry::{AlertType, LineEntry};

mod large_text_view;
use crate::large_text_view::LargeTextView;

mod hex_edit;
use crate::hex_edit::{FileManager, Action, CompoundAction, ActionStack, UpdateDescription, 
        EditMode, ActionResult, ShowType, ByteOperation, FillType, shift_vector, 
        vector_op, HexEdit, DataSource, Structure, make_png};
pub use crate::hex_edit::FileManagerType;


mod parsers;
use crate::parsers::{CommandToken, CommandKeyword, parse_command, parse_bytes, KeystrokeToken, parse_keystroke};

mod bin_format;
use crate::bin_format::{Endianness, UIntFormat, IIntFormat, FloatFormat, BinaryFormat, DataType, fmt_length, from_bytes, to_bytes};

mod globals;
use crate::globals::*;

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

enum EditState {
    Escaped,
    Command,
    Edit,
    Manual
}

const MANUAL_TEXT: &str = r"\c<b>COMMANDS</b>
  <b>SETTINGS:</b>
    :set caps [on|off]  =>  Toggle the case of display hexadecimal values
    :set hex [on|off]   =>  Toggle the <u>hex</u> display
    :set ascii [on|off] =>  Toggle the <u>ascii</u> display
    :set fname [on|off] =>  Toggle the <u>f</u>ile<u>name</u> display
    :set lnum [hex|dec|off]  =>  Toggle <u>l</u>ine <u>num</u>ber display/base
    :set cnum [hex|dec|off]  =>  Toggle <u>c</u>ursor index display/base
    :set line #         =>  Set the number of bytes per <u>l</u>ine to #
    :set fill r#        =>  Set the <u>fill</u> value to the contents of <u>r</u>egister #
    :set fill x#        =>  Set the <u>fill</u> value to # (in he<u>x</u>)
    :set icase [on|off] =>  Set <u>i</u>gnore <u>case</u> for find function
    :set regex [on|off] =>  Enable/disable <u>reg</u>ular <u>ex</u>pressions for find function
    :set undo #         =>  Set the maximum length of the <u>undo</u>/redo stack to #
    :set endian [be|le|ne] =>  Set the <u>endian</u>ness that will be used in the 'ins' and 'ovr' commands
    :set chunk #        =>  Set the <u>chunk</u> size used to insert bytes in swap and live mode
    :set clevel [lo|med|hi] =>  Set the <u>c</u>ompression <u>level</u> used for DEFLATE operations

  <b>INSERTION/OVERWRITE:</b>
    :ins fmt #          =>  <u>Ins</u>ert # encoded as fmt at the cusor location
    :ovr fmt #          =>  <u>Ov</u>e<u>r</u>write # encoded as fmt at the cusor location
    :p fmt              =>  <u>P</u>rint the next group of bytes as fmt
    :ps fmt             =>  <u>P</u>rint the next group of bytes as fmt and <u>s</u>eek to the end of the group

  <b>REGISTER OPERATIONS:</b>
    :slice r# ## ###    =>  <u>Slice</u> the contents of register # to [## ###)
    :cat r# [r##|x##]   =>  Con<u>cat</u>enate contents of <u>r</u>egister ## into <u>r</u>egister #
    :clear r#           =>  Delete the contents of <u>r</u>egister #
    :swap r#            =>  <u>Swap</u> the byte order of <u>r</u>egister #
    :rshft r# ##        =>  Bit<u>shift</u> the contens of <u>r</u>egister # <u>r</u>ight ## bits
    :lshft r# ##        =>  Bit<u>shift</u> the contens of <u>r</u>egister # <u>l</u>eft ## bits
    :not r#             =>  Perform bitwise NOT on <u>r</u>egiser #
    :and r# [r##|x##]   =>  Perform bitwise AND on <u>r</u>egister # with <u>r</u>egister ##
    :or r# [r##|x##]    =>  Perform bitwise OR on <u>r</u>egister # with <u>r</u>egister ##
    :nand r# [r##|x##]  =>  Perform bitwise NAND on <u>r</u>egister # with <u>r</u>egister ##
    :nor r# [r##|x##]   =>  Perform bitwise NOR on <u>r</u>egister # with <u>r</u>egister ##
    :xor r# [r##|x##]   =>  Perform bitwise XOR on <u>r</u>egister # with <u>r</u>egister ##
    :xnor r# [r##|x##]  =>  Perform bitwise XNOR on <u>r</u>egister # with <u>r</u>egister ##
    :deflate r#         =>  Compress the contents of <u>r</u>egister # using DEFLATE
    :inflate r#         =>  Uncompress the contents of <u>r</u>egister # using DEFLATE

  <b>READ/WRITE:</b>
    :w                  =>  <u>W</u>rite all changes to file
    :x                  =>  Write all changes to file and e<u>x</u>it
    :wq                 =>  Write all changes to file and exit
    :q                  =>  Exit if there are no unsaved changes
    :q!                 =>  Exit
    :open file          =>  <u>Open</u> 'file' in another tab

  <b>FIND:</b>
    /expr               =>  Find ASCII 'expr' in file
    ?expr               =>  Backwards find ASCII 'expr' in file
    \expr               =>  <b>[TODO]</b> Find hex 'expr' in file
    |expr               =>  <b>[TODO]</b> Backwards find hex 'expr' in file

  <b>MISC:</b>
    :clear undo         =>  <u>Clear</u> the contents of the <u>undo</u>/redo stack
    :show hist          =>  <b>[TODO]</b> <u>Show</u> command <u>hist</u>ory
    :show m#            =>  <b>[TODO]</b> <u>Show</u> details about <u>m</u>acro number #
    :man                =>  Show application <u>man</u>ual (this text)


\c<b>KEYSTROKE COMMANDS</b>

    ESC     =>  Clear the current keystroke buffer

    i       =>  Change to hex <u>i</u>nsert mode
    I       =>  Change to ASCII <u>i</u>nsert mode
    o       =>  Change to hex <u>o</u>verwrite mode
    O       =>  Change to ASCII <u>o</u>verwrite mode

    g       =>  Move cursor (<u>g</u>o) to start of file
    G       =>  Move cursor (<u>g</u>o) to end of file
    #g      =>  Move cursor (<u>g</u>o) to #th byte from the start of the file
    #G      =>  Move cursor (<u>g</u>o) to #th byte from the end of the file
    +#g     =>  Move cursor (<u>g</u>o) # bytes forward
    -#g     =>  Move cursor (<u>g</u>o) # bytes backward
    n       =>  Seek to <u>n</u>ext find result
    N       =>  Seek to previous find result

    #f      =>  Insert # <u>f</u>ill bytes from the cursor location
    #F      =>  Overwrite # <u>f</u>ill bytes from the cursor location
    #d      =>  <u>D</u>elete next # bytes from the cursor location
    #s      =>  <u>S</u>wap endinanness of next # bytes from the cursor location

    #y      =>  <u>Y</u>ank (copy) next # bytes from the cursor location to register 0
    #r##y   =>  <u>Y</u>ank (copy) next ## bytes from the cursor location to <u>r</u>egister #
    p       =>  <u>P</u>aste contents of register 0 at cursor location
    P       =>  <u>P</u>aste contents of register 0 at cursor location, overwriting current bytes
    #p      =>  <u>P</u>aste contents of register # at cursor location
    #P      =>  <u>P</u>aste contents of register # at cursor location, overwriting current bytes

    t       =>  Change to next open file
    T       =>  <b>[TODO]</b> Change to previous open file

    u       =>  <u>U</u>ndo last action
    U       =>  Redo last action
    #M      =>  Start recording #th <u>m</u>acro
    M       =>  Stop recording <u>m</u>acro
    #m      =>  Run #th <u>m</u>acro";

const BUGS: &str = "Inputting numbers greater than usize maximum in commands/keystrokes causes panic
Setting line length to value greater than width of terminal causes panic
";

struct HexEditManager<'a> {
    hex_edit: HexEdit<'a>,
    action_stack: ActionStack
}

struct EditorStack<'a> {
    editors: Vec<HexEditManager<'a>>,
    current: usize,
    x: usize,
    y: usize,
    width: usize,
    height: usize,
    clipboard_registers: [Vec<u8>; 32],
    endianness: Endianness,
    cnum: ShowType,
    show_hex: bool,
    show_ascii: bool,
    show_filename: bool,
    caps: bool,
    clevel: u8
}

impl<'a> EditorStack<'a> {
    fn new(x: usize, y: usize, width: usize, height: usize) -> EditorStack<'a> {
        EditorStack {
            editors: Vec::new(),
            current: 0,
            x,
            y,
            width,
            height,
            clipboard_registers: Default::default(),
            endianness: Endianness::Network,
            cnum: ShowType::Hex,
            show_hex: true,
            show_ascii: true,
            show_filename: false,
            caps: true,
            clevel: 9
        }
    }

    fn push(&mut self, filename: String, file_manager_type: FileManagerType, extract: bool, file_spec: Option<Rc<Structure>>) -> std::io::Result<usize> {
        let fm = FileManager::new(filename, file_manager_type, extract)?;
        let mut hex_edit = HexEdit::new(fm, self.x, self.y, self.width, self.height,
            16, self.show_filename, self.show_hex, self.show_ascii, // Line Length, Show filename, Show Hex, Show ASCII
            '.', "  ".to_string(), self.caps);
        if let Some(s) = file_spec {
            hex_edit.set_file_spec(s);
        }
        
        self.editors.push(HexEditManager {
            hex_edit,
            action_stack: ActionStack::new(256)
        });

        Ok(self.editors.len() - 1)
    }

    fn set_current(&mut self, index: usize) {
        self.current = index;
    }

    fn tab(&mut self) {
        self.current = (self.current + 1) % self.editors.len();
    }

    fn apply<F>(&mut self, f: F) -> ActionResult where F: Fn(&mut HexEdit) -> ActionResult {
        for (i, hm) in self.editors.iter_mut().enumerate() {
            if i != self.current {
                f(&mut hm.hex_edit);
            }
        }
        f(&mut self.editors[self.current].hex_edit)
    }

    fn set_edit_mode(&mut self, edit_mode: EditMode) -> ActionResult {
        for (i, hm) in self.editors.iter_mut().enumerate() {
            if i != self.current {
                hm.hex_edit.set_edit_mode(edit_mode);
            }
        }
        self.editors[self.current].hex_edit.set_edit_mode(edit_mode)
    }

    fn resize(&mut self, width: usize, height: usize)  {
        for hm in self.editors.iter_mut() {
            hm.hex_edit.resize(width, height);
        }
    }


    // fn current(&mut self) -> &mut HexEditManager {
    //     &mut self.editors[self.current]
    // }
}

enum CommandInstruction {
    NoOp,
    Exit,
    ChangeState(EditState),
    Open(String),
    Refresh
}

fn execute_command(editor_stack: &mut EditorStack, command: Vec<char>) -> (CommandInstruction, ActionResult) {

    match command[0] {
        ':' => {
            let tokens = parse_command(&command[1..].to_vec());
            match tokens.len() {
                1 => {
                    let hm = &mut editor_stack.editors[editor_stack.current];
                    match &tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Save) => {
                            let result = hm.hex_edit.save();
                            if let Some(err) = result.alert {
                                (CommandInstruction::NoOp, ActionResult::error(err.to_string()))
                            } else {
                                (CommandInstruction::NoOp, ActionResult::empty())
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::SaveAndQuit) => {
                            let result = hm.hex_edit.save();
                            if let Some(err) = result.alert {
                                (CommandInstruction::NoOp, ActionResult::error(err.to_string()))
                            } else {
                                (CommandInstruction::Exit, ActionResult::empty())
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::ForceQuit) => {
                            (CommandInstruction::Exit, ActionResult::empty())
                        },
                        CommandToken::Keyword(CommandKeyword::Quit) => {
                            if hm.hex_edit.is_modified() {
                                (CommandInstruction::NoOp, ActionResult::error("File has unsaved changes".to_string()))
                            } else {
                                (CommandInstruction::Exit, ActionResult::empty())
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::Manual) => {
                            (CommandInstruction::ChangeState(EditState::Manual), ActionResult::empty())
                        },
                        CommandToken::Keyword(CommandKeyword::Refresh) => {
                            (CommandInstruction::Refresh, ActionResult::empty())
                        }
                        _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                    }
                }
                2 => {
                    match (&tokens[0], &tokens[1]) {
                        (CommandToken::Keyword(CommandKeyword::Swap), CommandToken::Register(n)) => {
                            if *n < 32 {
                                (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.swap_register(*n as u8))
                            } else if *n < 64 {
                                editor_stack.clipboard_registers[*n - 32].reverse();
                                (CommandInstruction::NoOp, ActionResult::empty())
                            } else {
                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                            }
                            
                        },
                        (CommandToken::Keyword(CommandKeyword::Not), CommandToken::Register(n)) => {
                            if *n < 32 {
                                (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.invert_register(*n as u8))
                            } else if *n < 64 {
                                let new = editor_stack.clipboard_registers[*n - 32].iter().map(|x| !x).collect();
                                editor_stack.clipboard_registers[*n - 32] = new;
                                (CommandInstruction::NoOp, ActionResult::empty())
                            } else {
                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                            }
                            
                        },
                        (CommandToken::Keyword(CommandKeyword::Deflate), CommandToken::Register(n)) => {
                            if *n < 32 {
                                (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.deflate_register(*n as u8, editor_stack.clevel))
                            } else if *n < 64 {
                                let mut e = DeflateEncoder::new(Vec::new(), Compression::new(editor_stack.clevel as u32));
                                if let Err(msg) = e.write_all(&editor_stack.clipboard_registers[*n - 32]) {
                                    (CommandInstruction::NoOp, ActionResult::error(msg.to_string()))
                                } else {
                                    match e.finish() {
                                        Ok(w) => {
                                            editor_stack.clipboard_registers[*n - 32] = w;
                                            (CommandInstruction::NoOp, ActionResult::empty())
                                        },
                                        Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg.to_string()))
                                    }
                                }
                            } else {
                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                            }
                        },
                        (CommandToken::Keyword(CommandKeyword::Inflate), CommandToken::Register(n)) => {
                            if *n < 32 {
                                (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.inflate_register(*n as u8)) // TODO: Make level variable
                            } else if *n < 64 {
                                let mut e = DeflateDecoder::new(Vec::new());
                                if let Err(msg) = e.write_all(&editor_stack.clipboard_registers[*n - 32]) {
                                    (CommandInstruction::NoOp, ActionResult::error(msg.to_string()))
                                } else {
                                    match e.finish() {
                                        Ok(w) => {
                                            editor_stack.clipboard_registers[*n - 32] = w;
                                            (CommandInstruction::NoOp, ActionResult::empty())
                                        },
                                        Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg.to_string()))
                                    }
                                }
                            } else {
                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                            }
                        }
                        (CommandToken::Keyword(CommandKeyword::Clear), CommandToken::Register(n)) => {
                            if *n < 32 {
                                (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.set_register(*n as u8, &Vec::<u8>::new()))
                            } else if *n < 64 {
                                editor_stack.clipboard_registers[*n - 32] = Vec::<u8>::new();
                                (CommandInstruction::NoOp, ActionResult::empty())
                            } else {
                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                            }
                        },
                        (CommandToken::Keyword(CommandKeyword::Clear), CommandToken::Keyword(CommandKeyword::Undo)) => {
                            editor_stack.editors[editor_stack.current].action_stack.clear();
                            (CommandInstruction::NoOp, ActionResult::empty())
                        },
                        (CommandToken::Keyword(CommandKeyword::Open), CommandToken::Word(word)) => {
                            (CommandInstruction::Open(word.iter().collect()), ActionResult::empty())
                        },
                        (CommandToken::Keyword(kwrd), _) if matches!(kwrd, CommandKeyword::Print | CommandKeyword::PrintSeek) => { // :p []
                            let hm = &mut editor_stack.editors[editor_stack.current];

                            let fmt = match &tokens[1] {
                                CommandToken::Keyword(fmt_kwrd) => {
                                    match command_kwrd_to_fmt(fmt_kwrd) {
                                        Ok(f) => f,
                                        _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                                    }
                                },
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            };

                            let n_bytes = fmt_length(&fmt);

                            let mut buffer = vec![0; n_bytes];

                            match hm.hex_edit.get_bytes(hm.hex_edit.get_cursor_pos(), &mut buffer) {
                                Ok(n) if n == n_bytes => {
                                    match from_bytes(&buffer, DataType {fmt, end: editor_stack.endianness}) {
                                        Ok(s) => {
                                            let mut res = match kwrd {
                                                CommandKeyword::Print => ActionResult::empty(),
                                                CommandKeyword::PrintSeek => hm.hex_edit.seek(SeekFrom::Current(n_bytes as i64)),
                                                _ => return (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                                            };
                                            res.set_info(s);
                                            (CommandInstruction::NoOp, res)
                                        },
                                        Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg))
                                    }
                                },
                                Ok(_) => (CommandInstruction::NoOp, ActionResult::error("Not enough bytes for datatype".to_string())),
                                Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg.to_string()))
                            }
                        },
                        _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                    }
                },
                3 => {
                    match &tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Set) => { // :set [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Keyword(CommandKeyword::Caps), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    editor_stack.caps = matches!(kwrd, CommandKeyword::On);
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_capitalize_hex(matches!(kwrd, CommandKeyword::On))))
                                },
                                (CommandToken::Keyword(CommandKeyword::Hex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    editor_stack.show_hex = matches!(kwrd, CommandKeyword::On);
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_show_hex(matches!(kwrd, CommandKeyword::On))))
                                },
                                (CommandToken::Keyword(CommandKeyword::Ascii), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    editor_stack.show_ascii = matches!(kwrd, CommandKeyword::On);
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_show_ascii(matches!(kwrd, CommandKeyword::On))))
                                },
                                (CommandToken::Keyword(CommandKeyword::Filename), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    editor_stack.show_filename = matches!(kwrd, CommandKeyword::On);
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_show_filename(matches!(kwrd, CommandKeyword::On))))
                                },
                                (CommandToken::Keyword(CommandKeyword::Icase), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_ignore_case(matches!(kwrd, CommandKeyword::On))))
                                },
                                (CommandToken::Keyword(CommandKeyword::Regex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_use_regex(matches!(kwrd, CommandKeyword::On))))
                                },
                                (CommandToken::Keyword(CommandKeyword::LNum), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::Off | CommandKeyword::Hex | CommandKeyword::Dec) => {
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_show_lnum(match kwrd {
                                        CommandKeyword::Off => ShowType::Off,
                                        CommandKeyword::Dec => ShowType::Dec,
                                        _ => ShowType::Hex,
                                    })))
                                },
                                (CommandToken::Keyword(CommandKeyword::CNum), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::Off | CommandKeyword::Hex | CommandKeyword::Dec) => {
                                    editor_stack.cnum = match kwrd {
                                        CommandKeyword::Off => ShowType::Off,
                                        CommandKeyword::Dec => ShowType::Dec,
                                        _ => ShowType::Hex,
                                    };
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                },
                                (CommandToken::Keyword(CommandKeyword::Endian), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::BigEndian | CommandKeyword::LittleEndian | CommandKeyword::NetworkEndian) => {
                                    editor_stack.endianness = match kwrd {
                                        CommandKeyword::BigEndian => Endianness::Big,
                                        CommandKeyword::LittleEndian => Endianness::Little,
                                        CommandKeyword::NetworkEndian => Endianness::Network,
                                        _ => return (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                                    };
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                },
                                (CommandToken::Keyword(CommandKeyword::Line), CommandToken::Integer(_, n)) => {
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_line_length(*n as u8)))
                                },
                                (CommandToken::Keyword(CommandKeyword::Chunk), CommandToken::Integer(_, n)) => {
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_block_size(*n)))
                                },
                                (CommandToken::Keyword(CommandKeyword::CLevel), CommandToken::Integer(_, n)) => { // TODO: Need this to work for saving gz files as well
                                    if *n >= 10 {
                                        (CommandInstruction::NoOp, ActionResult::error("Compression level must be 0-9".to_string()))
                                    } else {
                                        editor_stack.clevel = *n as u8;
                                        (CommandInstruction::NoOp, ActionResult::empty())
                                    }
                                },
                                (CommandToken::Keyword(CommandKeyword::Undo), CommandToken::Integer(_, n)) => {
                                    for hm in editor_stack.editors.iter_mut() {
                                        hm.action_stack.set_length(*n);
                                    }
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                },
                                (CommandToken::Keyword(CommandKeyword::Fill), CommandToken::Register(n)) => {
                                    (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.set_fill(FillType::Register(*n as u8)))
                                },
                                (CommandToken::Keyword(CommandKeyword::Fill), CommandToken::Word(word)) => {
                                    match parse_bytes(word) {
                                        Ok(v) => (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.set_fill(FillType::Bytes(v))),
                                        Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg))
                                    }
                                },
                                _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::Cat) => { // :cat [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Register(n1), CommandToken::Register(n2)) => {
                                    if *n1 < 32 {
                                        if *n2 < 32 {
                                            (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.concatenate_register(*n1 as u8, FillType::Register(*n2 as u8)))
                                        } else if *n2 < 64 { 
                                            let fill = FillType::Bytes(editor_stack.clipboard_registers[*n2 - 32].to_vec());
                                            (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.concatenate_register(*n1 as u8, fill))
                                        } else {
                                            (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                        }
                                    } else if *n1 < 64 {
                                        if *n2 < 32 {
                                            let fill = editor_stack.editors[editor_stack.current].hex_edit.get_register(*n2 as u8).unwrap().to_vec();
                                            editor_stack.clipboard_registers[*n1 - 32].extend(fill);
                                            (CommandInstruction::NoOp, ActionResult::empty())
                                        } else if *n2 < 64 { 
                                            let fill = editor_stack.clipboard_registers[*n1 - 32].to_vec();
                                            editor_stack.clipboard_registers[*n1 - 32].extend(fill);
                                            (CommandInstruction::NoOp, ActionResult::empty())
                                        } else {
                                            (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                        }
                                    } else {
                                        (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                    }
                                    
                                },
                                (CommandToken::Register(n), CommandToken::Word(word)) => {
                                    match parse_bytes(word) {
                                        Ok(v) => {
                                            if *n < 32 {
                                                (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.concatenate_register(*n as u8, FillType::Bytes(v)))
                                            } else if *n < 64 {
                                                editor_stack.clipboard_registers[*n - 32].extend(v);
                                                (CommandInstruction::NoOp, ActionResult::empty())
                                            } else {
                                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                            }
                                        },
                                        Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg))
                                    }
                                },
                                _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            }
                        },
                        CommandToken::Keyword(kwrd) if matches!(kwrd, CommandKeyword::And | CommandKeyword::Or | CommandKeyword::Nand | CommandKeyword::Nor | CommandKeyword::Xor | CommandKeyword::Xnor) => { // :and [] []
                            let op = match kwrd {
                                CommandKeyword::And => ByteOperation::And,
                                CommandKeyword::Or => ByteOperation::Or,
                                CommandKeyword::Nand => ByteOperation::Nand,
                                CommandKeyword::Nor => ByteOperation::Nor,
                                CommandKeyword::Xor => ByteOperation::Xor,
                                CommandKeyword::Xnor => ByteOperation::Xnor,
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                            };
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Register(n1), CommandToken::Register(n2)) => {
                                    if *n1 < 32 {
                                        if *n2 < 32 {
                                            (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.manipulate_register(*n1 as u8, FillType::Register(*n2 as u8), op))
                                        } else if *n2 < 64 { 
                                            let fill = FillType::Bytes(editor_stack.clipboard_registers[*n2 - 32].to_vec());
                                            (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.manipulate_register(*n1 as u8, fill, op))
                                        } else {
                                            (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                        }
                                    } else if *n1 < 64 {
                                        if *n2 < 32 {
                                            let fill = editor_stack.editors[editor_stack.current].hex_edit.get_register(*n2 as u8).unwrap().to_vec();
                                            vector_op(&mut editor_stack.clipboard_registers[*n1 - 32], &fill, |a, b| a & b);
                                            (CommandInstruction::NoOp, ActionResult::empty())
                                        } else if *n2 < 64 { 
                                            let fill = editor_stack.clipboard_registers[*n1 - 32].to_vec();
                                            vector_op(&mut editor_stack.clipboard_registers[*n1 - 32], &fill, |a, b| a & b);
                                            (CommandInstruction::NoOp, ActionResult::empty())
                                        } else {
                                            (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                        }
                                    } else {
                                        (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                    }
                                    
                                },
                                (CommandToken::Register(n), CommandToken::Word(word)) => {
                                    match parse_bytes(word) {
                                        Ok(v) => {
                                            if *n < 32 {
                                                (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.manipulate_register(*n as u8, FillType::Bytes(v), op))
                                            } else if *n < 64 {
                                                vector_op(&mut editor_stack.clipboard_registers[*n - 32], &v, |a, b| a & b);
                                                (CommandInstruction::NoOp, ActionResult::empty())
                                            } else {
                                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                            }
                                        },
                                        Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg))
                                    }
                                },
                                _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            }
                        },
                        CommandToken::Keyword(kwrd) if matches!(kwrd, CommandKeyword::Ins | CommandKeyword::Ovr) => { // :ins | ovr [] []
                            let hm = &mut editor_stack.editors[editor_stack.current];
                            let input = match &tokens[2] {
                                CommandToken::Integer(word, _) => word,
                                CommandToken::Word(word) => word,
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            };

                            let fmt = match &tokens[1] {
                                CommandToken::Keyword(fmt_kwrd) => {
                                    match command_kwrd_to_fmt(fmt_kwrd) {
                                        Ok(f) => f,
                                        _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                                    }
                                },
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            };


                            match to_bytes(input, DataType {fmt, end: editor_stack.endianness}){
                                Ok(bytes) => {
                                    match kwrd {
                                        CommandKeyword::Ins => (CommandInstruction::NoOp, hm.hex_edit.insert(DataSource::Bytes(bytes.to_vec()))),
                                        CommandKeyword::Ovr => (CommandInstruction::NoOp, hm.hex_edit.overwrite(DataSource::Bytes(bytes.to_vec()))),
                                        _ => (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                                    }
                                },
                                Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg))
                            }
                        
                        },
                        CommandToken::Keyword(CommandKeyword::RShift | CommandKeyword::LShift) => { // :rshft | lshft [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Register(register), CommandToken::Integer(_, n)) => {
                                    let shift = match &tokens[0] {
                                        CommandToken::Keyword(CommandKeyword::RShift) => *n as i8,
                                        CommandToken::Keyword(CommandKeyword::LShift) => -(*n as i8),
                                        _ => return (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                                    };

                                    if *register < 32 {
                                        (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.shift_register(*register as u8, shift))
                                    } else if *register < 64 {
                                        match shift_vector(&mut editor_stack.clipboard_registers[*register - 32], shift) {
                                            Ok(()) => (CommandInstruction::NoOp, ActionResult::empty()),
                                            Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg.to_string()))
                                        }
                                        
                                    } else {
                                        (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                    }
                                },
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            }
                        },
                        _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                    }
                },
                4 => {
                    match (&tokens[0], &tokens[1], &tokens[2], &tokens[3]) {
                        (CommandToken::Keyword(CommandKeyword::Slice), CommandToken::Register(register), CommandToken::Integer(_, n1), CommandToken::Integer(_, n2)) => {
                            if *register < 32 {
                                (CommandInstruction::NoOp, editor_stack.editors[editor_stack.current].hex_edit.slice_register(*register as u8, *n1, *n2))
                            } else if *register < 64 {
                                if *n2 >= editor_stack.clipboard_registers[*register - 32].len() {
                                    (CommandInstruction::NoOp, ActionResult::error("Slice index outside of register contents bounds".to_string()))
                                } else if *n1 > *n2 {
                                    (CommandInstruction::NoOp, ActionResult::error("Slice start index greater than slice end index".to_string()))
                                } else {
                                    let temp = &editor_stack.clipboard_registers[*register - 32][*n1..*n2];
                                    editor_stack.clipboard_registers[*register - 32] = temp.to_vec();
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                }
                            } else {
                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                            }
                        },
                        _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                    }
                }
                _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
            }
        },
        // FIND IN FILE
        '/' => {
            let hm = &mut editor_stack.editors[editor_stack.current];
            hm.hex_edit.clear_find();
            hm.hex_edit.find(&command[1..].to_vec());
            (CommandInstruction::NoOp, hm.hex_edit.seek_next())
        },
        '?' => {
            let hm = &mut editor_stack.editors[editor_stack.current];
            hm.hex_edit.clear_find();
            hm.hex_edit.find(&command[1..].to_vec());
            (CommandInstruction::NoOp, hm.hex_edit.seek_prev())
        },
        '\\' => {
            let mut word = command[1..].to_vec();
            word.insert(0, 'x');
            match parse_bytes(&word) {
                Ok(v) => {
                    let hm = &mut editor_stack.editors[editor_stack.current];
                    hm.hex_edit.clear_find();
                    hm.hex_edit.find_bytes(&v);
                    (CommandInstruction::NoOp, hm.hex_edit.seek_next())
                },
                Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg))
            }
        }
        _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
    }

    //Ok(())
}

struct MacroManager {
    macros: [Option<Rc<CompoundAction>>; 32], //Needs to be 32 or less for Default::default() to work
    start_index: usize,
    current_macro: Option<u8>
}

impl MacroManager {

    fn new() -> MacroManager {
        MacroManager {
            macros: Default::default(),
            start_index: 0,
            current_macro: None
        }
    }

    fn get(&self, n: u8) -> Option<Rc<CompoundAction>> {
        if n < 32 {
            match &self.macros[n as usize] {
                Some(m) => Some(Rc::clone(&m)),
                None => None
            }
        } else {
            None
        }
    }

    fn start(&mut self, n: u8, action_stack: &ActionStack) -> ActionResult {
        self.start_index = action_stack.current_index();
        self.current_macro = Some(n);
        ActionResult::empty()
    }

    fn finish(&mut self, action_stack: &ActionStack) -> ActionResult {
        match self.current_macro {
            Some(n) => {
                self.macros[n as usize] = Some(Rc::new(action_stack.combine(self.start_index, action_stack.current_index())));
                self.current_macro = None;
                ActionResult::empty()
            },
            None => ActionResult::error("No macros being recorded".to_string())
        }
    }

    fn run(&self, n: u8, hex_edit: &mut HexEdit) -> ActionResult {
        match &self.macros[n as usize] {
            Some(m) => m.redo(hex_edit),
            None => ActionResult::error(format!("Macro {} not defined", n))
        }
    }
}

fn execute_keystroke(editor_stack: &mut EditorStack, macro_manager: &mut MacroManager, keystroke: Vec<char>) -> ActionResult {

    let tokens = parse_keystroke(&keystroke);
    match tokens.len() {

        1 => {
            let hm = &mut editor_stack.editors[editor_stack.current];
            match tokens[0] {
                KeystrokeToken::Character('g') => {
                    hm.hex_edit.seek(SeekFrom::Start(0))
                },
                KeystrokeToken::Character('G') => {
                    hm.hex_edit.seek(SeekFrom::End(0))
                },
                KeystrokeToken::Character('n') => {
                    hm.hex_edit.seek_next()
                },
                KeystrokeToken::Character('N') => {
                    hm.hex_edit.seek_prev()
                },
                KeystrokeToken::Character('u') => {
                    hm.action_stack.undo(&mut hm.hex_edit)
                },
                KeystrokeToken::Character('U') => {
                    hm.action_stack.redo(&mut hm.hex_edit)
                },
                KeystrokeToken::Character('p') => {
                    hm.hex_edit.insert(DataSource::Register(0))
                },
                KeystrokeToken::Character('P') => {
                    hm.hex_edit.overwrite(DataSource::Register(0))
                },
                KeystrokeToken::Character('M') => {
                    macro_manager.finish(&hm.action_stack)
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    ActionResult::error(format!("Command not recognized: '{}'", s))
                }
            }
        },

        2 => {
            match (tokens[0], tokens[1]) {
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    editor_stack.editors[editor_stack.current].hex_edit.seek(SeekFrom::Start(n as u64))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('G')) => {
                    editor_stack.editors[editor_stack.current].hex_edit.seek(SeekFrom::End(n as i64))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('f')) => {
                    editor_stack.editors[editor_stack.current].hex_edit.insert(DataSource::Fill(n))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('F')) => {
                    editor_stack.editors[editor_stack.current].hex_edit.overwrite(DataSource::Fill(n))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('d')) => {
                    editor_stack.editors[editor_stack.current].hex_edit.delete_bytes(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('s')) => {
                    editor_stack.editors[editor_stack.current].hex_edit.swap_bytes(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('y')) => {
                    editor_stack.editors[editor_stack.current].hex_edit.yank(0, n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('p')) => {
                    if n < 32 {
                        editor_stack.editors[editor_stack.current].hex_edit.insert(DataSource::Register(n as u8))
                    } else if n < 64 {
                        let v = editor_stack.clipboard_registers[n - 32].to_vec();
                        editor_stack.editors[editor_stack.current].hex_edit.insert(DataSource::Bytes(v))
                    } else {
                        ActionResult::error("Register indices must be less than 64".to_string())
                    }
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('P')) => {
                    if n < 32 {
                        editor_stack.editors[editor_stack.current].hex_edit.overwrite(DataSource::Register(n as u8))
                    } else if n < 64 {
                        let v = editor_stack.clipboard_registers[n - 32].to_vec();
                        editor_stack.editors[editor_stack.current].hex_edit.overwrite(DataSource::Bytes(v))
                    } else {
                        ActionResult::error("Register indices must be less than 64".to_string())
                    }
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('m')) => {
                    macro_manager.run(n as u8, &mut editor_stack.editors[editor_stack.current].hex_edit)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('M')) => {
                    macro_manager.start(n as u8, &editor_stack.editors[editor_stack.current].action_stack)
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    ActionResult::error(format!("Command not recognized: '{}'", s))
                }
            }
        },

        3 => {
            let hm = &mut editor_stack.editors[editor_stack.current].hex_edit;
            match (tokens[0], tokens[1], tokens[2]) {
                (KeystrokeToken::Character('+'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    hm.seek(SeekFrom::Current(n as i64))
                },
                (KeystrokeToken::Character('-'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    hm.seek(SeekFrom::Current(-(n as i64)))
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    ActionResult::error(format!("Command not recognized: '{}'", s))
                }
            }
        }

        4 => {
            match (tokens[0], tokens[1], tokens[2], tokens[3]) {
                (KeystrokeToken::Integer(n1), KeystrokeToken::Character('r'), KeystrokeToken::Integer(n2), KeystrokeToken::Character('y')) => {
                    if n1 < 32 {
                        editor_stack.editors[editor_stack.current].hex_edit.yank(n1 as u8, n2)
                    } else if n1 < 64 {
                        let pos = editor_stack.editors[editor_stack.current].hex_edit.get_cursor_pos();
                        let mut v: Vec<u8> = vec![0;n2];
                        let res = editor_stack.editors[editor_stack.current].hex_edit.get_bytes(pos, &mut v);
                        match res {
                            Ok(_) => { // TODO: Check if this is less han n2
                                editor_stack.clipboard_registers[n1 - 32] = v.to_vec();
                                ActionResult::empty()
                            },
                            Err(msg) => ActionResult::error(msg.to_string())
                        } 
                    } else {
                        ActionResult::error("Register indices must be less than 64".to_string())
                    }
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    ActionResult::error(format!("Command not recognized: '{}'", s))
                }
            }
        }
        _ => {
            let s: String = keystroke.iter().collect();
            ActionResult::error(format!("Command not recognized: '{}'", s))
        }
    }
}

fn alert(text: String, alert_type: AlertType, window: &mut Window, line_entry: &mut LineEntry, hex_edit: &mut HexEdit) {
    line_entry.alert(text.chars().collect(), alert_type);
    line_entry.draw(window);
    hex_edit.refresh_cursor(window);
}

pub fn init_colors() {
    start_color();
    println!("Colors: {} Color Pairs: {}", COLORS(), COLOR_PAIRS());
    // init_color(globals::LIGHT_RED, 500, 0, 0);
    // init_color(globals::LIGHT_YELLOW, 500, 500, 0);
    // init_color(globals::GRAY, 500, 500, 500);
    init_pair(globals::ERROR_COLOR as i16, pancurses::COLOR_RED, pancurses::COLOR_BLACK);
    init_pair(globals::HIGHLIGHT_COLOR as i16, pancurses::COLOR_BLACK, pancurses::COLOR_YELLOW);
    init_pair(globals::CURSOR_COLOR as i16, pancurses::COLOR_BLACK, pancurses::COLOR_WHITE);
    init_pair(globals::WARNING_COLOR as i16, pancurses::COLOR_YELLOW, pancurses::COLOR_BLACK);
    init_pair(globals::BAD_ASCII_COLOR as i16, globals::BRIGHT_RED, pancurses::COLOR_BLACK);
    init_pair(globals::EOF_COLOR as i16, globals::BRIGHT_BLACK, pancurses::COLOR_BLACK);
    init_pair(globals::INFO_COLOR as i16, pancurses::COLOR_CYAN, pancurses::COLOR_BLACK);
    init_pair(globals::CURRENT_FIELD_COLOR as i16, pancurses::COLOR_BLACK, pancurses::COLOR_CYAN);
}

pub fn run(filename: String, file_manager_type: FileManagerType, extract: bool) -> std::io::Result<()> {

    let mut edit_state = EditState::Escaped;
    let mut window = initscr();
    window.refresh();
    window.keypad(true);
    noecho();

    init_colors();

    let png = Rc::new(make_png());

    let mut current_keystroke = Vec::<char>::new();

    let mut editors = EditorStack::new(0, 0, window.get_max_x() as usize - 1, window.get_max_y()as usize - 1);
    if let Err(msg) = editors.push(filename, file_manager_type, extract, Some(png)) {
        eprintln!("{}", msg);
        std::process::exit(1);
    };

    let mut macro_manager = MacroManager::new();

    let cursor_index_len: usize;

    {
        let hm = &mut editors.editors[editors.current];

        cursor_index_len = format!("{:x}", hm.hex_edit.len()).len();
        hm.hex_edit.set_viewport_row(0)?;
        hm.hex_edit.draw(&mut window);
    }

    let mut command_history = Vec::<Vec<char>>::new();
    let mut command_history_index = 0;

    let mut line_entry = LineEntry::new(window.get_max_x() as usize - 2 - cursor_index_len, 0, window.get_max_y() as usize - 1);

    let mut manual_view = LargeTextView::new(0, 0, window.get_max_x() as usize - 1, window.get_max_y() as usize - 1, MANUAL_TEXT.to_string());

    loop {
        let ch_input = window.getch();
        if line_entry.alerting() {
            line_entry.unalert();
            line_entry.draw(&mut window);
        }


        match edit_state {
            EditState::Escaped => {
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                        editors.resize(window.get_max_x() as usize - 1, window.get_max_y()as usize - 1);
                        line_entry.reset_geometry(window.get_max_x() as usize - 2 - cursor_index_len, 0, window.get_max_y() as usize - 1);
                        line_entry.draw(&mut window);
                        editors.editors[editors.current].hex_edit.update(&mut window, UpdateDescription::All); // TODO: Handle this result
                    },
                    Some(Input::Character('\u{1b}')) => {
                        current_keystroke = Vec::<char>::new();
                    },
                    Some(Input::Character('o')) => {
                        edit_state = EditState::Edit;
                        editors.set_edit_mode(EditMode::HexOverwrite);
                        editors.editors[editors.current].hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('O')) => {
                        edit_state = EditState::Edit;
                        editors.set_edit_mode(EditMode::AsciiOverwrite);
                        editors.editors[editors.current].hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('i')) => {
                        edit_state = EditState::Edit;
                        editors.set_edit_mode(EditMode::HexInsert);
                        editors.editors[editors.current].hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('I')) => {
                        edit_state = EditState::Edit;
                        editors.set_edit_mode(EditMode::AsciiInsert);
                        editors.editors[editors.current].hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character(':')) | Some(Input::Character('/')) | Some(Input::Character('\\')) => {
                        edit_state = EditState::Command;
                        line_entry.addch(ch_input.unwrap()); 
                        line_entry.draw(&mut window);
                    }, 
                    Some(Input::Character('t')) => {
                        editors.tab();
                        editors.editors[editors.current].hex_edit.update(&mut window, UpdateDescription::All); // TODO: Handle this result
                        let fname = editors.editors[editors.current].hex_edit.filename();
                        alert(fname, AlertType::Info, &mut window, &mut line_entry, &mut editors.editors[editors.current].hex_edit);
                    }
                    Some(Input::Character(c)) => {
                        current_keystroke.push(c);
                        if matches!(c, 'g' | 'G' | 'f' | 'F' | 'd' | 'y' | 'p' | 'P' | 'u' | 'U' | 'n' | 'N' | 's' | 'm' | 'M') {
                            let result = execute_keystroke(&mut editors, &mut macro_manager, current_keystroke);
                            let hm = &mut editors.editors[editors.current];
                            if let Some(err) = result.alert {
                                alert(err, result.alert_type.clone(), &mut window, &mut line_entry, &mut hm.hex_edit);
                            }
                            if let Some(action) = result.action {
                                hm.action_stack.add(action);
                            }
                            let result = hm.hex_edit.update(&mut window, result.update);
                            if let Err(err) = result {
                                alert(err.to_string(), AlertType::Error, &mut window, &mut line_entry, &mut hm.hex_edit);
                            }
                            current_keystroke = Vec::<char>::new();
                        }
                    },
                    Some(ch) if matches!(ch, Input::KeyRight | Input::KeyLeft | Input::KeyUp | Input::KeyDown | Input::KeyNPage | Input::KeyPPage | Input::KeyHome | Input::KeyEnd) => {
                        let hm = &mut editors.editors[editors.current];
                        let result = hm.hex_edit.addch(ch);
                        if let Some(err) = result.alert {
                            alert(err, result.alert_type.clone(), &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                        if let Some(action) = result.action {
                            hm.action_stack.add(action);
                        }
                        let result = hm.hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), AlertType::Error, &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                    },
                    _ => {
                        alert("Invalid Keystroke".to_string(), AlertType::Error, &mut window, &mut line_entry, &mut editors.editors[editors.current].hex_edit)
                    }
                }
            },
            EditState::Command => {

                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    },
                    Some(Input::KeyUp) => {
                        if command_history_index > 0 {
                            command_history_index -= 1;
                            line_entry.set_text(command_history[command_history_index].to_vec());
                            line_entry.draw(&mut window);
                        }
                    },
                    Some(Input::KeyDown) => {
                        if command_history_index + 1 < command_history.len() {
                            command_history_index += 1;
                            line_entry.set_text(command_history[command_history_index].to_vec());
                            line_entry.draw(&mut window);
                        }
                    },
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                    },
                    Some(Input::Character('\n')) => {
                        //println!("Newline: {}", String::from_iter(line_entry.get_text()));
                        command_history.push(line_entry.get_text());
                        command_history_index = command_history.len();

                        let (instr, result) = execute_command(&mut editors, line_entry.get_text());

                        let hm = &mut editors.editors[editors.current];

                        if let Some(err) = result.alert {
                            alert(err, result.alert_type.clone(), &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                        if let Some(action) = result.action {
                            hm.action_stack.add(action);
                        }
                        let result = hm.hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), AlertType::Error, &mut window, &mut line_entry, &mut hm.hex_edit);
                        }

                        line_entry.clear();
                        line_entry.draw(&mut window);

                        match instr {
                            CommandInstruction::NoOp => {
                                edit_state = EditState::Escaped;
                                hm.hex_edit.refresh_cursor(&mut window);
                            },
                            CommandInstruction::Exit => {
                                endwin();
                                std::process::exit(0);
                            },
                            CommandInstruction::ChangeState(EditState::Manual) => {
                                edit_state = EditState::Manual;
                                manual_view.resize(window.get_max_x() as usize - 1, window.get_max_y() as usize - 1);
                                manual_view.draw(&mut window);
                            },
                            CommandInstruction::Open(filename) => {
                                if let Ok(i) = editors.push(filename, FileManagerType::ReadOnly, extract, None) {
                                    editors.current = i;
                                    editors.editors[editors.current].hex_edit.set_viewport_row(0);
                                    editors.editors[editors.current].hex_edit.draw(&mut window);//.update(&mut window, UpdateDescription::All);
                                    edit_state = EditState::Escaped;
                                }
                            },
                            CommandInstruction::Refresh => {
                                ///resize_term(0, 0);
                                endwin();
                                window = initscr();
                                window.refresh();
                                window.keypad(true);
                                noecho();
                                init_colors();
                                //window.refresh();
                                editors.resize(window.get_max_x() as usize - 1, window.get_max_y()as usize - 1);
                                //panic!("{} {}", window.get_max_x(), window.get_max_y());
                                line_entry.reset_geometry(window.get_max_x() as usize - 2 - cursor_index_len, 0, window.get_max_y() as usize - 1);
                                line_entry.draw(&mut window);
                                editors.editors[editors.current].hex_edit.update(&mut window, UpdateDescription::All); // TODO: Handle this result
                            }
                            _ => () // This should never be hit
                        }
                    },
                    Some(ch) => { 
                        line_entry.addch(ch); 
                        line_entry.draw(&mut window);
                    },
                    None => ()
                }
            },
            EditState::Edit => {
                let hm = &mut editors.editors[editors.current];
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    }
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                    },
                    Some(ch) => { 
                        let result = hm.hex_edit.addch(ch);
                        if let Some(err) = result.alert {
                            alert(err, result.alert_type.clone(), &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                        if let Some(action) = result.action {
                            hm.action_stack.add(action);
                        }
                        let result = hm.hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), AlertType::Error, &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                    },
                    None => ()
                }
            },
            EditState::Manual => {
                let hm = &mut editors.editors[editors.current];
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                        manual_view.resize(window.get_max_x() as usize - 1, window.get_max_y() as usize - 1);
                        manual_view.draw(&mut window);
                    },
                    Some(Input::Character('\u{1b}')) => { // escape
                        edit_state = EditState::Escaped;
                        hm.hex_edit.draw(&mut window);
                        line_entry.draw(&mut window);
                    },
                    Some(ch) if matches!(ch, Input::KeyUp | Input::KeyDown | Input::KeyHome | Input::KeyEnd | Input::KeyPPage | Input::KeyNPage) => {
                        manual_view.addch(ch);
                        manual_view.draw(&mut window);
                    },
                    _ => alert("Invalid Keystroke".to_string(), AlertType::Error, &mut window, &mut line_entry, &mut editors.editors[editors.current].hex_edit)
                }
            }
        }

        let (y, x) = window.get_cur_yx();

        // Draw cursor index
        let pos = editors.editors[editors.current].hex_edit.get_cursor_pos();
        let length = editors.editors[editors.current].hex_edit.len();
        let caps_hex = editors.editors[editors.current].hex_edit.get_capitalize_hex();
        let cursor_label_len = match editors.cnum {
            ShowType::Off => 0,
            ShowType::Dec => format!("{}", length).len(),
            ShowType::Hex => format!("{:x}", length).len()
        };
        let cursor_index_string = match editors.cnum {
            ShowType::Off => "".to_string(),
            ShowType::Dec => format!("{:0width$}", pos, width=cursor_label_len),
            ShowType::Hex => match caps_hex {
                true => format!("{:0width$x}", pos, width=cursor_label_len).to_ascii_uppercase(),
                false => format!("{:0width$x}", pos, width=cursor_label_len)
            }
        };
        window.mvaddstr((window.get_max_y() as usize - 1) as i32,
        (window.get_max_x() as usize - 1 - cursor_label_len) as i32, 
        cursor_index_string);

        window.mv(y, x);

    }

}
