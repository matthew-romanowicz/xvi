use std::rc::Rc;
use std::io::SeekFrom;

extern crate pancurses;
use pancurses::{initscr, endwin, Input, noecho, Window, resize_term};

mod line_entry;
use crate::line_entry::LineEntry;

mod large_text_view;
use crate::large_text_view::LargeTextView;

mod hex_edit;
use crate::hex_edit::{FileManager, Action, CompoundAction, ActionStack, UpdateDescription, EditMode, ActionResult, ShowType, FillType, HexEdit};
pub use crate::hex_edit::FileManagerType;

mod parsers;
use crate::parsers::{CommandToken, CommandKeyword, parse_command, parse_bytes, KeystrokeToken, parse_keystroke};

mod bin_format;
use crate::bin_format::{Endianness, UIntFormat, IIntFormat, FloatFormat, BinaryFormat, DataType, to_bytes};

enum EditState {
    Escaped,
    Command,
    Edit,
    Manual
}

const MANUAL_TEXT: &str = r"\b\cCOMMANDS
\b  SETTINGS:
    :set caps [on|off]  =>  Toggle the case of display hexadecimal values
    :set hex [on|off]   =>  Toggle the hex display
    :set ascii [on|off] =>  Toggle the ascii display
    :set lnum [hex|dec|off]  =>  Toggle line number display/base
    :set cnum [hex|dec|off]  =>  [TODO] Toggle cursor index display/base
    :set line #         =>  Set the number of bytes per line to #
    :set fill r#        =>  Set the fill value to the contents of register #
    :set fill x#        =>  Set the fill value to # (in hex)
    :set icase [on|off] =>  Set ignore case for find function
    :set regex [on|off] =>  Enable/disable regular expressions for find function
    :set undo #         =>  Set the maximum length of the undo/redo stack to #
    :set endian [be|le|ne] =>  Set the endianness that will be used in the 'ins' and 'ovr' commands
    :set chunk #        =>  Set the chunk size used to insert bytes in swap and live mode

\b  INSERTION/OVERWRITE:
    :ins fmt #          =>  Insert # encoded as fmt at the cusor location
    :ovr fmt #          =>  Overwrite # encoded as fmt at the cusor location

\b  REGISTER OPERATIONS:
    :cat r# r##         =>  Concatenate contents of listed register ## into register #
    :cat r# x##         =>  Concatenate ## (in hex) into register #
    :clear r#           =>  Delete the contents of register #
    :swap r#            =>  Swap the byte order of register #
    :rshft r# ##        =>  Bitshift the contens of register # right ## bits
    :lshft r# ##        =>  Bitshift the contens of register # left ## bits
    :and r# r##         =>  [TODO] Perform bitwise AND on register # with register ##
    :or r# r##          =>  [TODO] Perform bitwise OR on register # with register ##
    :not r#             =>  Perform bitwise NOT on regiser #

\b  READ/WRITE:
    :w                  =>  Write all changes to file
    :x                  =>  Write all changes to file and exit
    :wq                 =>  Write all changes to file and exit
    :q                  =>  Exit if there are no unsaved changes
    :q!                 =>  Exit
    :open file          =>  Opens 'file' in another tab

\b  FIND:
    /expr               =>  Find ASCII 'expr' in file
    ?expr               =>  Backwards find ASCII 'expr' in file
    \expr               =>  [TODO] Find hex 'expr' in file
    |expr               =>  [TODO] Backwards find hex 'expr' in file

\b  MISC:
    :clear undo         =>  Clear the contents of the undo/redo stack
    :show hist          =>  [TODO] Show command history
    :show m#            =>  [TODO] Show details about macro number #
    :man                =>  Show application manual (this text)


\b\cKEYSTROKE COMMANDS

    ESC     =>  Clear the current keystroke buffer

    i       =>  Change to hex insert mode
    I       =>  Change to ASCII insert mode
    o       =>  Change to hex overwrite mode
    O       =>  Change to ASCII overwrite mode

    g       =>  Move cursor to start of file
    G       =>  Move cursor to end of file
    #g      =>  Move cursor to #th byte from the start of the file
    #G      =>  Move cursor to #th byte from the end of the file
    +#g     =>  Move cursor # bytes forward
    -#g     =>  Move cursor # bytes backward
    n       =>  Seek to next find result
    N       =>  Seek to previous find result

    #f      =>  Insert next # bytes from the cursor location
    #F      =>  Overwrite next # bytes from the cursor location
    #d      =>  Delete next # bytes from the cursor location
    #s      =>  Swap endinanness of next # bytes from the cursor location

    #y      =>  Yank (copy) next # bytes from the cursor location to register 0
    #r##y   =>  Yank (copy) next ## bytes from the cursor location to register #
    p       =>  Insert contents of register # at cursor location
    P       =>  Overwrite bytes with contents of register # at cursor location
    #p      =>  Insert contents of register # at cursor location
    #P      =>  Overwrite bytes with contents of register # at cursor location

    t       =>  Change to next open file
    T       =>  [TODO] Change to previous open file

    u       =>  Undo last action
    U       =>  Redo last action
    #M      =>  Start recording #th macro
    M       =>  Stop recording macro
    #m      =>  Run #th macro";

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
    endianness: Endianness
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
            endianness: Endianness::Network
        }
    }

    fn push(&mut self, filename: String, file_manager_type: FileManagerType, extract: bool) -> std::io::Result<usize> {
        let fm = FileManager::new(filename, file_manager_type, extract)?;
        self.editors.push(HexEditManager {
            hex_edit: HexEdit::new(fm, self.x, self.y, self.width, self.height,
                16, true, true, // Line Length, Show Hex, Show ASCII
                '.', "  ".to_string(), true), // Invalid ASCII, Separator, Capitalize Hex
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
        for (i, mut hm) in self.editors.iter_mut().enumerate() {
            if i != self.current {
                f(&mut hm.hex_edit);
            }
        }
        f(&mut self.editors[self.current].hex_edit)
    }

    fn set_edit_mode(&mut self, edit_mode: EditMode) -> ActionResult {
        for (i, mut hm) in self.editors.iter_mut().enumerate() {
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
    Open(String)
}

fn execute_command(editor_stack: &mut EditorStack, command: Vec<char>) -> (CommandInstruction, ActionResult) {

    match command[0] {
        ':' => {
            let tokens = parse_command(&command[1..].to_vec());
            match tokens.len() {
                1 => {
                    let mut hm = &mut editor_stack.editors[editor_stack.current];
                    match &tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Save) => {
                            let result = hm.hex_edit.save();
                            if let Some(err) = result.error {
                                (CommandInstruction::NoOp, ActionResult::error(err.to_string()))
                            } else {
                                (CommandInstruction::NoOp, ActionResult::empty())
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::SaveAndQuit) => {
                            let result = hm.hex_edit.save();
                            if let Some(err) = result.error {
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
                        _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                    }
                }
                3 => {
                    match &tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Set) => { // :set [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Keyword(CommandKeyword::Caps), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_capitalize_hex(matches!(kwrd, CommandKeyword::On))))
                                },
                                (CommandToken::Keyword(CommandKeyword::Hex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_show_hex(matches!(kwrd, CommandKeyword::On))))
                                },
                                (CommandToken::Keyword(CommandKeyword::Ascii), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, editor_stack.apply(|h| h.set_show_ascii(matches!(kwrd, CommandKeyword::On))))
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
                                (CommandToken::Keyword(CommandKeyword::Undo), CommandToken::Integer(_, n)) => {
                                    for mut hm in editor_stack.editors.iter_mut() {
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
                        CommandToken::Keyword(kwrd) if matches!(kwrd, CommandKeyword::Ins | CommandKeyword::Ovr) => { // :ins | ovr [] []
                            let mut hm = &mut editor_stack.editors[editor_stack.current];
                            let input = match &tokens[2] {
                                CommandToken::Integer(word, _) => word,
                                CommandToken::Word(word) => word,
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            };

                            let fmt = match &tokens[1] {
                                CommandToken::Keyword(CommandKeyword::U8) => BinaryFormat::UInt(UIntFormat::U8),
                                CommandToken::Keyword(CommandKeyword::U16) => BinaryFormat::UInt(UIntFormat::U16),
                                CommandToken::Keyword(CommandKeyword::U32) => BinaryFormat::UInt(UIntFormat::U32),
                                CommandToken::Keyword(CommandKeyword::U64) => BinaryFormat::UInt(UIntFormat::U64),
                                CommandToken::Keyword(CommandKeyword::I8) => BinaryFormat::IInt(IIntFormat::I8),
                                CommandToken::Keyword(CommandKeyword::I16) => BinaryFormat::IInt(IIntFormat::I16),
                                CommandToken::Keyword(CommandKeyword::I32) => BinaryFormat::IInt(IIntFormat::I32),
                                CommandToken::Keyword(CommandKeyword::I64) => BinaryFormat::IInt(IIntFormat::I64),
                                CommandToken::Keyword(CommandKeyword::F32) => BinaryFormat::Float(FloatFormat::F32),
                                CommandToken::Keyword(CommandKeyword::F64) => BinaryFormat::Float(FloatFormat::F64),
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            };

                            match to_bytes(input, DataType {fmt, end: editor_stack.endianness}){
                                Ok(bytes) => {
                                    match kwrd {
                                        CommandKeyword::Ins => (CommandInstruction::NoOp, hm.hex_edit.insert_bytes(&bytes)),
                                        CommandKeyword::Ovr => (CommandInstruction::NoOp, hm.hex_edit.overwrite_bytes(&bytes)),
                                        _ => (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                                    }
                                },
                                Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg))
                            }
                            

                        },
                        CommandToken::Keyword(CommandKeyword::RShift | CommandKeyword::LShift) => { // :rshft | lshft [] []
                            let mut hm = &mut editor_stack.editors[editor_stack.current];
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Register(register), CommandToken::Integer(_, n)) => { // TODO: Make this work for registers 32-63
                                    match &tokens[0] {
                                        CommandToken::Keyword(CommandKeyword::RShift) => {
                                            (CommandInstruction::NoOp, hm.hex_edit.shift_register(*register as u8, *n as i8))
                                        },
                                        CommandToken::Keyword(CommandKeyword::LShift) => {
                                            (CommandInstruction::NoOp, hm.hex_edit.shift_register(*register as u8, -(*n as i8)))
                                        },
                                        _ => (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                                    }
                                },
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            }
                        },
                        _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                    }
                },
                _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
            }
        },
        // FIND IN FILE
        '/' => {
            let mut hm = &mut editor_stack.editors[editor_stack.current];
            hm.hex_edit.clear_find();
            hm.hex_edit.find(&command[1..].to_vec());
            (CommandInstruction::NoOp, hm.hex_edit.seek_next())
        },
        '?' => {
            let mut hm = &mut editor_stack.editors[editor_stack.current];
            hm.hex_edit.clear_find();
            hm.hex_edit.find(&command[1..].to_vec());
            (CommandInstruction::NoOp, hm.hex_edit.seek_prev())
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
            let mut hm = &mut editor_stack.editors[editor_stack.current];
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
                    hm.hex_edit.insert_register(0)
                },
                KeystrokeToken::Character('P') => {
                    hm.hex_edit.overwrite_register(0)
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
                    editor_stack.editors[editor_stack.current].hex_edit.insert_fill(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('F')) => {
                    editor_stack.editors[editor_stack.current].hex_edit.overwrite_fill(n)
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
                        editor_stack.editors[editor_stack.current].hex_edit.insert_register(n as u8)
                    } else if n < 64 {
                        let v = editor_stack.clipboard_registers[n - 32].to_vec();
                        editor_stack.editors[editor_stack.current].hex_edit.insert_bytes(&v)
                    } else {
                        ActionResult::error("Register indices must be less than 64".to_string())
                    }
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('P')) => {
                    if n < 32 {
                        editor_stack.editors[editor_stack.current].hex_edit.overwrite_register(n as u8)
                    } else if n < 64 {
                        let v = editor_stack.clipboard_registers[n - 32].to_vec();
                        editor_stack.editors[editor_stack.current].hex_edit.overwrite_bytes(&v)
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
            let mut hm = &mut editor_stack.editors[editor_stack.current].hex_edit;
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

fn alert(text: String, window: &mut Window, line_entry: &mut LineEntry, hex_edit: &mut HexEdit) {
    println!("ALERT: {}", text);
    line_entry.alert(text.chars().collect());
    line_entry.draw(window);
    hex_edit.refresh_cursor(window);
}

pub fn run(filename: String, file_manager_type: FileManagerType, extract: bool) -> std::io::Result<()> {
    let mut edit_state = EditState::Escaped;
    let mut window = initscr();
    window.refresh();
    window.keypad(true);
    noecho();

    let mut current_keystroke = Vec::<char>::new();

    let mut editors = EditorStack::new(0, 0, window.get_max_x() as usize - 1, window.get_max_y()as usize - 1);
    editors.push(filename, file_manager_type, extract);

    let mut macro_manager = MacroManager::new();

    let mut cursor_index_len = 0;

    {
        let mut hm = &mut editors.editors[editors.current];

        cursor_index_len = format!("{:x}", hm.hex_edit.len()).len();
        hm.hex_edit.set_viewport_row(0)?;
        hm.hex_edit.draw(&mut window);
    }

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
                    Some(Input::Character(':')) | Some(Input::Character('/')) => {
                        edit_state = EditState::Command;
                        line_entry.addch(ch_input.unwrap()); 
                        line_entry.draw(&mut window);
                    }, 
                    Some(Input::Character('t')) => {
                        editors.tab();
                        editors.editors[editors.current].hex_edit.update(&mut window, UpdateDescription::All); // TODO: Handle this result
                    }
                    Some(Input::Character(c)) => {
                        current_keystroke.push(c);
                        if matches!(c, 'g' | 'G' | 'f' | 'F' | 'd' | 'y' | 'p' | 'P' | 'u' | 'U' | 'n' | 'N' | 's' | 'm' | 'M') {
                            let result = execute_keystroke(&mut editors, &mut macro_manager, current_keystroke);
                            let mut hm = &mut editors.editors[editors.current];
                            if let Some(err) = result.error {
                                alert(err, &mut window, &mut line_entry, &mut hm.hex_edit);
                            }
                            if let Some(action) = result.action {
                                hm.action_stack.add(action);
                            }
                            let result = hm.hex_edit.update(&mut window, result.update);
                            if let Err(err) = result {
                                alert(err.to_string(), &mut window, &mut line_entry, &mut hm.hex_edit);
                            }
                            current_keystroke = Vec::<char>::new();
                        }
                    },
                    Some(ch) if matches!(ch, Input::KeyRight | Input::KeyLeft | Input::KeyUp | Input::KeyDown | Input::KeyNPage | Input::KeyPPage | Input::KeyHome | Input::KeyEnd) => {
                        let mut hm = &mut editors.editors[editors.current];
                        let result = hm.hex_edit.addch(ch);
                        if let Some(err) = result.error {
                            alert(err, &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                        if let Some(action) = result.action {
                            hm.action_stack.add(action);
                        }
                        let result = hm.hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                    },
                    _ => {
                        println!("Invalid keystroke");
                    }
                }
            },
            EditState::Command => {

                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    },
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                    },
                    Some(Input::Character('\n')) => {
                        //println!("Newline: {}", String::from_iter(line_entry.get_text()));
                        let (instr, result) = execute_command(&mut editors, line_entry.get_text());

                        let mut hm = &mut editors.editors[editors.current];

                        if let Some(err) = result.error {
                            alert(err, &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                        if let Some(action) = result.action {
                            hm.action_stack.add(action);
                        }
                        let result = hm.hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), &mut window, &mut line_entry, &mut hm.hex_edit);
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
                                manual_view.draw(&mut window);
                            },
                            CommandInstruction::Open(filename) => {
                                if let Ok(i) = editors.push(filename, FileManagerType::ReadOnly, extract) {
                                    editors.current = i;
                                    editors.editors[editors.current].hex_edit.update(&mut window, UpdateDescription::All);
                                }
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
                let mut hm = &mut editors.editors[editors.current];
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    }
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                    },
                    Some(ch) => { 
                        let result = hm.hex_edit.addch(ch);
                        if let Some(err) = result.error {
                            alert(err, &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                        if let Some(action) = result.action {
                            hm.action_stack.add(action);
                        }
                        let result = hm.hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), &mut window, &mut line_entry, &mut hm.hex_edit);
                        }
                    },
                    None => ()
                }
            },
            EditState::Manual => {
                let mut hm = &mut editors.editors[editors.current];
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    },
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                        hm.hex_edit.draw(&mut window);
                        line_entry.draw(&mut window);
                    },
                    Some(ch) if matches!(ch, Input::KeyUp | Input::KeyDown) => {
                        manual_view.addch(ch);
                        manual_view.draw(&mut window);
                    },
                    _ => println!("Invalid Keystroke"),
                }
            }
        }

        let (y, x) = window.get_cur_yx();

        // Draw cursor index
        let mut cursor_index_string: String = format!("{:0width$x}", editors.editors[editors.current].hex_edit.get_cursor_pos(), width=cursor_index_len);
        if editors.editors[editors.current].hex_edit.get_capitalize_hex() {
            cursor_index_string = cursor_index_string.to_ascii_uppercase();
        }
        window.mvaddstr((window.get_max_y() as usize - 1) as i32,
        (window.get_max_x() as usize - 1 - cursor_index_len) as i32, 
        cursor_index_string);

        window.mv(y, x);

    }

}
