use std::rc::Rc;
use std::io::SeekFrom;

extern crate pancurses;
use pancurses::{initscr, endwin, Input, noecho, Window, resize_term};

mod line_entry;
use crate::line_entry::LineEntry;

mod large_text_view;
use crate::large_text_view::LargeTextView;

mod hex_edit;
use crate::hex_edit::{FileManager, Action, CompoundAction, ActionStack, EditMode, ActionResult, ShowType, FillType, HexEdit};
pub use crate::hex_edit::FileManagerType;

mod parsers;
use crate::parsers::{CommandToken, CommandKeyword, parse_command, parse_filltype, KeystrokeToken, parse_keystroke};

mod bin_format;
use crate::bin_format::{Endianness, UIntFormat, IIntFormat, FloatFormat, BinaryFormat, DataType, to_bytes};

enum EditState {
    Escaped,
    Command,
    Edit,
    Manual
}

const MANUAL_TEXT: &str = "\\b\\cCOMMANDS
\\b  SETTINGS:
    :set caps [on|off]  =>  Toggle the case of display hexadecimal values
    :set hex [on|off]   =>  Toggle the hex display
    :set ascii [on|off] =>  Toggle the ascii display
    :set lnum [hex|dec|off]  =>  [TODO] Toggle line number display/base
    :set cnum [hex|dec|off]  =>  [TODO] Toggle cursor index display/base
    :set line #         =>  Set the number of bytes per line to #
    :set fill r#        =>  Set the fill value to the contents of register #
    :set fill x#        =>  Set the fill value to # (in hex)
    :set icase [on|off] =>  Set ignore case for find function
    :set regex [on|off] =>  Enable/disable regular expressions for find function
    :set undo #         =>  Set the maximum length of the undo/redo stack to #
    :set endian [be|le] =>  [TODO] Set the endianness that will be used in the 'ins' and 'ovr' commands
    :set chunk #        =>  Set the chunk size used to insert bytes in swap and live mode

\\b  INSERTION/OVERWRITE:
    :ins fmt #          =>  [TODO] Insert # encoded as fmt at the cusor location
    :ovr fmt #          =>  [TODO] Overwrite # encoded as fmt at the cusor location

\\b  REGISTER OPERATIONS:
    :cat r# r##         =>  Concatenate contents of listed register ## into register #
    :cat r# x##         =>  Concatenate ## (in hex) into register #
    :swap r#            =>  Swap the byte order of register #
    :rshft r# ##        =>  Bitshift the contens of register # right ## bits
    :lshft r# ##        =>  Bitshift the contens of register # left ## bits
    :and r# r##         =>  [TODO] Perform bitwise AND on register # with register ##
    :or r# r##          =>  [TODO] Perform bitwise OR on register # with register ##
    :not r#             =>  [TODO] Perform bitwise NOT on regiser #

\\b  READ/WRITE:
    :w                  =>  Write all changes to file
    :x                  =>  Write all changes to file and exit
    :wq                 =>  Write all changes to file and exit
    :q                  =>  Exit if there are no unsaved changes
    :q!                 =>  Exit
    :open file          =>  [TODO] Opens 'file' in another tab

\\b  FIND:
    /expr               =>  Find ASCII 'expr' in file
    ?expr               =>  Backwards find ASCII 'expr' in file
    \\expr              =>  Find hex 'expr' in file
    |expr               =>  Backwards find hex 'expr' in file

\\b  MISC:
    :clear undo         =>  Clear the contents of the undo/redo stack
    :show hist          =>  [TODO] Show command history
    :show m#            =>  [TODO] Show details about macro number #
    :man                =>  Show application manual (this text)


\\b\\cKEYSTROKE COMMANDS

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

    u       =>  Undo last action
    U       =>  Redo last action
    #u      =>  [TODO] Undo last # actions
    #U      =>  [TODO] Redo last # actions
    #M      =>  Start recording #th macro
    M       =>  Stop recording macro
    #m      =>  Run #th macro";

const BUGS: &str = "Macro executions can't be undone
Inputting numbers greater than usize maximum in commands/keystrokes causes panic
Setting line length to value greater than width of terminal causes panic
";

enum CommandInstruction {
    NoOp,
    Exit,
    ChangeState(EditState)
}

fn execute_command(hex_edit: &mut HexEdit, action_stack: &mut ActionStack, command: Vec<char>) -> (CommandInstruction, ActionResult) {

    match command[0] {
        ':' => {
            let tokens = parse_command(&command[1..].to_vec());
            match tokens.len() {
                1 => {
                    match &tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Save) => {
                            let result = hex_edit.save();
                            if let Some(err) = result.error {
                                (CommandInstruction::NoOp, ActionResult::error(err.to_string()))
                            } else {
                                (CommandInstruction::NoOp, ActionResult::empty())
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::SaveAndQuit) => {
                            let result = hex_edit.save();
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
                            if hex_edit.is_modified() {
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
                        (CommandToken::Keyword(CommandKeyword::Swap), CommandToken::Word(word)) => {
                            println!("Setting fill type");
                            match parse_filltype(word.to_vec()) { // TODO: Don't use this function, use something more direct
                                Ok(fill) => match fill {
                                    FillType::Register(n) => (CommandInstruction::NoOp, hex_edit.swap_register(n)),
                                    _ => (CommandInstruction::NoOp, ActionResult::error("Could not parse register number".to_string()))
                                },
                                Err(s) => (CommandInstruction::NoOp, ActionResult::error(s))
                            }
                        },
                        (CommandToken::Keyword(CommandKeyword::Clear), CommandToken::Keyword(CommandKeyword::Undo)) => {
                            action_stack.clear();
                            (CommandInstruction::NoOp, ActionResult::empty())
                        },
                        _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                    }
                }
                3 => {
                    match tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Set) => { // :set [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Keyword(CommandKeyword::Caps), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, hex_edit.set_capitalize_hex(matches!(kwrd, CommandKeyword::On)))
                                },
                                (CommandToken::Keyword(CommandKeyword::Hex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, hex_edit.set_show_hex(matches!(kwrd, CommandKeyword::On)))
                                },
                                (CommandToken::Keyword(CommandKeyword::Ascii), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, hex_edit.set_show_ascii(matches!(kwrd, CommandKeyword::On)))
                                },
                                (CommandToken::Keyword(CommandKeyword::Icase), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, hex_edit.set_ignore_case(matches!(kwrd, CommandKeyword::On)))
                                },
                                (CommandToken::Keyword(CommandKeyword::Regex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                    (CommandInstruction::NoOp, hex_edit.set_use_regex(matches!(kwrd, CommandKeyword::On)))
                                },
                                (CommandToken::Keyword(CommandKeyword::LNum), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::Off | CommandKeyword::Hex | CommandKeyword::Dec) => {
                                    (CommandInstruction::NoOp, hex_edit.set_show_lnum(match kwrd {
                                        CommandKeyword::Off => ShowType::Off,
                                        CommandKeyword::Dec => ShowType::Dec,
                                        _ => ShowType::Hex,
                                    }))
                                },
                                (CommandToken::Keyword(CommandKeyword::Line), CommandToken::Integer(_, n)) => {
                                    (CommandInstruction::NoOp, hex_edit.set_line_length(*n as u8))
                                },
                                (CommandToken::Keyword(CommandKeyword::Chunk), CommandToken::Integer(_, n)) => {
                                    (CommandInstruction::NoOp, hex_edit.set_block_size(*n))
                                },
                                (CommandToken::Keyword(CommandKeyword::Undo), CommandToken::Integer(_, n)) => {
                                    action_stack.set_length(*n);
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                },
                                (CommandToken::Keyword(CommandKeyword::Fill), CommandToken::Word(word)) => {
                                    match parse_filltype(word.to_vec()) {
                                        Ok(fill) => (CommandInstruction::NoOp, hex_edit.set_fill(fill)),
                                        Err(s) => (CommandInstruction::NoOp, ActionResult::error(s))
                                    }
                                },
                                _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::Cat) => { // :cat [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Word(word1), CommandToken::Word(word2)) => {
                                    match parse_filltype(word1.to_vec()) { // TODO: use a different function here
                                        Ok(FillType::Register(n)) => {
                                            match parse_filltype(word2.to_vec()) {
                                                Ok(fill) => (CommandInstruction::NoOp, hex_edit.concatenate_register(n, fill)),
                                                Err(s) => (CommandInstruction::NoOp, ActionResult::error(s))
                                            }
                                        },
                                        Ok(FillType::Bytes(_)) => {
                                            (CommandInstruction::NoOp, ActionResult::error("First 'cat' parameter must be register (r#)".to_string()))
                                        },
                                        Err(s) => (CommandInstruction::NoOp, ActionResult::error(s))
                                    }
                                },
                                _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            }
                        },
                        CommandToken::Keyword(CommandKeyword::Ins) => { // :ins [] []
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
                                _ => return (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
                            };

                            println!("U32: {:?}", to_bytes(input, DataType {fmt, end: Endianness::Big}));
                            (CommandInstruction::NoOp, ActionResult::empty())

                        },
                        CommandToken::Keyword(CommandKeyword::RShift | CommandKeyword::LShift) => { // :rshft | lshft [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Word(word), CommandToken::Integer(_, n)) => {
                                    match parse_filltype(word.to_vec()) { // TODO: use a different function here
                                        Ok(FillType::Register(register)) => {
                                            match &tokens[0] {
                                                CommandToken::Keyword(CommandKeyword::RShift) => {
                                                    (CommandInstruction::NoOp, hex_edit.shift_register(register, *n as i8))
                                                },
                                                CommandToken::Keyword(CommandKeyword::LShift) => {
                                                    (CommandInstruction::NoOp, hex_edit.shift_register(register, -(*n as i8)))
                                                },
                                                _ => (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                                            }
                                        },
                                        Ok(FillType::Bytes(_)) => {
                                            (CommandInstruction::NoOp, ActionResult::error("First 'rshft/lshft' parameter must be register (r#)".to_string()))
                                        },
                                        Err(s) => (CommandInstruction::NoOp, ActionResult::error(s))
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
            hex_edit.clear_find();
            hex_edit.find(&command[1..].to_vec());
            (CommandInstruction::NoOp, hex_edit.seek_next())
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

fn execute_keystroke(hex_edit: &mut HexEdit, action_stack: &mut ActionStack, macro_manager: &mut MacroManager, keystroke: Vec<char>) -> ActionResult {
    let tokens = parse_keystroke(&keystroke);
    match tokens.len() {

        1 => {
            match tokens[0] {
                KeystrokeToken::Character('g') => {
                    hex_edit.seek(SeekFrom::Start(0))
                },
                KeystrokeToken::Character('G') => {
                    hex_edit.seek(SeekFrom::End(0))
                },
                KeystrokeToken::Character('n') => {
                    hex_edit.seek_next()
                },
                KeystrokeToken::Character('N') => {
                    hex_edit.seek_prev()
                },
                KeystrokeToken::Character('u') => {
                    action_stack.undo(hex_edit)
                },
                KeystrokeToken::Character('U') => {
                    action_stack.redo(hex_edit)
                },
                KeystrokeToken::Character('p') => {
                    hex_edit.insert_register(0)
                },
                KeystrokeToken::Character('P') => {
                    hex_edit.overwrite_register(0)
                },
                KeystrokeToken::Character('M') => {
                    macro_manager.finish(&action_stack)
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
                    hex_edit.seek(SeekFrom::Start(n as u64))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('G')) => {
                    hex_edit.seek(SeekFrom::End(n as i64))
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('f')) => {
                    hex_edit.insert_fill(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('F')) => {
                    hex_edit.overwrite_fill(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('d')) => {
                    hex_edit.delete_bytes(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('s')) => {
                    hex_edit.swap_bytes(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('y')) => {
                    hex_edit.yank(0, n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('p')) => {
                    hex_edit.insert_register(n as u8)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('P')) => {
                    hex_edit.overwrite_register(n as u8)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('u')) => {
                    hex_edit.undo(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('U')) => {
                    hex_edit.redo(n)
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('m')) => {
                    let res = macro_manager.run(n as u8, hex_edit);
                    match macro_manager.get(n as u8) { 
                        // Need to manually add the action here for now since I can't return a Rc
                        Some(m) => action_stack.add(m),
                        None => ()
                    };
                    res
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('M')) => {
                    macro_manager.start(n as u8, &action_stack)
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    ActionResult::error(format!("Command not recognized: '{}'", s))
                }
            }
        },

        3 => {
            match (tokens[0], tokens[1], tokens[2]) {
                (KeystrokeToken::Character('+'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    hex_edit.seek(SeekFrom::Current(n as i64))
                },
                (KeystrokeToken::Character('-'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    hex_edit.seek(SeekFrom::Current(-(n as i64)))
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
                    hex_edit.yank(n1 as u8, n2)
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

    let fm = FileManager::new(filename, file_manager_type, extract)?;

    let mut hex_edit = HexEdit::new(fm, // File Manager
                                    0, 0, // x, y
                                    window.get_max_x() as usize - 1, window.get_max_y()as usize - 1, // width, height
                                    16, true, true, // Line Length, Show Hex, Show ASCII
                                    '.', "  ".to_string(), true); // Invalid ASCII, Separator, Capitalize Hex
    
    let mut action_stack = ActionStack::new(256);

    let mut macro_manager = MacroManager::new();

    let cursor_index_len = format!("{:x}", hex_edit.len()).len();

    let mut line_entry = LineEntry::new(window.get_max_x() as usize - 2 - cursor_index_len, 0, window.get_max_y() as usize - 1);

    let mut manual_view = LargeTextView::new(0, 0, window.get_max_x() as usize - 1, window.get_max_y() as usize - 1, MANUAL_TEXT.to_string());

    hex_edit.set_viewport_row(0)?;
    hex_edit.draw(&mut window);

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
                        println!("{}", window.get_max_y());
                        if let Err(err) = hex_edit.resize(window.get_max_x() as usize - 1, window.get_max_y()as usize - 1) {
                            alert(err.to_string(), &mut window, &mut line_entry, &mut hex_edit);
                        }
                        line_entry.reset_geometry(window.get_max_x() as usize - 2 - cursor_index_len, 0, window.get_max_y() as usize - 1);
                        hex_edit.draw(&mut window);
                        line_entry.draw(&mut window);
                    },
                    Some(Input::Character('\u{1b}')) => {
                        current_keystroke = Vec::<char>::new();
                    },
                    Some(Input::Character('o')) => {
                        edit_state = EditState::Edit;
                        hex_edit.set_edit_mode(EditMode::HexOverwrite);
                        hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('O')) => {
                        edit_state = EditState::Edit;
                        hex_edit.set_edit_mode(EditMode::AsciiOverwrite);
                        hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('i')) => {
                        edit_state = EditState::Edit;
                        hex_edit.set_edit_mode(EditMode::HexInsert);
                        hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('I')) => {
                        edit_state = EditState::Edit;
                        hex_edit.set_edit_mode(EditMode::AsciiInsert);
                        hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character(':')) | Some(Input::Character('/')) => {
                        edit_state = EditState::Command;
                        line_entry.addch(ch_input.unwrap()); 
                        line_entry.draw(&mut window);
                    }, 
                    Some(Input::Character(c)) => {
                        current_keystroke.push(c);
                        if matches!(c, 'g' | 'G' | 'f' | 'F' | 'd' | 'y' | 'p' | 'P' | 'u' | 'U' | 'n' | 'N' | 's' | 'm' | 'M') {
                            let result = execute_keystroke(&mut hex_edit, &mut action_stack, &mut macro_manager, current_keystroke);
                            if let Some(err) = result.error {
                                alert(err, &mut window, &mut line_entry, &mut hex_edit);
                            }
                            if let Some(action) = result.action {
                                action_stack.add(action);
                            }
                            let result = hex_edit.update(&mut window, result.update);
                            if let Err(err) = result {
                                alert(err.to_string(), &mut window, &mut line_entry, &mut hex_edit);
                            }
                            current_keystroke = Vec::<char>::new();
                        }
                    },
                    Some(ch) if matches!(ch, Input::KeyRight | Input::KeyLeft | Input::KeyUp | Input::KeyDown | Input::KeyNPage | Input::KeyPPage | Input::KeyHome | Input::KeyEnd) => {
                        let result = hex_edit.addch(ch);
                        if let Some(err) = result.error {
                            alert(err, &mut window, &mut line_entry, &mut hex_edit);
                        }
                        if let Some(action) = result.action {
                            action_stack.add(action);
                        }
                        let result = hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), &mut window, &mut line_entry, &mut hex_edit);
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
                        let (instr, result) = execute_command(&mut hex_edit, &mut action_stack, line_entry.get_text());
                        if let Some(err) = result.error {
                            alert(err, &mut window, &mut line_entry, &mut hex_edit);
                        }
                        if let Some(action) = result.action {
                            action_stack.add(action);
                        }
                        let result = hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), &mut window, &mut line_entry, &mut hex_edit);
                        }

                        line_entry.clear();
                        line_entry.draw(&mut window);

                        match instr {
                            CommandInstruction::NoOp => {
                                edit_state = EditState::Escaped;
                                hex_edit.refresh_cursor(&mut window);
                            },
                            CommandInstruction::Exit => {
                                endwin();
                                std::process::exit(0);
                            },
                            CommandInstruction::ChangeState(EditState::Manual) => {
                                edit_state = EditState::Manual;
                                manual_view.draw(&mut window);
                            },
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
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    }
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                    },
                    Some(ch) => { 
                        let result = hex_edit.addch(ch);
                        if let Some(err) = result.error {
                            alert(err, &mut window, &mut line_entry, &mut hex_edit);
                        }
                        if let Some(action) = result.action {
                            action_stack.add(action);
                        }
                        let result = hex_edit.update(&mut window, result.update);
                        if let Err(err) = result {
                            alert(err.to_string(), &mut window, &mut line_entry, &mut hex_edit);
                        }
                    },
                    None => ()
                }
            },
            EditState::Manual => {
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    },
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                        hex_edit.draw(&mut window);
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
        let mut cursor_index_string: String = format!("{:0width$x}", hex_edit.get_cursor_pos(), width=cursor_index_len);
        if hex_edit.get_capitalize_hex() {
            cursor_index_string = cursor_index_string.to_ascii_uppercase();
        }
        window.mvaddstr((window.get_max_y() as usize - 1) as i32,
        (window.get_max_x() as usize - 1 - cursor_index_len) as i32, 
        cursor_index_string);

        window.mv(y, x);

    }

}
