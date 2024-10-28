use std::rc::Rc;
use std::io::SeekFrom;
use std::io::Write;

use flate2::write::DeflateEncoder;
use flate2::write::DeflateDecoder;
use flate2::Compression;

extern crate pancurses;
use pancurses::{initscr, endwin, Input, noecho, Window, resize_term, start_color, init_color, init_pair, COLORS, COLOR_PAIRS};

mod expr;
use crate::expr::*;

mod line_entry;
use crate::line_entry::{AlertType, LineEntry};

mod large_text_view;
use crate::large_text_view::LargeTextView;

mod hex_edit;
use crate::hex_edit::{FileManager, Action, CompoundAction, ActionStack, UpdateDescription, 
        EditMode, ActionResult, ShowType, ByteOperation, FillType, shift_vector, 
        vector_op, HexEdit, DataSource, Structure};
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

impl<'a> HexEditManager<'a> {
    fn undo(&mut self) -> ActionResult {
        self.action_stack.undo(&mut self.hex_edit)
    }

    fn redo(&mut self) -> ActionResult {
        self.action_stack.redo(&mut self.hex_edit)
    }
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


    fn current<'b>(&'b self) -> &'b HexEditManager<'a> {
        &self.editors[self.current]
    }

    fn current_mut<'b>(&'b mut self) -> &'b mut HexEditManager<'a> {
        &mut self.editors[self.current]
    }

    fn get_current_register(&self, index: usize) -> Result<Vec<u8>, String> {
        if index < 32 {
            self.current().hex_edit.get_register(index as u8)
        } else if index < 64 {
            Ok(self.clipboard_registers[index - 32].clone())
        } else {
            Err("Clipboard register must be less than 64".to_string())
        }
    }
}

enum CommandInstruction {
    NoOp,
    Exit,
    ChangeState(EditState),
    Open(String),
    Refresh
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

struct App<'a> {
    width: usize,
    height: usize,
    edit_state: EditState,
    editors: EditorStack<'a>,
    macro_manager: MacroManager,
    current_keystroke: Vec<char>,
    cursor_index_len: usize,
    command_history: Vec<Vec<char>>,
    command_history_index: usize,
    line_entry: LineEntry,
    manual_view: LargeTextView
}

impl<'a> App<'a> {

    fn new(width: usize, height: usize) -> App<'a> {
        let editors = EditorStack::new(0, 0, width, height);

        let cursor_index_len = 0;

        let line_entry = LineEntry::new(0, height);

        let manual_view = LargeTextView::new(0, 0,  width, height, MANUAL_TEXT.to_string());

        App {
            width,
            height,
            edit_state: EditState::Escaped,
            editors,
            macro_manager: MacroManager::new(),
            current_keystroke: Vec::<char>::new(),
            cursor_index_len,
            command_history: Vec::<Vec<char>>::new(),
            command_history_index: 0,
            line_entry,
            manual_view
        }
    }

    fn init(&mut self, window: &mut Option<Window>, filename: String, file_manager_type: FileManagerType, extract: bool) -> std::io::Result<()> {

        self.editors.push(filename, file_manager_type, extract, None)?;

        {
            let hm = &mut self.editors.editors[self.editors.current];

            self.cursor_index_len = format!("{:x}", hm.hex_edit.len()).len();
            hm.hex_edit.set_viewport_row(0)?;
            if let Some(window) = window {
                hm.hex_edit.draw(window);
            }
        }

        self.line_entry.init(self.width - 1 - self.cursor_index_len);
        
        Ok(())
    }

    fn alert(&mut self, text: String, alert_type: AlertType, window: &mut Option<Window>) {
        self.line_entry.alert(text.chars().collect(), alert_type);
        if let Some(window) = window {
            self.line_entry.draw(window);
        }
        self.refresh_current_cursor(window);
    }

    fn refresh_current_cursor(&self, window: &mut Option<Window>)  {
        // self.editors.editors[self.editors.current].hex_edit.refresh_cursor(window);
        if let Some(window) = window {
            self.editors.current().hex_edit.refresh_cursor(window);
        }
    }

    fn handle_action_result(&mut self, window: &mut Option<Window>, result: ActionResult) {
        if let Some(err) = result.alert {
            self.alert(err, result.alert_type.clone(), window);
        }
        if let Some(action) = result.action {
            self.editors.current_mut().action_stack.add(action);
        }
        let mut update_result = Ok(());
        if let Some(window) = window {
            update_result = self.editors.current_mut().hex_edit.update(window, result.update);
        }
        if let Err(err) = update_result {
            self.alert(err.to_string(), AlertType::Error, window);
        }
        
    }

    fn transition_to_edit(&mut self, window: &mut Option<Window>, mode: EditMode) {
        self.edit_state = EditState::Edit;
        self.editors.set_edit_mode(mode);
        self.refresh_current_cursor(window);
    }

    fn current_editor(&self) -> &'a HexEdit {
        &self.editors.editors[self.editors.current].hex_edit
    }

    fn current_editor_mut(&mut self) -> &'a mut HexEdit {
        &mut self.editors.editors[self.editors.current].hex_edit
    }

    fn current_cursor_pos(&self) -> usize {
        self.editors.current().hex_edit.get_cursor_pos()
    }

    fn execute_command(&mut self, command: Vec<char>) -> (CommandInstruction, ActionResult) {

        match command[0] {
            ':' => {
                let tokens = parse_command(&command[1..].to_vec());
                match tokens.len() {
                    1 => {
                        // let hm = &mut editor_stack.editors[editor_stack.current];
                        match &tokens[0] {
                            CommandToken::Keyword(CommandKeyword::Save) => {
                                let result = self.editors.current_mut().hex_edit.save();
                                if let Some(err) = result.alert {
                                    (CommandInstruction::NoOp, ActionResult::error(err.to_string()))
                                } else {
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                }
                            },
                            CommandToken::Keyword(CommandKeyword::SaveAndQuit) => {
                                let result = self.editors.current_mut().hex_edit.save();
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
                                if self.editors.current().hex_edit.is_modified() {
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
                                    (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.swap_register(*n as u8))
                                } else if *n < 64 {
                                    self.editors.clipboard_registers[*n - 32].reverse();
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                } else {
                                    (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                }
                                
                            },
                            (CommandToken::Keyword(CommandKeyword::Not), CommandToken::Register(n)) => {
                                if *n < 32 {
                                    (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.invert_register(*n as u8))
                                } else if *n < 64 {
                                    let new = self.editors.clipboard_registers[*n - 32].iter().map(|x| !x).collect();
                                    self.editors.clipboard_registers[*n - 32] = new;
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                } else {
                                    (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                }
                                
                            },
                            (CommandToken::Keyword(CommandKeyword::Deflate), CommandToken::Register(n)) => {
                                if *n < 32 {
                                    let clevel = self.editors.clevel;
                                    (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.deflate_register(*n as u8, clevel))
                                } else if *n < 64 {
                                    let mut e = DeflateEncoder::new(Vec::new(), Compression::new(self.editors.clevel as u32));
                                    if let Err(msg) = e.write_all(&self.editors.clipboard_registers[*n - 32]) {
                                        (CommandInstruction::NoOp, ActionResult::error(msg.to_string()))
                                    } else {
                                        match e.finish() {
                                            Ok(w) => {
                                                self.editors.clipboard_registers[*n - 32] = w;
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
                                    (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.inflate_register(*n as u8)) // TODO: Make level variable
                                } else if *n < 64 {
                                    let mut e = DeflateDecoder::new(Vec::new());
                                    if let Err(msg) = e.write_all(&self.editors.clipboard_registers[*n - 32]) {
                                        (CommandInstruction::NoOp, ActionResult::error(msg.to_string()))
                                    } else {
                                        match e.finish() {
                                            Ok(w) => {
                                                self.editors.clipboard_registers[*n - 32] = w;
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
                                    (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.set_register(*n as u8, &Vec::<u8>::new()))
                                } else if *n < 64 {
                                    self.editors.clipboard_registers[*n - 32] = Vec::<u8>::new();
                                    (CommandInstruction::NoOp, ActionResult::empty())
                                } else {
                                    (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                }
                            },
                            (CommandToken::Keyword(CommandKeyword::Clear), CommandToken::Keyword(CommandKeyword::Undo)) => {
                                self.editors.current_mut().action_stack.clear();
                                (CommandInstruction::NoOp, ActionResult::empty())
                            },
                            (CommandToken::Keyword(CommandKeyword::Open), CommandToken::Word(word)) => {
                                (CommandInstruction::Open(word.iter().collect()), ActionResult::empty())
                            },
                            (CommandToken::Keyword(kwrd), _) if matches!(kwrd, CommandKeyword::Print | CommandKeyword::PrintSeek) => { // :p []
                                // let hm = &mut editor_stack.editors[editor_stack.current];
    
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

                                let cursor_pos = self.editors.current().hex_edit.get_cursor_pos();
    
                                match self.editors.current_mut().hex_edit.get_bytes(cursor_pos, &mut buffer) {
                                    Ok(n) if n == n_bytes => {
                                        match from_bytes(&buffer, DataType {fmt, end: self.editors.endianness}) {
                                            Ok(s) => {
                                                let mut res = match kwrd {
                                                    CommandKeyword::Print => ActionResult::empty(),
                                                    CommandKeyword::PrintSeek => self.editors.current_mut().hex_edit.seek(SeekFrom::Current(n_bytes as i64)),
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
                                        self.editors.caps = matches!(kwrd, CommandKeyword::On);
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_capitalize_hex(matches!(kwrd, CommandKeyword::On))))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Hex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                        self.editors.show_hex = matches!(kwrd, CommandKeyword::On);
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_show_hex(matches!(kwrd, CommandKeyword::On))))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Ascii), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                        self.editors.show_ascii = matches!(kwrd, CommandKeyword::On);
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_show_ascii(matches!(kwrd, CommandKeyword::On))))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Filename), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                        self.editors.show_filename = matches!(kwrd, CommandKeyword::On);
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_show_filename(matches!(kwrd, CommandKeyword::On))))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Icase), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_ignore_case(matches!(kwrd, CommandKeyword::On))))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Regex), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::On | CommandKeyword::Off) => {
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_use_regex(matches!(kwrd, CommandKeyword::On))))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::LNum), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::Off | CommandKeyword::Hex | CommandKeyword::Dec) => {
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_show_lnum(match kwrd {
                                            CommandKeyword::Off => ShowType::Off,
                                            CommandKeyword::Dec => ShowType::Dec,
                                            _ => ShowType::Hex,
                                        })))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::CNum), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::Off | CommandKeyword::Hex | CommandKeyword::Dec) => {
                                        self.editors.cnum = match kwrd {
                                            CommandKeyword::Off => ShowType::Off,
                                            CommandKeyword::Dec => ShowType::Dec,
                                            _ => ShowType::Hex,
                                        };
                                        (CommandInstruction::NoOp, ActionResult::empty())
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Endian), CommandToken::Keyword(kwrd)) if matches!(kwrd, CommandKeyword::BigEndian | CommandKeyword::LittleEndian | CommandKeyword::NetworkEndian) => {
                                        self.editors.endianness = match kwrd {
                                            CommandKeyword::BigEndian => Endianness::Big,
                                            CommandKeyword::LittleEndian => Endianness::Little,
                                            CommandKeyword::NetworkEndian => Endianness::Network,
                                            _ => return (CommandInstruction::NoOp, ActionResult::error("Impossible state".to_string()))
                                        };
                                        (CommandInstruction::NoOp, ActionResult::empty())
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Line), CommandToken::Integer(_, n)) => {
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_line_length(*n as u8)))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Chunk), CommandToken::Integer(_, n)) => {
                                        (CommandInstruction::NoOp, self.editors.apply(|h| h.set_block_size(*n)))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::CLevel), CommandToken::Integer(_, n)) => { // TODO: Need this to work for saving gz files as well
                                        if *n >= 10 {
                                            (CommandInstruction::NoOp, ActionResult::error("Compression level must be 0-9".to_string()))
                                        } else {
                                            self.editors.clevel = *n as u8;
                                            (CommandInstruction::NoOp, ActionResult::empty())
                                        }
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Undo), CommandToken::Integer(_, n)) => {
                                        for hm in self.editors.editors.iter_mut() {
                                            hm.action_stack.set_length(*n);
                                        }
                                        (CommandInstruction::NoOp, ActionResult::empty())
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Syntax), CommandToken::Word(word)) => {
                                        let syntax: String = word.into_iter().collect();
                                        (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.set_syntax(Some(syntax)))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Syntax), CommandToken::Keyword(CommandKeyword::Off)) => {
                                        (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.set_syntax(None))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Fill), CommandToken::Register(n)) => {
                                        (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.set_fill(FillType::Register(*n as u8)))
                                    },
                                    (CommandToken::Keyword(CommandKeyword::Fill), CommandToken::Word(word)) => {
                                        match parse_bytes(word) {
                                            Ok(v) => (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.set_fill(FillType::Bytes(v))),
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
                                                (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.concatenate_register(*n1 as u8, FillType::Register(*n2 as u8)))
                                            } else if *n2 < 64 { 
                                                let fill = FillType::Bytes(self.editors.clipboard_registers[*n2 - 32].to_vec());
                                                (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.concatenate_register(*n1 as u8, fill))
                                            } else {
                                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                            }
                                        } else if *n1 < 64 {
                                            if *n2 < 32 {
                                                let fill = self.editors.current().hex_edit.get_register(*n2 as u8).unwrap().to_vec();
                                                self.editors.clipboard_registers[*n1 - 32].extend(fill);
                                                (CommandInstruction::NoOp, ActionResult::empty())
                                            } else if *n2 < 64 { 
                                                let fill = self.editors.clipboard_registers[*n2 - 32].to_vec();
                                                self.editors.clipboard_registers[*n1 - 32].extend(fill);
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
                                                    (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.concatenate_register(*n as u8, FillType::Bytes(v)))
                                                } else if *n < 64 {
                                                    self.editors.clipboard_registers[*n - 32].extend(v);
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
                                                (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.manipulate_register(*n1 as u8, FillType::Register(*n2 as u8), op))
                                            } else if *n2 < 64 { 
                                                let fill = FillType::Bytes(self.editors.clipboard_registers[*n2 - 32].to_vec());
                                                (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.manipulate_register(*n1 as u8, fill, op))
                                            } else {
                                                (CommandInstruction::NoOp, ActionResult::error("Register indices must be less than 64".to_string()))
                                            }
                                        } else if *n1 < 64 {
                                            if *n2 < 32 {
                                                let fill = self.editors.current_mut().hex_edit.get_register(*n2 as u8).unwrap().to_vec();
                                                match op {
                                                    ByteOperation::And => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| a & b),
                                                    ByteOperation::Or => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| a | b),
                                                    ByteOperation::Nand => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| !(a & b)),
                                                    ByteOperation::Nor => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| !(a | b)),
                                                    ByteOperation::Xor => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| a ^ b),
                                                    ByteOperation::Xnor => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| !(a ^ b))
                                                };
                                                (CommandInstruction::NoOp, ActionResult::empty())
                                            } else if *n2 < 64 { 
                                                let fill = self.editors.clipboard_registers[*n2 - 32].to_vec();
                                                match op {
                                                    ByteOperation::And => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| a & b),
                                                    ByteOperation::Or => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| a | b),
                                                    ByteOperation::Nand => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| !(a & b)),
                                                    ByteOperation::Nor => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| !(a | b)),
                                                    ByteOperation::Xor => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| a ^ b),
                                                    ByteOperation::Xnor => vector_op(&mut self.editors.clipboard_registers[*n1 - 32], &fill, |a, b| !(a ^ b))
                                                };
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
                                                    (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.manipulate_register(*n as u8, FillType::Bytes(v), op))
                                                } else if *n < 64 {
                                                    match op {
                                                        ByteOperation::And => vector_op(&mut self.editors.clipboard_registers[*n - 32], &v, |a, b| a & b),
                                                        ByteOperation::Or => vector_op(&mut self.editors.clipboard_registers[*n - 32], &v, |a, b| a | b),
                                                        ByteOperation::Nand => vector_op(&mut self.editors.clipboard_registers[*n - 32], &v, |a, b| !(a & b)),
                                                        ByteOperation::Nor => vector_op(&mut self.editors.clipboard_registers[*n - 32], &v, |a, b| !(a | b)),
                                                        ByteOperation::Xor => vector_op(&mut self.editors.clipboard_registers[*n - 32], &v, |a, b| a ^ b),
                                                        ByteOperation::Xnor => vector_op(&mut self.editors.clipboard_registers[*n - 32], &v, |a, b| !(a ^ b))
                                                    };
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
                                // let hm = &mut editor_stack.editors[editor_stack.current];
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
    
    
                                match to_bytes(input, DataType {fmt, end: self.editors.endianness}){
                                    Ok(bytes) => {
                                        match kwrd {
                                            CommandKeyword::Ins => (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.insert(DataSource::Bytes(bytes.to_vec()))),
                                            CommandKeyword::Ovr => (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.overwrite(DataSource::Bytes(bytes.to_vec()))),
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
                                            (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.shift_register(*register as u8, shift))
                                        } else if *register < 64 {
                                            match shift_vector(&mut self.editors.clipboard_registers[*register - 32], shift) {
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
                                    (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.slice_register(*register as u8, *n1, *n2))
                                } else if *register < 64 {
                                    if *n2 > self.editors.clipboard_registers[*register - 32].len() {
                                        (CommandInstruction::NoOp, ActionResult::error("Slice index outside of register contents bounds".to_string()))
                                    } else if *n1 > *n2 {
                                        (CommandInstruction::NoOp, ActionResult::error("Slice start index greater than slice end index".to_string()))
                                    } else {
                                        let temp = &self.editors.clipboard_registers[*register - 32][*n1..*n2];
                                        self.editors.clipboard_registers[*register - 32] = temp.to_vec();
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
                // let hm = &mut editor_stack.editors[editor_stack.current];
                self.editors.current_mut().hex_edit.clear_find();
                self.editors.current_mut().hex_edit.find(&command[1..].to_vec());
                (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.seek_next())
            },
            '?' => {
                // let hm = &mut editor_stack.editors[editor_stack.current];
                self.editors.current_mut().hex_edit.clear_find();
                self.editors.current_mut().hex_edit.find(&command[1..].to_vec());
                (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.seek_prev())
            },
            '\\' => {
                let mut word = command[1..].to_vec();
                word.insert(0, 'x');
                match parse_bytes(&word) {
                    Ok(v) => {
                        // let hm = &mut editor_stack.editors[editor_stack.current];
                        self.editors.current_mut().hex_edit.clear_find();
                        self.editors.current_mut().hex_edit.find_bytes(&v);
                        (CommandInstruction::NoOp, self.editors.current_mut().hex_edit.seek_next())
                    },
                    Err(msg) => (CommandInstruction::NoOp, ActionResult::error(msg))
                }
            }
            _ => (CommandInstruction::NoOp, ActionResult::error("Command not recognized".to_string()))
        }
    
        //Ok(())
    }

    fn execute_keystroke(&mut self, keystroke: Vec<char>) -> ActionResult {

        let tokens = parse_keystroke(&keystroke);
        match tokens.len() {
    
            1 => {
                // let hm = &mut self.editors.editors[self.editors.current];
                match tokens[0] {
                    KeystrokeToken::Character('g') => {
                        self.editors.current_mut().hex_edit.seek(SeekFrom::Start(0))
                    },
                    KeystrokeToken::Character('G') => {
                        self.editors.current_mut().hex_edit.seek(SeekFrom::End(0))
                    },
                    KeystrokeToken::Character('n') => {
                        self.editors.current_mut().hex_edit.seek_next()
                    },
                    KeystrokeToken::Character('N') => {
                        self.editors.current_mut().hex_edit.seek_prev()
                    },
                    KeystrokeToken::Character('u') => {
                        self.editors.current_mut().undo()
                    },
                    KeystrokeToken::Character('U') => {
                        self.editors.current_mut().redo()
                    },
                    KeystrokeToken::Character('p') => {
                        self.editors.current_mut().hex_edit.insert(DataSource::Register(0))
                    },
                    KeystrokeToken::Character('P') => {
                        self.editors.current_mut().hex_edit.overwrite(DataSource::Register(0))
                    },
                    KeystrokeToken::Character('M') => {
                        self.macro_manager.finish(&self.editors.current().action_stack)
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
                        self.editors.current_mut().hex_edit.seek(SeekFrom::Start(n as u64))
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('G')) => {
                        self.editors.current_mut().hex_edit.seek(SeekFrom::End(n as i64))
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('f')) => {
                        self.editors.current_mut().hex_edit.insert(DataSource::Fill(n))
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('F')) => {
                        self.editors.current_mut().hex_edit.overwrite(DataSource::Fill(n))
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('d')) => {
                        self.editors.current_mut().hex_edit.delete_bytes(n)
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('s')) => {
                        self.editors.current_mut().hex_edit.swap_bytes(n)
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('y')) => {
                        self.editors.current_mut().hex_edit.yank(0, n)
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('p')) => {
                        if n < 32 {
                            self.editors.current_mut().hex_edit.insert(DataSource::Register(n as u8))
                        } else if n < 64 {
                            let v = self.editors.clipboard_registers[n - 32].to_vec();
                            self.editors.current_mut().hex_edit.insert(DataSource::Bytes(v))
                        } else {
                            ActionResult::error("Register indices must be less than 64".to_string())
                        }
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('P')) => {
                        if n < 32 {
                            self.editors.current_mut().hex_edit.overwrite(DataSource::Register(n as u8))
                        } else if n < 64 {
                            let v = self.editors.clipboard_registers[n - 32].to_vec();
                            self.editors.current_mut().hex_edit.overwrite(DataSource::Bytes(v))
                        } else {
                            ActionResult::error("Register indices must be less than 64".to_string())
                        }
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('m')) => {
                        self.macro_manager.run(n as u8, &mut self.editors.current_mut().hex_edit)
                    },
                    (KeystrokeToken::Integer(n), KeystrokeToken::Character('M')) => {
                        self.macro_manager.start(n as u8, &self.editors.current_mut().action_stack)
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
                        self.editors.current_mut().hex_edit.seek(SeekFrom::Current(n as i64))
                    },
                    (KeystrokeToken::Character('-'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                        self.editors.current_mut().hex_edit.seek(SeekFrom::Current(-(n as i64)))
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
                            self.editors.current_mut().hex_edit.yank(n1 as u8, n2)
                        } else if n1 < 64 {
                            let pos = self.editors.current().hex_edit.get_cursor_pos();
                            let mut v: Vec<u8> = vec![0;n2];
                            let res = self.editors.current_mut().hex_edit.get_bytes(pos, &mut v);
                            match res {
                                Ok(_) => { // TODO: Check if this is less han n2
                                    self.editors.clipboard_registers[n1 - 32] = v.to_vec();
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

    fn addch(&mut self, mut window: Option<Window>, ch_input: Option<Input>) -> std::io::Result<Option<Window>> {
        match self.edit_state {
            EditState::Escaped => {
                match ch_input {
                    Some(Input::KeyResize) => {
                        if let Some(window) = &mut window {
                            resize_term(0, 0);
                            self.editors.resize(window.get_max_x() as usize - 1, window.get_max_y()as usize - 1);
                            self.line_entry.reset_geometry(window.get_max_x() as usize - 2 - self.cursor_index_len, 0, window.get_max_y() as usize - 1);
                            self.line_entry.draw(window);
                            self.editors.current_mut().hex_edit.update(window, UpdateDescription::All); // TODO: Handle this result
                        } else {
                            panic!("Resize not supported in headless mode");
                        }
                        
                    },
                    Some(Input::Character('\u{1b}')) => {
                        self.current_keystroke = Vec::<char>::new();
                    },
                    Some(Input::Character('o')) => {
                        self.transition_to_edit(&mut window, EditMode::HexOverwrite);
                    },
                    Some(Input::Character('O')) => {
                        self.transition_to_edit(&mut window, EditMode::AsciiOverwrite);
                    },
                    Some(Input::Character('i')) => {
                        self.transition_to_edit(&mut window, EditMode::HexInsert);
                    },
                    Some(Input::Character('I')) => {
                        self.transition_to_edit(&mut window, EditMode::AsciiInsert);
                    },
                    Some(Input::Character(':')) | Some(Input::Character('/')) | Some(Input::Character('\\')) => {
                        self.edit_state = EditState::Command;
                        self.line_entry.addch(ch_input.unwrap()); 
                        if let Some(window) = &mut window {
                            self.line_entry.draw(window);
                        }
                    }, 
                    Some(Input::Character('t')) => {
                        self.editors.tab();
                        if let Some(window) = &mut window {
                            self.editors.current_mut().hex_edit.update(window, UpdateDescription::All); // TODO: Handle this result
                        }
                        let fname = self.editors.current().hex_edit.filename();
                        self.alert(fname, AlertType::Info, &mut window);
                    }
                    Some(Input::Character(c)) => {
                        self.current_keystroke.push(c);
                        if matches!(c, 'g' | 'G' | 'f' | 'F' | 'd' | 'y' | 'p' | 'P' | 'u' | 'U' | 'n' | 'N' | 's' | 'm' | 'M') {
                            let result = self.execute_keystroke(self.current_keystroke.clone());
                            self.handle_action_result(&mut window, result);
                            self.current_keystroke = Vec::<char>::new();
                        }
                    },
                    Some(ch) if matches!(ch, Input::KeyRight | Input::KeyLeft | Input::KeyUp | Input::KeyDown | Input::KeyNPage | Input::KeyPPage | Input::KeyHome | Input::KeyEnd) => {
                        let result = self.editors.current_mut().hex_edit.addch(ch);
                        self.handle_action_result(&mut window, result);
                    },
                    _ => {
                        self.alert("Invalid Keystroke".to_string(), AlertType::Error, &mut window)
                    }
                }
            },
            EditState::Command => {

                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    },
                    Some(Input::KeyUp) => {
                        if self.command_history_index > 0 {
                            self.command_history_index -= 1;
                            self.line_entry.set_text(self.command_history[self.command_history_index].to_vec());
                            if let Some(window) = &mut window {
                                self.line_entry.draw(window);
                            }
                            
                        }
                    },
                    Some(Input::KeyDown) => {
                        if self.command_history_index + 1 < self.command_history.len() {
                            self.command_history_index += 1;
                            self.line_entry.set_text(self.command_history[self.command_history_index].to_vec());
                            if let Some(window) = &mut window {
                                self.line_entry.draw(window);
                            }
                        }
                    },
                    Some(Input::Character('\u{1b}')) => {
                        self.edit_state = EditState::Escaped;
                    },
                    Some(Input::Character('\n')) => {
                        //println!("Newline: {}", String::from_iter(line_entry.get_text()));
                        self.command_history.push(self.line_entry.get_text());
                        self.command_history_index = self.command_history.len();

                        let (instr, result) = self.execute_command(self.line_entry.get_text());

                        self.handle_action_result(&mut window, result);

                        self.line_entry.clear();
                        if let Some(window) = &mut window {
                            self.line_entry.draw(window);
                        }

                        match instr {
                            CommandInstruction::NoOp => {
                                self.edit_state = EditState::Escaped;

                                self.refresh_current_cursor(&mut window)
                                // hm.hex_edit.refresh_cursor(&mut window);
                            },
                            CommandInstruction::Exit => {
                                endwin();
                                std::process::exit(0);
                            },
                            CommandInstruction::ChangeState(EditState::Manual) => {
                                if let Some(window) = &mut window {
                                    self.edit_state = EditState::Manual;
                                    self.manual_view.resize(window.get_max_x() as usize - 1, window.get_max_y() as usize - 1);
                                    self.manual_view.draw(window);
                                } else {
                                    panic!("Manual state not valid in headless mode")
                                }
                            },
                            CommandInstruction::Open(filename) => {
                                if let Ok(i) = self.editors.push(filename, FileManagerType::ReadOnly, false, None) {
                                    self.editors.current = i;
                                    self.editors.current_mut().hex_edit.set_viewport_row(0);
                                    if let Some(window) = &mut window {
                                        self.editors.current().hex_edit.draw(window);//.update(&mut window, UpdateDescription::All);
                                    }
                                    self.edit_state = EditState::Escaped;
                                }
                            },
                            CommandInstruction::Refresh => {
                                //resize_term(0, 0);
                                endwin();
                                let mut new_window = initscr();
                                new_window.refresh();
                                new_window.keypad(true);
                                noecho();
                                init_colors();
                                //window.refresh();
                                self.editors.resize(new_window.get_max_x() as usize - 1, new_window.get_max_y()as usize - 1);
                                //panic!("{} {}", window.get_max_x(), window.get_max_y());
                                self.line_entry.reset_geometry(new_window.get_max_x() as usize - 2 - self.cursor_index_len, 0, new_window.get_max_y() as usize - 1);
                                self.line_entry.draw(&mut new_window);
                                self.editors.current_mut().hex_edit.update(&mut new_window, UpdateDescription::All); // TODO: Handle this result
                                window = Some(new_window);
                            }
                            _ => () // This should never be hit
                        }
                    },
                    Some(ch) => { 
                        self.line_entry.addch(ch); 
                        if let Some(window) = &mut window {
                            self.line_entry.draw(window);
                        }
                    },
                    None => ()
                }
            },
            EditState::Edit => {
                // let hm = &mut self.editors.editors[self.editors.current];
                match ch_input {
                    Some(Input::KeyResize) => {
                        if let Some(window) = &window {
                            resize_term(0, 0);
                        } else {
                            panic!("Resize not valid in headless mode")
                        }
                    }
                    Some(Input::Character('\u{1b}')) => {
                        self.edit_state = EditState::Escaped;
                    },
                    Some(ch) => { 
                        let result = self.editors.current_mut().hex_edit.addch(ch);
                        self.handle_action_result(&mut window, result);
                    },
                    None => ()
                }
            },
            EditState::Manual => {
                // let hm = &mut self.editors.editors[self.editors.current];
                match ch_input {
                    Some(Input::KeyResize) => {
                        if let Some(window) = &mut window {
                            resize_term(0, 0);
                            self.manual_view.resize(window.get_max_x() as usize - 1, window.get_max_y() as usize - 1);
                            self.manual_view.draw(window);
                        } else {
                            panic!("Resize not valid in headless mode")
                        }
                    },
                    Some(Input::Character('\u{1b}')) => { // escape
                        self.edit_state = EditState::Escaped;
                        if let Some(window) = &mut window {
                            self.editors.current().hex_edit.draw(window);
                            self.line_entry.draw(window);
                        }
                    },
                    Some(ch) if matches!(ch, Input::KeyUp | Input::KeyDown | Input::KeyHome | Input::KeyEnd | Input::KeyPPage | Input::KeyNPage) => {
                        self.manual_view.addch(ch);
                        if let Some(window) = &mut window {
                            self.manual_view.draw(window);
                        }
                    },
                    _ => self.alert("Invalid Keystroke".to_string(), AlertType::Error, &mut window)
                }
            }
        }

        if let Some(window) = &window {
            let (y, x) = window.get_cur_yx();

            // Draw cursor index
            let pos = self.editors.current().hex_edit.get_cursor_pos();
            let length = self.editors.current().hex_edit.len();
            let caps_hex = self.editors.current().hex_edit.get_capitalize_hex();
            let cursor_label_len = match self.editors.cnum {
                ShowType::Off => 0,
                ShowType::Dec => format!("{}", length).len(),
                ShowType::Hex => format!("{:x}", length).len()
            };
            let cursor_index_string = match self.editors.cnum {
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

        Ok(window)
    }
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
    init_pair(globals::RELATED_FIELD_COLOR as i16, pancurses::COLOR_BLACK, pancurses::COLOR_MAGENTA);
}

pub fn run(filename: String, file_manager_type: FileManagerType, extract: bool) -> std::io::Result<()> {

    let mut window = initscr();
    window.refresh();
    window.keypad(true);
    noecho();

    init_colors();

    let mut app = App::new(window.get_max_x() as usize - 1, window.get_max_y() as usize - 1);

    let mut window = Some(window);
    
    app.init(&mut window, filename, file_manager_type, extract)?;

    loop {
        let ch_input;
        if let Some(window) = &mut window {
            ch_input = window.getch();
            if app.line_entry.alerting() {
                app.line_entry.unalert();
                app.line_entry.draw(window);
            }
        } else {
            unreachable!()
        }

        window = app.addch(window, ch_input)?;

    }

}

#[cfg(test)]
impl<'a> std::process::Termination for App<'a> {
    fn report(self) -> std::process::ExitCode {
        std::process::ExitCode::SUCCESS
    }
}

#[cfg(test)]
mod app_string_tests {
    use super::*;

    fn test_driver(app: &mut App, inputs: &str) {
        for c in inputs.chars() {
            let input = match c {
                '←' => Input::KeyLeft,
                '↑' => Input::KeyUp,
                '→' => Input::KeyRight,
                '↓' => Input::KeyDown,
                _ => Input::Character(c)
            };
            if app.line_entry.alerting() {
                app.line_entry.unalert();
            }
            let window = app.addch(None, Some(input)).unwrap();
            assert!(window.is_none());
        }
    }

    #[test]
    fn basic_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        test_driver(&mut app, "4r16y↓4p-16g32y");
        let r0_expected = vec![
            0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,
            0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x20, 
            0x08, 0x02, 0x00, 0x00, 0x00, 0xfc, 0x18, 0xed
        ];
        let r0 = app.editors.current().hex_edit.get_register(0).unwrap();
        assert_eq!(r0_expected, r0)
    }

    #[test]
    fn seek_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        let file_length = 1788;

        // Index from start
        test_driver(&mut app, "150g");
        assert_eq!(app.current_cursor_pos(), 150);
        assert_eq!(app.line_entry.get_alert(), None);
        test_driver(&mut app, "1500g");
        assert_eq!(app.current_cursor_pos(), 1500);
        assert_eq!(app.line_entry.get_alert(), None);

        // Negative index from current
        test_driver(&mut app, "-100g");
        assert_eq!(app.current_cursor_pos(), 1400);
        assert_eq!(app.line_entry.get_alert(), None);
        test_driver(&mut app, "-150g");
        assert_eq!(app.current_cursor_pos(), 1250);
        assert_eq!(app.line_entry.get_alert(), None);

        // Try to seek to nevative index
        test_driver(&mut app, "-1251g");
        assert_eq!(app.current_cursor_pos(), 1250);
        assert_eq!(app.line_entry.get_alert(), Some("Cannot seek to negative index".to_string()));

        // Positive index from current
        test_driver(&mut app, "+150g");
        assert_eq!(app.current_cursor_pos(), 1400);
        assert_eq!(app.line_entry.get_alert(), None);
        test_driver(&mut app, "+100g");
        assert_eq!(app.current_cursor_pos(), 1500);
        assert_eq!(app.line_entry.get_alert(), None);

        // Go to start
        test_driver(&mut app, "g");
        assert_eq!(app.current_cursor_pos(), 0);
        assert_eq!(app.line_entry.get_alert(), None);

        // Index from end
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length);
        assert_eq!(app.line_entry.get_alert(), None);
        test_driver(&mut app, "100G");
        assert_eq!(app.current_cursor_pos(), file_length - 100);
        assert_eq!(app.line_entry.get_alert(), None);
        test_driver(&mut app, "1000G");
        assert_eq!(app.current_cursor_pos(), file_length - 1000);
        assert_eq!(app.line_entry.get_alert(), None);

        // Try to seek to nevative index from end
        test_driver(&mut app, "10000G");
        assert_eq!(app.current_cursor_pos(), file_length - 1000);
        assert_eq!(app.line_entry.get_alert(), Some("Cannot seek to negative index".to_string()));

        // Exceed file length
        test_driver(&mut app, "+6000g");
        assert_eq!(app.current_cursor_pos(), file_length + 5000);
        assert_eq!(app.line_entry.get_alert(), None);
        // Using isize::MAX instead of usize::MAX since rust vectors can't actually get up to usize
        test_driver(&mut app, format!("{}g", std::isize::MAX as usize).as_str());
        assert_eq!(app.current_cursor_pos(), std::isize::MAX as usize);
        assert_eq!(app.line_entry.get_alert(), None);
    }

    fn verify_cursor_history(app: &mut App, history: &Vec<usize>) {
        for p in history.iter().rev().skip(1) {
            test_driver(app, "u");
            println!("{}", p);
            assert_eq!(app.current_cursor_pos(), *p);
            assert_eq!(app.line_entry.get_alert(), None);
        }
        test_driver(app, "u");
        assert_eq!(app.line_entry.get_alert(), Some("No actions in stack".to_string()));
        for p in history.iter().skip(1) {
            test_driver(app, "U");
            assert_eq!(app.current_cursor_pos(), *p);
            assert_eq!(app.line_entry.get_alert(), None);
        }
        test_driver(app, "U");
        assert_eq!(app.line_entry.get_alert(), Some("No actions in stack".to_string()));
    }

    #[test]
    fn seek_undo_redo_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        let file_length = 1788;

        let mut pos_history = vec![0];

        // Index from start
        test_driver(&mut app, "150g");
        assert_eq!(app.current_cursor_pos(), 150);
        pos_history.push(150);
        verify_cursor_history(&mut app, &pos_history);
        test_driver(&mut app, "1500g");
        assert_eq!(app.current_cursor_pos(), 1500);
        pos_history.push(1500);
        verify_cursor_history(&mut app, &pos_history);

        // Negative index from current
        test_driver(&mut app, "-100g");
        assert_eq!(app.current_cursor_pos(), 1400);
        pos_history.push(1400);
        verify_cursor_history(&mut app, &pos_history);
        test_driver(&mut app, "-150g");
        assert_eq!(app.current_cursor_pos(), 1250);
        pos_history.push(1250);
        verify_cursor_history(&mut app, &pos_history);

        // Try to seek to nevative index
        test_driver(&mut app, "-1251g");
        assert_eq!(app.current_cursor_pos(), 1250);
        assert_eq!(app.line_entry.get_alert(), Some("Cannot seek to negative index".to_string()));
        // Does not influence undo/redo stack
        verify_cursor_history(&mut app, &pos_history);

        // Positive index from current
        test_driver(&mut app, "+150g");
        assert_eq!(app.current_cursor_pos(), 1400);
        pos_history.push(1400);
        verify_cursor_history(&mut app, &pos_history);
        test_driver(&mut app, "+100g");
        assert_eq!(app.current_cursor_pos(), 1500);
        pos_history.push(1500);
        verify_cursor_history(&mut app, &pos_history);

        // Go to start
        test_driver(&mut app, "g");
        assert_eq!(app.current_cursor_pos(), 0);
        pos_history.push(0);
        verify_cursor_history(&mut app, &pos_history);

        // Index from end
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length);
        pos_history.push(file_length);
        verify_cursor_history(&mut app, &pos_history);
        test_driver(&mut app, "100G");
        assert_eq!(app.current_cursor_pos(), file_length - 100);
        pos_history.push(file_length - 100);
        verify_cursor_history(&mut app, &pos_history);
        test_driver(&mut app, "1000G");
        assert_eq!(app.current_cursor_pos(), file_length - 1000);
        pos_history.push(file_length - 1000);
        verify_cursor_history(&mut app, &pos_history);

        // Try to seek to nevative index from end
        test_driver(&mut app, "10000G");
        assert_eq!(app.current_cursor_pos(), file_length - 1000);
        assert_eq!(app.line_entry.get_alert(), Some("Cannot seek to negative index".to_string()));
        // Does not influence the undo/redo stack
        verify_cursor_history(&mut app, &pos_history);

        // Exceed file length
        test_driver(&mut app, "+6000g");
        assert_eq!(app.current_cursor_pos(), file_length + 5000);
        pos_history.push(file_length + 5000);
        verify_cursor_history(&mut app, &pos_history);
        // Using isize::MAX instead of usize::MAX since rust vectors can't actually get up to usize
        test_driver(&mut app, format!("{}g", std::isize::MAX as usize).as_str());
        assert_eq!(app.current_cursor_pos(), std::isize::MAX as usize);
        pos_history.push(std::isize::MAX as usize);
        verify_cursor_history(&mut app, &pos_history);
    }

    #[test]
    fn delete_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        let file_length = 1788;

        // Delete 16 bytes at index 16
        test_driver(&mut app, "16g16d"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 16);

        // Copy 8 bytes at byte 12 (first 4 bytes are left of deleted span, 
        // last 4 are right of deleted span) and confirm their contents
        test_driver(&mut app, "12g8yG");
        let r0_expected = vec![
            0x49, 0x48, 0x44, 0x52,
            0xa3, 0x00, 0x00, 0x03
        ];
        let r0 = app.editors.current().hex_edit.get_register(0).unwrap();
        assert_eq!(r0_expected, r0);
        assert_eq!(app.current_cursor_pos(), file_length - 16);

        // Delete 1200 bytes at index 0
        test_driver(&mut app, "g1200d"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 0);

        // Grab the first 8 bytes (left of deleted span) and confirm their contents
        test_driver(&mut app, "8yG");
        let r0_expected = vec![
            0xff, 0x0c, 0xbe, 0x99, 0x00, 0x29, 0x47, 0x4b
        ];
        let r0 = app.editors.current().hex_edit.get_register(0).unwrap();
        assert_eq!(r0_expected, r0);
        assert_eq!(app.current_cursor_pos(), file_length - 1216);

        // Attempt to delete past the end of the file
        test_driver(&mut app, "1500d");
        assert_eq!(app.line_entry.get_alert(), Some("Cannot delete past EOF".to_string()));

        // Attempt to delete starting from outside the file
        test_driver(&mut app, "1500g1d");
        assert_eq!(app.line_entry.get_alert(), Some("Cannot delete past EOF".to_string()));

        // Delete the remainder of the file
        test_driver(&mut app, format!("g{}d", file_length - 1200 - 16).as_str());
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm that the file is empty
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), 0);
    }

    #[test]
    fn delete_undo_redo_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        let file_length = 1788;

        // Delete 16 bytes at index 16 then undo
        test_driver(&mut app, "16g16du"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 16);

        let buff_expected = vec![
            0x49, 0x48, 0x44, 0x52,
            0x00, 0x00, 0x00, 0x20
        ];
        let mut buff = vec![0; 8];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(12, &mut buff).unwrap(), 8);
        assert_eq!(buff_expected, buff);

        // Redo and confirm
        test_driver(&mut app, "U");
        let buff_expected = vec![
            0x49, 0x48, 0x44, 0x52,
            0xa3, 0x00, 0x00, 0x03
        ];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(12, &mut buff).unwrap(), 8);
        assert_eq!(buff_expected, buff);
        assert_eq!(app.current_cursor_pos(), 16);

        // Delete every other pair of bytes from byte 16 to 32
        test_driver(&mut app, "16g2d+2g2d+2g2d+2g2d+2g"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 24);

        let buff_expected = vec![
            0x00, 0x03, 0x58, 0x49,
            0x4d, 0x00, 0x00, 0x00
        ];
        let mut buff = vec![0; 8];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(16, &mut buff).unwrap(), 8);
        assert_eq!(buff_expected, buff);

        // Undo deleting pairs of bytes
        test_driver(&mut app, "uuuuuuuu"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 16);

        let buff_expected = vec![
            0xa3, 0x00, 0x00, 0x03,
            0xd2, 0x65, 0x58, 0x49,
            0x66, 0x4d, 0x4d, 0x00,
            0x2a, 0x00, 0x00, 0x00
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(16, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Redo deleting pairs of bytes
        test_driver(&mut app, "UUUUUUUU"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 24);

        let buff_expected = vec![
            0x00, 0x03, 0x58, 0x49,
            0x4d, 0x00, 0x00, 0x00
        ];
        let mut buff = vec![0; 8];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(16, &mut buff).unwrap(), 8);
        assert_eq!(buff_expected, buff);

        // Delete 1200 bytes at index 0
        test_driver(&mut app, "g1200d"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 0);

        // Grab the first 8 bytes (left of deleted span) and confirm their contents
        test_driver(&mut app, "8yG");
        let r0_expected = vec![
            0xb6, 0xa0, 0x37, 0x26, 0xe1, 0xf1, 0xb1, 0x70
        ];
        let r0 = app.editors.current().hex_edit.get_register(0).unwrap();
        assert_eq!(r0_expected, r0);
        assert_eq!(app.current_cursor_pos(), file_length - 1216 - 8);

        // Attempt to delete past the end of the file
        test_driver(&mut app, "1500d");
        assert_eq!(app.line_entry.get_alert(), Some("Cannot delete past EOF".to_string()));

        // Attempt to delete starting from outside the file
        test_driver(&mut app, "1500g1d");
        assert_eq!(app.line_entry.get_alert(), Some("Cannot delete past EOF".to_string()));

        // Undo deleting 1200 bytes (including seeks done since then)
        test_driver(&mut app, "uuuu"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 0);

        // Confirm some bytes in the previously deleted region
        let buff_expected = vec![
            0x00, 0x03, 0x58, 0x49,
            0x4d, 0x00, 0x00, 0x00
        ];
        let mut buff = vec![0; 8];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(16, &mut buff).unwrap(), 8);
        assert_eq!(buff_expected, buff);

        // Redo 1200 byte deletion and grab the first 8 bytes (left of deleted span) and confirm their contents
        test_driver(&mut app, "U8yG");
        let r0_expected = vec![
            0xb6, 0xa0, 0x37, 0x26, 0xe1, 0xf1, 0xb1, 0x70
        ];
        let r0 = app.editors.current().hex_edit.get_register(0).unwrap();
        assert_eq!(r0_expected, r0);
        assert_eq!(app.current_cursor_pos(), file_length - 1216 - 8);

        // Delete the remainder of the file
        test_driver(&mut app, format!("g{}d", file_length - 1200 - 16 - 8).as_str());
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm that the file is empty
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), 0);

        // Undo deleting 1200 bytes and file (including seeks done since then)
        test_driver(&mut app, "uuuuuu"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 0);

        // Confirm some bytes in the previously deleted region
        let buff_expected = vec![
            0x00, 0x03, 0x58, 0x49,
            0x4d, 0x00, 0x00, 0x00
        ];
        let mut buff = vec![0; 8];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(16, &mut buff).unwrap(), 8);
        assert_eq!(buff_expected, buff);

        // Redo file deletion and confirm file is empty
        test_driver(&mut app, "UUUUUG"); 
        assert_eq!(app.current_cursor_pos(), 0);
    }

    #[test]
    fn yank_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        let file_length = 1788;

        // Yank 16 bytes starting at 1712
        test_driver(&mut app, "1712g16y"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1712);

        // Paste them at byte 1728 and confirm their contents
        test_driver(&mut app, "1728gp"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1728 + 16);

        let buff_expected = vec![
            0x74, 0x98, 0xb2, 0x8e, 
            0x72, 0x95, 0x1a, 0xfc, 
            0xb7, 0xb5, 0x00, 0xb8, 
            0x21, 0x2c, 0x64, 0x64
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1728, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Confirm file length increased by 16
        test_driver(&mut app, "G"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        // (overwrite) paste the copied 16 bytes at 1744 and confirm their values
        test_driver(&mut app, "1744gP"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1744 + 16);

        let buff_expected = vec![
            0x74, 0x98, 0xb2, 0x8e, 
            0x72, 0x95, 0x1a, 0xfc, 
            0xb7, 0xb5, 0x00, 0xb8, 
            0x21, 0x2c, 0x64, 0x64
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1744, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Confirm file length did not change
        test_driver(&mut app, "G"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        // Yank sequential pairs of values into each register 0-31
        for i in 0..32 {
            test_driver(&mut app, format!("{}g{}r2y", 1040 + i*2, i).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            assert_eq!(app.current_cursor_pos(), 1040 + i*2);
        }

        // Paste (overwrite) each pair in reverse order. Confirm file length is unchanged
        for i in 0..32 {
            test_driver(&mut app, format!("{}g{}P", 1040 + i*2, 31 - i).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            assert_eq!(app.current_cursor_pos(), 1040 + i*2 + 2);

            test_driver(&mut app, "G"); 
            assert_eq!(app.current_cursor_pos(), file_length + 16);
        }

        // Paste (insert) each pair at the same location, so they end up in reverse order.
        // Confirm that file length changes accordingly
        for i in 0..32 {
            test_driver(&mut app, format!("{}g{}p", 1104, i).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            assert_eq!(app.current_cursor_pos(), 1104 + 2);

            test_driver(&mut app, "G"); 
            assert_eq!(app.current_cursor_pos(), file_length + 16 + (i + 1) * 2);
        }

        // Verify data pasted by above two loops
        let buff_expected = vec![
            0xec, 0xee, 0xdf, 0xee, 0xbd, 0x99, 0x99, 0xdd, 
            0x64, 0x73, 0x5a, 0xc0, 0x72, 0xb9, 0xb9, 0xbb, 
            0xee, 0x66, 0xe1, 0xcc, 0xca, 0xe7, 0x91, 0x06, 
            0x43, 0x42, 0x80, 0xe3, 0x97, 0xe3, 0x2b, 0x1c, 
            0x50, 0xc0, 0xa5, 0xb4, 0x1e, 0x48, 0xd5, 0x07, 
            0x2a, 0x36, 0x4a, 0x90, 0x18, 0x4d, 0x13, 0x25, 
            0xd4, 0x37, 0xf8, 0x60, 0x44, 0xd3, 0xa3, 0xc6, 
            0x8d, 0x1a, 0x10, 0x13, 0xaf, 0xc7, 0xc0, 0xf9
        ];
        let mut buff = vec![0; 64];
        
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1040, &mut buff).unwrap(), 64);
        assert_eq!(buff_expected, buff);
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1104, &mut buff).unwrap(), 64);
        assert_eq!(buff_expected, buff);
    }

    #[test]
    fn yank_undo_redo_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        let file_length = 1788;

        // Yank 16 bytes starting at 1712 and undo
        test_driver(&mut app, "1712g16yu"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1712);

        // Try to paste them at byte 1728 and confirm nothing is pasted
        test_driver(&mut app, "1728gp"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1728);

        let buff_expected = vec![
            0x25, 0xe8, 0x2a, 0x96, 
            0x36, 0x00, 0x40, 0x99, 
            0xca, 0x19, 0x4d, 0x74, 
            0x79, 0x1e, 0xe5, 0x2f
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1728, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Try to paste (overwrite) them at byte 1728 and confirm nothing is pasted
        test_driver(&mut app, "1728gP"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1728);

        let buff_expected = vec![
            0x25, 0xe8, 0x2a, 0x96, 
            0x36, 0x00, 0x40, 0x99, 
            0xca, 0x19, 0x4d, 0x74, 
            0x79, 0x1e, 0xe5, 0x2f
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1728, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Yank 16 bytes starting at 1712 and undo then redo
        test_driver(&mut app, "1712g16yuU"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1712);

        // Paste them at byte 1728 and confirm their contents
        test_driver(&mut app, "1728gp"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1728 + 16);

        let buff_expected = vec![
            0x74, 0x98, 0xb2, 0x8e, 
            0x72, 0x95, 0x1a, 0xfc, 
            0xb7, 0xb5, 0x00, 0xb8, 
            0x21, 0x2c, 0x64, 0x64
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1728, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Confirm file length increased by 16
        test_driver(&mut app, "G"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        // Undo and confirm previous state is restored
        test_driver(&mut app, "uu"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1728);

        let buff_expected = vec![
            0x25, 0xe8, 0x2a, 0x96, 
            0x36, 0x00, 0x40, 0x99, 
            0xca, 0x19, 0x4d, 0x74, 
            0x79, 0x1e, 0xe5, 0x2f
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1728, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Redo and confirm
        test_driver(&mut app, "U"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1728 + 16);

        let buff_expected = vec![
            0x74, 0x98, 0xb2, 0x8e, 
            0x72, 0x95, 0x1a, 0xfc, 
            0xb7, 0xb5, 0x00, 0xb8, 
            0x21, 0x2c, 0x64, 0x64
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1728, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Confirm file length increased by 16
        test_driver(&mut app, "G"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        // (overwrite) paste the copied 16 bytes at 1744 and undo. Confirm previous state is restored
        test_driver(&mut app, "1744gPu"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1728 + 16);

        let buff_expected = vec![
            0x74, 0x98, 0xb2, 0x8e, 
            0x72, 0x95, 0x1a, 0xfc, 
            0xb7, 0xb5, 0x00, 0xb8, 
            0x21, 0x2c, 0x64, 0x64
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1728, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Redo and confirm pasted values
        test_driver(&mut app, "U"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), 1744 + 16);

        let buff_expected = vec![
            0x74, 0x98, 0xb2, 0x8e, 
            0x72, 0x95, 0x1a, 0xfc, 
            0xb7, 0xb5, 0x00, 0xb8, 
            0x21, 0x2c, 0x64, 0x64
        ];
        let mut buff = vec![0; 16];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1744, &mut buff).unwrap(), 16);
        assert_eq!(buff_expected, buff);

        // Confirm file length did not change
        test_driver(&mut app, "G"); 
        assert_eq!(app.line_entry.get_alert(), None);
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        // Yank sequential pairs of values into each register 0-31
        for i in 0..32 {
            test_driver(&mut app, format!("{}g{}r2y", 1040 + i*2, i).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            assert_eq!(app.current_cursor_pos(), 1040 + i*2);
        }

        // Yank sequential pairs of values into each register 0-31 starting from 0
        for i in 0..32 {
            test_driver(&mut app, format!("{}g{}r2y", i*2, i).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            assert_eq!(app.current_cursor_pos(), i*2);
        }

        // Undo previous set of yanks
        test_driver(&mut app, &"u".repeat(64));

        // Paste (overwrite) each pair in reverse order. Confirm file length is unchanged
        for i in 0..32 {
            test_driver(&mut app, format!("{}g{}P", 1040 + i*2, 31 - i).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            assert_eq!(app.current_cursor_pos(), 1040 + i*2 + 2);

            test_driver(&mut app, "G"); 
            assert_eq!(app.current_cursor_pos(), file_length + 16);
        }

        // Paste (insert) each pair at the same location, so they end up in reverse order.
        // Confirm that file length changes accordingly
        for i in 0..32 {
            test_driver(&mut app, format!("{}g{}p", 1104, i).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            assert_eq!(app.current_cursor_pos(), 1104 + 2);

            test_driver(&mut app, "G"); 
            assert_eq!(app.current_cursor_pos(), file_length + 16 + (i + 1) * 2);
        }

        // Undo previous pastes
        test_driver(&mut app, &"u".repeat(192));

        let buff_expected_1040 = vec![
            0xc0, 0xf9, 0xaf, 0xc7, 0x10, 0x13, 0x8d, 0x1a, 
            0xa3, 0xc6, 0x44, 0xd3, 0xf8, 0x60, 0xd4, 0x37, 
            0x13, 0x25, 0x18, 0x4d, 0x4a, 0x90, 0x2a, 0x36, 
            0xd5, 0x07, 0x1e, 0x48, 0xa5, 0xb4, 0x50, 0xc0, 
            0x2b, 0x1c, 0x97, 0xe3, 0x80, 0xe3, 0x43, 0x42, 
            0x91, 0x06, 0xca, 0xe7, 0xe1, 0xcc, 0xee, 0x66, 
            0xb9, 0xbb, 0x72, 0xb9, 0x5a, 0xc0, 0x64, 0x73, 
            0x99, 0xdd, 0xbd, 0x99, 0xdf, 0xee, 0xec, 0xee
        ];

        let buff_expected_1104 = vec![
            0xcc, 0x04, 0x16, 0x0b, 0xd8, 0x6a, 0x0b, 0xfc, 
            0x27, 0xc0, 0xc5, 0x05, 0x18, 0x06, 0x94, 0xcb, 
            0xa2, 0x0d, 0x87, 0x34, 0x68, 0x9a, 0xa2, 0xab, 
            0x69, 0x50, 0xa9, 0x40, 0xbb, 0x0d, 0xe3, 0x31, 
            0x58, 0x16, 0x4d, 0x4d, 0xa7, 0xd0, 0xe9, 0xd0, 
            0x20, 0x4e, 0x55, 0xab, 0xa4, 0xee, 0x03, 0x18, 
            0x8d, 0x48, 0xa1, 0xd5, 0x02, 0x5d, 0x77, 0x03, 
            0xf0, 0xdb, 0x6c, 0x12, 0x1e, 0x6d, 0xa1, 0x3c
        ];

        let mut buff = vec![0; 64];
        
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1040, &mut buff).unwrap(), 64);
        assert_eq!(buff_expected_1040, buff);
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1104, &mut buff).unwrap(), 64);
        assert_eq!(buff_expected_1104, buff);

        // Redo previous pastes
        test_driver(&mut app, &"U".repeat(192));

        // Verify data pasted by above two loops
        let buff_expected = vec![
            0xec, 0xee, 0xdf, 0xee, 0xbd, 0x99, 0x99, 0xdd, 
            0x64, 0x73, 0x5a, 0xc0, 0x72, 0xb9, 0xb9, 0xbb, 
            0xee, 0x66, 0xe1, 0xcc, 0xca, 0xe7, 0x91, 0x06, 
            0x43, 0x42, 0x80, 0xe3, 0x97, 0xe3, 0x2b, 0x1c, 
            0x50, 0xc0, 0xa5, 0xb4, 0x1e, 0x48, 0xd5, 0x07, 
            0x2a, 0x36, 0x4a, 0x90, 0x18, 0x4d, 0x13, 0x25, 
            0xd4, 0x37, 0xf8, 0x60, 0x44, 0xd3, 0xa3, 0xc6, 
            0x8d, 0x1a, 0x10, 0x13, 0xaf, 0xc7, 0xc0, 0xf9
        ];
        let mut buff = vec![0; 64];
        
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1040, &mut buff).unwrap(), 64);
        assert_eq!(buff_expected, buff);
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(1104, &mut buff).unwrap(), 64);
        assert_eq!(buff_expected, buff);
    }

    #[test]
    fn fill_bytes_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        let file_length = 1788;

        // Insert 8 default fill bytes at 0
        test_driver(&mut app, "8f");
        assert_eq!(app.current_cursor_pos(), 8);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is impacted
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length + 8);

        test_driver(&mut app, ":set fill xabcdef\n");
        assert_eq!(app.line_entry.get_alert(), None);

        // Insert 8 default fill bytes at 8
        test_driver(&mut app, "8g8f");
        assert_eq!(app.current_cursor_pos(), 16);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is impacted
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        // Overwrite 8 fill bytes at 24
        test_driver(&mut app, "24g8F");
        assert_eq!(app.current_cursor_pos(), 32);
        assert_eq!(app.line_entry.get_alert(), None);

        // Overwrite 2 fill bytes at 19
        test_driver(&mut app, "19g2F");
        assert_eq!(app.current_cursor_pos(), 21);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is not impacted
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        let buff_expected = vec![
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
            0xab, 0xcd, 0xef, 0xab, 0xcd, 0xef, 0xab, 0xcd,
            0x89, 0x50, 0x4e, 0xab, 0xcd, 0x0a, 0x1a, 0x0a, 
            0xab, 0xcd, 0xef, 0xab, 0xcd, 0xef, 0xab, 0xcd,
            0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x20
        ];

        let mut buff = vec![0; 40];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(0, &mut buff).unwrap(), 40);
        assert_eq!(buff_expected, buff);

        // Overwrite 8 bytes, 4 of which will be written past EOF
        test_driver(&mut app, format!("{}g8F", file_length + 12).as_str());
        assert_eq!(app.current_cursor_pos(), file_length + 20);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is impacted
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length + 20);

        let buff_expected = vec![
            0x49, 0x45, 0x4e, 0x44, 
            0xab, 0xcd, 0xef, 0xab, 
            0xcd, 0xef, 0xab, 0xcd
        ];

        let mut buff = vec![0; 12];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(file_length + 8, &mut buff).unwrap(), 12);
        assert_eq!(buff_expected, buff);

        // Insert ascii character 8 bytes past EOF
        test_driver(&mut app, format!("{}gI?", file_length + 27).as_str());
        assert_eq!(app.current_cursor_pos(), file_length + 28);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is impacted
        test_driver(&mut app, "\u{1b}G");
        assert_eq!(app.current_cursor_pos(), file_length + 28);

        let buff_expected = vec![
            0x49, 0x45, 0x4e, 0x44, 
            0xab, 0xcd, 0xef, 0xab, 
            0xcd, 0xef, 0xab, 0xcd,
            0xab, 0xcd, 0xef, 0xab,
            0xcd, 0xef, 0xab, 0x3f,
        ];

        let mut buff = vec![0; 20];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(file_length + 8, &mut buff).unwrap(), 20);
        assert_eq!(buff_expected, buff);
    }

    #[test]
    fn fill_register_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();
        let file_length = 1788;

        // Yank first three bytes into register 0 (0x89 0x50 0x4e)
        test_driver(&mut app, "3y");
        test_driver(&mut app, ":set fill r0\n");

        // Insert 8 fill bytes at 0
        test_driver(&mut app, "8f");
        assert_eq!(app.current_cursor_pos(), 8);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is impacted
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length + 8);

        // Yank 3 bytes after the original yank (0x47, 0x0d, 0x0a)
        test_driver(&mut app, "11g3y");

        // Insert 8 fill bytes at 8
        test_driver(&mut app, "8g8f");
        assert_eq!(app.current_cursor_pos(), 16);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is impacted
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        // Overwrite 8 fill bytes at 24
        test_driver(&mut app, "24g8F");
        assert_eq!(app.current_cursor_pos(), 32);
        assert_eq!(app.line_entry.get_alert(), None);

        // Set fill to use register 31 and yank bytes 16-19 (0x89, 0x50) into register 31
        test_driver(&mut app, ":set fill r31\n");
        test_driver(&mut app, "16g31r2y");

        // Overwrite 2 fill bytes at 19
        test_driver(&mut app, "19g2F");
        assert_eq!(app.current_cursor_pos(), 21);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is not impacted
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length + 16);

        let buff_expected = vec![
            0x89, 0x50, 0x4e, 0x89, 0x50, 0x4e, 0x89, 0x50, 
            0x47, 0x0d, 0x0a, 0x47, 0x0d, 0x0a, 0x47, 0x0d,
            0x89, 0x50, 0x4e, 0x89, 0x50, 0x0a, 0x1a, 0x0a, 
            0x47, 0x0d, 0x0a, 0x47, 0x0d, 0x0a, 0x47, 0x0d,
            0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x20
        ];

        let mut buff = vec![0; 40];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(0, &mut buff).unwrap(), 40);
        assert_eq!(buff_expected, buff);

        // Set the fill back to register 0
        test_driver(&mut app, ":set fill r0\n");

        // Overwrite 8 bytes, 4 of which will be written past EOF
        test_driver(&mut app, format!("{}g8F", file_length + 12).as_str());
        assert_eq!(app.current_cursor_pos(), file_length + 20);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is impacted
        test_driver(&mut app, "G");
        assert_eq!(app.current_cursor_pos(), file_length + 20);

        let buff_expected = vec![
            0x49, 0x45, 0x4e, 0x44, 
            0x47, 0x0d, 0x0a, 0x47, 
            0x0d, 0x0a, 0x47, 0x0d
        ];

        let mut buff = vec![0; 12];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(file_length + 8, &mut buff).unwrap(), 12);
        assert_eq!(buff_expected, buff);

        // Insert ascii character 8 bytes past EOF
        test_driver(&mut app, format!("{}gI?", file_length + 27).as_str());
        assert_eq!(app.current_cursor_pos(), file_length + 28);
        assert_eq!(app.line_entry.get_alert(), None);

        // Confirm file length is impacted
        test_driver(&mut app, "\u{1b}G");
        assert_eq!(app.current_cursor_pos(), file_length + 28);

        let buff_expected = vec![
            0x49, 0x45, 0x4e, 0x44, 
            0x47, 0x0d, 0x0a, 0x47, 
            0x0d, 0x0a, 0x47, 0x0d,
            0x47, 0x0d, 0x0a, 0x47, 
            0x0d, 0x0a, 0x47, 0x3f,
        ];

        let mut buff = vec![0; 20];
        assert_eq!(app.editors.current_mut().hex_edit.get_bytes(file_length + 8, &mut buff).unwrap(), 20);
        assert_eq!(buff_expected, buff);
    }

    fn register_unary_op_test<'a, F>(op_name: &str, op: F) -> App<'a> where F: Fn(&Vec<u8>) -> Vec<u8> {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();

        let bytes_8_16 = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        for n in 0..64 {
            // Yank the bytes 8-16 into register n
            // 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
            test_driver(&mut app, format!("8g{}r8y", n).as_str()); 
        }

        let mut current = bytes_8_16.clone();

        // Invert each register inversion 64 times and check its value before and after
        for i in 0..64 {
            let next = op(&current);
            for n in 0..64 {
                let mut rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(current, rn);
            
                test_driver(&mut app, format!(":{} r{}\n", op_name, n).as_str()); 
                assert_eq!(app.line_entry.get_alert(), None);
                rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(next, rn);
                
            }
            current = next;
        }

        app
    }

    #[test]
    fn register_not_test<'a>() -> App<'a> {
        register_unary_op_test("not", |a| a.iter().map(|b| !b).collect::<Vec<u8>>())
    }

    #[test]
    fn register_swap_test<'a>() -> App<'a> {
        register_unary_op_test("swap", |a| a.iter().rev().cloned().collect::<Vec<u8>>())
    }

    fn register_unary_op_undo_test<'a, F>(op_name: &str, op: F) -> App<'a> where F: Fn(&Vec<u8>) -> Vec<u8> {

        let bytes_8_16 = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        let mut results = vec![];
        let mut current = bytes_8_16.clone();
        for i in 0..8 {
            results.push(current.clone());
            current = op(&current);
        }
        results.push(current);

        results.reverse();

        let mut app = register_unary_op_test(op_name, op);

        for i in 0..8 {
            for n in (0..32).rev() {
                let mut rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(results[i], rn);

                test_driver(&mut app, "u"); 
                assert_eq!(app.line_entry.get_alert(), None);
                rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(results[i + 1], rn);
            }
        }

        // The default action stack is 256, so we should be at the bottom of it at this point
        test_driver(&mut app, "u"); 
        assert_eq!(app.line_entry.get_alert(), Some("No actions in stack".to_string()));

        app
    }

    #[test]
    fn register_not_undo_test<'a>() -> App<'a> {
        register_unary_op_undo_test("not", |a| a.iter().map(|b| !b).collect::<Vec<u8>>())
    }

    #[test]
    fn register_swap_undo_test<'a>() -> App<'a> {
        register_unary_op_undo_test("swap", |a| a.iter().rev().cloned().collect::<Vec<u8>>())
    }

    fn register_unary_op_redo_test<'a, F>(op_name: &str, op: F) -> App<'a> where F: Fn(&Vec<u8>) -> Vec<u8> {

        let bytes_8_16 = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        let mut results = vec![];
        let mut current = bytes_8_16.clone();
        for i in 0..8 {
            results.push(current.clone());
            current = op(&current);
        }
        results.push(current);

        let mut app = register_unary_op_undo_test(op_name, op);

        for i in 0..8 {
            for n in 0..32 {
                let mut rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(results[i], rn);

                test_driver(&mut app, "U"); 
                assert_eq!(app.line_entry.get_alert(), None);
                rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(results[i + 1], rn);
            }
        }

        // We should be at the top of the action stack at this point
        test_driver(&mut app, "U"); 
        assert_eq!(app.line_entry.get_alert(), Some("No actions in stack".to_string()));

        app
    }

    #[test]
    fn register_not_redo_test<'a>() -> App<'a> {
        register_unary_op_redo_test("not", |a| a.iter().map(|b| !b).collect::<Vec<u8>>())
    }

    #[test]
    fn register_swap_redo_test<'a>() -> App<'a> {
        register_unary_op_redo_test("swap", |a| a.iter().rev().cloned().collect::<Vec<u8>>())
    }

    #[test]
    fn register_slice_test<'a>() -> App<'a> {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();

        let bytes_8_16 = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        for n in 0..64 {
            // Yank the bytes 8-16 into register n
            // 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
            test_driver(&mut app, format!("8g{}r8y", n).as_str()); 
        }

        for n in 0..64 {
            let mut rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16, rn);

            test_driver(&mut app, format!(":slice r{} 0 8\n", n).as_str());
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16, rn);

            test_driver(&mut app, format!(":slice r{} 0 6\n", n).as_str());
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[0..6], rn);

            test_driver(&mut app, format!(":slice r{} 2 6\n", n).as_str());
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[2..6], rn);

            test_driver(&mut app, format!(":slice r{} 1 4\n", n).as_str());
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[3..6], rn);

            test_driver(&mut app, format!(":slice r{} 0 4\n", n).as_str());
            assert_eq!(app.line_entry.get_alert(), Some("Slice index outside of register contents bounds".to_string()));
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[3..6], rn);

            test_driver(&mut app, format!(":slice r{} 2 1\n", n).as_str());
            assert_eq!(app.line_entry.get_alert(), Some("Slice start index greater than slice end index".to_string()));
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[3..6], rn);

            test_driver(&mut app, format!(":slice r{} 1 2\n", n).as_str());
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[4..5], rn);

            test_driver(&mut app, format!(":slice r{} 1 1\n", n).as_str());
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[5..5], rn);
        }
        app
    }

    #[test]
    fn register_slice_undo_test<'a>() -> App<'a> {

        let bytes_8_16 = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        let mut app = register_slice_test();

        for n in (0..32).rev() {
            let mut rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[5..5], rn);

            test_driver(&mut app, "u");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[4..5], rn);

            test_driver(&mut app, "u");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[3..6], rn);

            test_driver(&mut app, "u");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[2..6], rn);

            test_driver(&mut app, "u");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[0..6], rn);

            test_driver(&mut app, "u");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16, rn);

            test_driver(&mut app, "u");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16, rn);
        }

        app
    }

    #[test]
    fn register_slice_redo_test<'a>() -> App<'a> {

        let bytes_8_16 = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        let mut app = register_slice_undo_test();

        for n in 0..32 {
            let mut rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16, rn);

            test_driver(&mut app, "U");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16, rn);

            test_driver(&mut app, "U");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[0..6], rn);

            test_driver(&mut app, "U");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[2..6], rn);

            test_driver(&mut app, "U");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[3..6], rn);

            test_driver(&mut app, "U");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[4..5], rn);

            test_driver(&mut app, "U");
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            assert_eq!(bytes_8_16[5..5], rn);
        }

        app
    }

    fn apply_elementwise<F>(lhs: &Vec<u8>, rhs: &Vec<u8>, op: &F) -> Vec<u8> where F: Fn((&u8, &u8)) -> u8 {
        lhs.iter().zip(rhs.iter().cycle()).map(op).collect()
    }

    fn register_op_test<'a, F>(op_name: &str, op: F) -> App<'a> where F: Fn(&Vec<u8>, &Vec<u8>) -> Vec<u8> {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();


        let bytes_0_8: Vec<u8> = vec![
            0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
        ];

        let bytes_8_16: Vec<u8> = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        let bytes_28_32: Vec<u8> = vec![
            0x00, 0xfc, 0x18, 0xed
        ];

        let count: Vec<u8> = vec![
            0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa
        ];

        let bytes_0_8_and_8_16: Vec<u8> = op(&bytes_0_8, &bytes_8_16);
        let bytes_8_16_and_0_8: Vec<u8> = op(&bytes_8_16, &bytes_0_8);
        let bytes_8_16_and_0_8_and_8_16: Vec<u8> = op(&bytes_8_16, &bytes_0_8_and_8_16);
        let bytes_8_16_and_0_8_and_8_16_and_28_32: Vec<u8> = op(&bytes_8_16_and_0_8_and_8_16, &bytes_28_32);
        let bytes_28_32_and_8_16_and_0_8_and_8_16_and_28_32: Vec<u8> = op(&bytes_28_32, &bytes_8_16_and_0_8_and_8_16_and_28_32);
        let bytes_8_16_and_0_8_and_28_32: Vec<u8> = op(&bytes_8_16_and_0_8, &bytes_28_32);
        let bytes_28_32_and_8_16_and_0_8: Vec<u8> = op(&bytes_28_32, &bytes_8_16_and_0_8);
        let bytes_0_8_and_aa0f: Vec<u8> = op(&bytes_0_8, &vec![0xaa, 0x0f]); 
        let bytes_8_16_and_aa0f: Vec<u8> = op(&bytes_8_16, &vec![0xaa, 0x0f]); 
        let bytes_0_8_and_aa0f_and_count: Vec<u8> = op(&bytes_0_8_and_aa0f, &count);
        let bytes_8_16_and_aa0f_and_count: Vec<u8> = op(&bytes_8_16_and_aa0f, &count);

        // Needed in order to fully undo all of the and operations later
        test_driver(&mut app, ":set undo 272\n");
        assert_eq!(app.line_entry.get_alert(), None);

        for n in 0..64 {
            if n % 2 == 0 {
                test_driver(&mut app, format!("0g{}r8y", n).as_str()); 
                let rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(bytes_0_8, rn);
            } else {
                test_driver(&mut app, format!("8g{}r8y", n).as_str()); 
                let rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(bytes_8_16, rn);
            }
        }

        // Performs "and" on each register with the register after it. The 63rd register gets anded with the
        // 0th register. Keep in mind the 0th register has aleady been mutated at this point, but it doesnt matter
        // because b & (a & b) == b & a
        for n in 0..64 {
            test_driver(&mut app, format!(":{} r{} r{}\n", op_name, n, (n + 1) % 64).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            let rn = app.editors.get_current_register(n).unwrap();
            if n == 63 {
                assert_eq!(bytes_8_16_and_0_8_and_8_16, rn);
            } else if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_8_16, rn);
            } else {
                assert_eq!(bytes_8_16_and_0_8, rn);
            }
        }

        // Reset every even register to bytes 28-32
        for n in 0..32 {
            test_driver(&mut app, format!("28g{}r4y", n * 2).as_str()); 
            let rn = app.editors.get_current_register(n * 2).unwrap();
            assert_eq!(bytes_28_32, rn);
        }

        // Performs "and" on each register with the register before it. The 0th register gets anded with the
        // 63rd register. Keep in mind the 63rd register gets used after mutation for r62, but it doesnt matter
        // because b & (a & b) == b & a
        for n in (0..64).rev() {
            test_driver(&mut app, format!(":{} r{} r{}\n", op_name, n, (n + 63) % 64).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            let rn = app.editors.get_current_register(n).unwrap();
            if n == 0 {
                assert_eq!(bytes_28_32_and_8_16_and_0_8_and_8_16_and_28_32, rn);
            } else if n == 63 {
                assert_eq!(bytes_8_16_and_0_8_and_8_16_and_28_32, rn);
            } else if n %2 == 0 {
                assert_eq!(bytes_28_32_and_8_16_and_0_8, rn);
            } else {
                assert_eq!(bytes_8_16_and_0_8_and_28_32, rn);
            }
            
        }

        // Reset all the registers to their original values
        for n in 0..64 {
            if n % 2 == 0 {
                test_driver(&mut app, format!("0g{}r8y", n).as_str()); 
                let rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(bytes_0_8, rn);
            } else {
                test_driver(&mut app, format!("8g{}r8y", n).as_str()); 
                let rn = app.editors.get_current_register(n).unwrap();
                assert_eq!(bytes_8_16, rn);
            }
        }

        // "and" each register with 0xaa 0x0f, confirm result, and then and with 0x11 0x22 0x33 0x44 0x55 0x66 0x77 0x88 0x99 0xaa
        for n in 0..64 {
            let mut rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8, rn);
            } else {
                assert_eq!(bytes_8_16, rn);
            }
            test_driver(&mut app, format!(":{} r{} xaa0f\n", op_name, n).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_aa0f, rn);
            } else {
                assert_eq!(bytes_8_16_and_aa0f, rn);
            }

            test_driver(&mut app, format!(":{} r{} x112233445566778899aa\n", op_name, n).as_str()); 
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_aa0f_and_count, rn);
            } else {
                assert_eq!(bytes_8_16_and_aa0f_and_count, rn);
            }
        }

        app
    }



    #[test]
    fn register_and_test<'a>() -> App<'a> {
        register_op_test("and", |a, b| apply_elementwise(a, b, &|(c, d)| c & d))
    }

    #[test]
    fn register_or_test<'a>() -> App<'a> {
        register_op_test("or", |a, b| apply_elementwise(a, b, &|(c, d)| c | d))
    }

    #[test]
    fn register_xor_test<'a>() -> App<'a> {
        register_op_test("xor", |a, b| apply_elementwise(a, b, &|(c, d)| c ^ d))
    }

    #[test]
    fn register_nor_test<'a>() -> App<'a> {
        register_op_test("nor", |a, b| apply_elementwise(a, b, &|(c, d)| !(c | d)))
    }

    #[test]
    fn register_nand_test<'a>() -> App<'a> {
        register_op_test("nand", |a, b| apply_elementwise(a, b, &|(c, d)| !(c & d)))
    }

    #[test]
    fn register_xnor_test<'a>() -> App<'a> {
        register_op_test("xnor", |a, b| apply_elementwise(a, b, &|(c, d)| !(c ^ d)))
    }

    #[test]
    fn register_cat_test<'a>() -> App<'a> {
        register_op_test("cat", |a, b| a.clone().into_iter().chain(b.clone().into_iter()).collect())
    }


    fn register_op_undo_test<'a, F>(op_name: &str, op: F) -> App<'a> where F: Fn(&Vec<u8>, &Vec<u8>) -> Vec<u8> {

        let bytes_0_8 = vec![
            0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
        ];

        let bytes_8_16 = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        let bytes_28_32 = vec![
            0x00, 0xfc, 0x18, 0xed
        ];

        let count = vec![
            0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa
        ];

        let bytes_0_8_and_8_16: Vec<u8> = op(&bytes_0_8, &bytes_8_16);
        let bytes_8_16_and_0_8: Vec<u8> = op(&bytes_8_16, &bytes_0_8);
        let bytes_8_16_and_0_8_and_8_16: Vec<u8> = op(&bytes_8_16, &bytes_0_8_and_8_16);
        let bytes_8_16_and_0_8_and_8_16_and_28_32: Vec<u8> = op(&bytes_8_16_and_0_8_and_8_16, &bytes_28_32);
        let bytes_28_32_and_8_16_and_0_8_and_8_16_and_28_32: Vec<u8> = op(&bytes_28_32, &bytes_8_16_and_0_8_and_8_16_and_28_32);
        let bytes_8_16_and_0_8_and_28_32: Vec<u8> = op(&bytes_8_16_and_0_8, &bytes_28_32);
        let bytes_28_32_and_8_16_and_0_8: Vec<u8> = op(&bytes_28_32, &bytes_8_16_and_0_8);
        let bytes_0_8_and_aa0f: Vec<u8> = op(&bytes_0_8, &vec![0xaa, 0x0f]); 
        let bytes_8_16_and_aa0f: Vec<u8> = op(&bytes_8_16, &vec![0xaa, 0x0f]); 
        let bytes_0_8_and_aa0f_and_count: Vec<u8> = op(&bytes_0_8_and_aa0f, &count);
        let bytes_8_16_and_aa0f_and_count: Vec<u8> = op(&bytes_8_16_and_aa0f, &count);

        let mut app = register_op_test(op_name, op);

        // undo "and" each register with 0x11 0x22 0x33 0x44 0x55 0x66 0x77 0x88 0x99 0xaa and then
        // with 0xaa 0x0f
        for n in (0..32).rev() {
            let mut rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_aa0f_and_count, rn);
            } else {
                assert_eq!(bytes_8_16_and_aa0f_and_count, rn);
            }

            test_driver(&mut app, "u"); 
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_aa0f, rn);
            } else {
                assert_eq!(bytes_8_16_and_aa0f, rn);
            }

            test_driver(&mut app, "u"); 
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8, rn);
            } else {
                assert_eq!(bytes_8_16, rn);
            }
        }

        // Undo seek for registers 32-63
        test_driver(&mut app, &"u".repeat(32)); 
        assert_eq!(app.line_entry.get_alert(), None);

        // Undo seek and yank for registers 0-31
        for n in (0..32).rev() {
            test_driver(&mut app, "uu"); 
            assert_eq!(app.line_entry.get_alert(), None);
            let rn = app.editors.get_current_register(n).unwrap();
            if n == 0 {
                assert_eq!(bytes_28_32_and_8_16_and_0_8_and_8_16_and_28_32, rn);
            } else if n == 63 {
                assert_eq!(bytes_8_16_and_0_8_and_8_16_and_28_32, rn);
            } else if n %2 == 0 {
                assert_eq!(bytes_28_32_and_8_16_and_0_8, rn);
            } else {
                assert_eq!(bytes_8_16_and_0_8_and_28_32, rn);
            }
            
        }

        // Undo "and" between each register and the one before it
        for n in 0..32 {
            test_driver(&mut app, "u"); 
            assert_eq!(app.line_entry.get_alert(), None);
            let rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_28_32, rn);
            } else {
                assert_eq!(bytes_8_16_and_0_8, rn);
            }
            
        }

        // Undo seek for even registers 32-63
        test_driver(&mut app, &"u".repeat(16)); 
        assert_eq!(app.line_entry.get_alert(), None);

        // Undo seek and yank for every even register
        for n in (0..32).rev() {
            if n %2 == 0 {
                test_driver(&mut app, "uu"); 
                assert_eq!(app.line_entry.get_alert(), None);
            }
            let rn = app.editors.get_current_register(n).unwrap();

            if n == 63 {
                assert_eq!(bytes_8_16_and_0_8_and_8_16, rn);
            } else if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_8_16, rn);
            } else {
                assert_eq!(bytes_8_16_and_0_8, rn);
            }
        }

        // Undo "and" of each register with the one after it
        for n in (0..32).rev() {
            test_driver(&mut app, "u"); 
            assert_eq!(app.line_entry.get_alert(), None);
            let rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8, rn);
            } else {
                assert_eq!(bytes_8_16, rn);
            }
        }

        // We should have hit the bottom of the stack at this point (272 operations)
        test_driver(&mut app, "u"); 
        assert_eq!(app.line_entry.get_alert(), Some("No actions in stack".to_string()));

        // Verify registers 32-63 were not impacted by these operations
        for n in 32..64 {
            let rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_aa0f_and_count, rn);
            } else {
                assert_eq!(bytes_8_16_and_aa0f_and_count, rn);
            }
        }

        app

    }

    #[test]
    fn register_and_undo_test<'a>() -> App<'a> {
        register_op_undo_test("and", |a, b| apply_elementwise(a, b, &|(c, d)| c & d))
    }

    #[test]
    fn register_or_undo_test<'a>() -> App<'a> {
        register_op_undo_test("or", |a, b| apply_elementwise(a, b, &|(c, d)| c | d))
    }

    #[test]
    fn register_xor_undo_test<'a>() -> App<'a> {
        register_op_undo_test("xor", |a, b| apply_elementwise(a, b, &|(c, d)| c ^ d))
    }

    #[test]
    fn register_nor_undo_test<'a>() -> App<'a> {
        register_op_undo_test("nor", |a, b| apply_elementwise(a, b, &|(c, d)| !(c | d)))
    }

    #[test]
    fn register_nand_undo_test<'a>() -> App<'a> {
        register_op_undo_test("nand", |a, b| apply_elementwise(a, b, &|(c, d)| !(c & d)))
    }

    #[test]
    fn register_xnor_undo_test<'a>() -> App<'a> {
        register_op_undo_test("xnor", |a, b| apply_elementwise(a, b, &|(c, d)| !(c ^ d)))
    }

    #[test]
    fn register_cat_undo_test<'a>() -> App<'a> {
        register_op_undo_test("cat", |a, b| a.clone().into_iter().chain(b.clone().into_iter()).collect())
    }

    fn register_op_redo_test<'a, F>(op_name: &str, op: F) -> App<'a> where F: Fn(&Vec<u8>, &Vec<u8>) -> Vec<u8> {
        let bytes_0_8 = vec![
            0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
        ];

        let bytes_8_16 = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];

        let bytes_28_32 = vec![
            0x00, 0xfc, 0x18, 0xed
        ];

        let count = vec![
            0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa
        ];

        let bytes_0_8_and_8_16: Vec<u8> = op(&bytes_0_8, &bytes_8_16);
        let bytes_8_16_and_0_8: Vec<u8> = op(&bytes_8_16, &bytes_0_8);
        let bytes_8_16_and_0_8_and_8_16: Vec<u8> = op(&bytes_8_16, &bytes_0_8_and_8_16);
        let bytes_8_16_and_0_8_and_8_16_and_28_32: Vec<u8> = op(&bytes_8_16_and_0_8_and_8_16, &bytes_28_32);
        let bytes_28_32_and_8_16_and_0_8_and_8_16_and_28_32: Vec<u8> = op(&bytes_28_32, &bytes_8_16_and_0_8_and_8_16_and_28_32);
        let bytes_8_16_and_0_8_and_28_32: Vec<u8> = op(&bytes_8_16_and_0_8, &bytes_28_32);
        let bytes_28_32_and_8_16_and_0_8: Vec<u8> = op(&bytes_28_32, &bytes_8_16_and_0_8);
        let bytes_0_8_and_aa0f: Vec<u8> = op(&bytes_0_8, &vec![0xaa, 0x0f]); 
        let bytes_8_16_and_aa0f: Vec<u8> = op(&bytes_8_16, &vec![0xaa, 0x0f]); 
        let bytes_0_8_and_aa0f_and_count: Vec<u8> = op(&bytes_0_8_and_aa0f, &count);
        let bytes_8_16_and_aa0f_and_count: Vec<u8> = op(&bytes_8_16_and_aa0f, &count);

        let mut app = register_op_undo_test(op_name, op);

        // Redo "and" of each register with the one after it
        for n in 0..32 {
            test_driver(&mut app, "U"); 
            assert_eq!(app.line_entry.get_alert(), None);
            let rn = app.editors.get_current_register(n).unwrap();
            if n == 63 {
                assert_eq!(bytes_8_16_and_0_8_and_8_16, rn);
            } else if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_8_16, rn);
            } else {
                assert_eq!(bytes_8_16_and_0_8, rn);
            }
        }

        // Redo seek and yank for every even register
        for n in 0..32 {
            if n %2 == 0 {
                test_driver(&mut app, "UU"); 
                assert_eq!(app.line_entry.get_alert(), None);
            }
            let rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_28_32, rn);
            } else {
                assert_eq!(bytes_8_16_and_0_8, rn);
            }
        }

        // Redo seek for even registers 32-63
        test_driver(&mut app, &"U".repeat(16)); 
        assert_eq!(app.line_entry.get_alert(), None);

        // Redo "and" between each register and the one before it
        for n in (0..32).rev() {
            test_driver(&mut app, "U"); 
            assert_eq!(app.line_entry.get_alert(), None);
            let rn = app.editors.get_current_register(n).unwrap();
            if n == 0 {
                assert_eq!(bytes_28_32_and_8_16_and_0_8_and_8_16_and_28_32, rn);
            } else if n == 63 {
                assert_eq!(bytes_8_16_and_0_8_and_8_16_and_28_32, rn);
            } else if n %2 == 0 {
                assert_eq!(bytes_28_32_and_8_16_and_0_8, rn);
            } else {
                assert_eq!(bytes_8_16_and_0_8_and_28_32, rn);
            }
            
        }

        // Redo seek and yank for registers 0-31
        for n in 0..32 {
            test_driver(&mut app, "UU"); 
            assert_eq!(app.line_entry.get_alert(), None);
            let rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8, rn);
            } else {
                assert_eq!(bytes_8_16, rn);
            }
            
        }

        // Redo seek for registers 32-63
        test_driver(&mut app, &"U".repeat(32)); 
        assert_eq!(app.line_entry.get_alert(), None);

        // Redo "and" each register with 0xaa 0x0f and then with 0x11 0x22 0x33 0x44 0x55 0x66 0x77 0x88 0x99 0xaa
        for n in 0..32 {

            test_driver(&mut app, "U"); 
            assert_eq!(app.line_entry.get_alert(), None);
            let mut rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_aa0f, rn);
            } else {
                assert_eq!(bytes_8_16_and_aa0f, rn);
            }

            test_driver(&mut app, "U"); 
            assert_eq!(app.line_entry.get_alert(), None);
            rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_aa0f_and_count, rn);
            } else {
                assert_eq!(bytes_8_16_and_aa0f_and_count, rn);
            }
        }

        // We should have hit the top of the stack at this point (272 operations)
        test_driver(&mut app, "U"); 
        assert_eq!(app.line_entry.get_alert(), Some("No actions in stack".to_string()));

        // Verify registers 32-63 were not impacted by these operations
        for n in 32..64 {
            let rn = app.editors.get_current_register(n).unwrap();
            if n % 2 == 0 {
                assert_eq!(bytes_0_8_and_aa0f_and_count, rn);
            } else {
                assert_eq!(bytes_8_16_and_aa0f_and_count, rn);
            }
        }

        app
    }

    #[test]
    fn register_and_redo_test<'a>() -> App<'a> {
        register_op_redo_test("and", |a, b| apply_elementwise(a, b, &|(c, d)| c & d))
    }

    #[test]
    fn register_or_redo_test<'a>() -> App<'a> {
        register_op_redo_test("or", |a, b| apply_elementwise(a, b, &|(c, d)| c | d))
    }

    #[test]
    fn register_xor_redo_test<'a>() -> App<'a> {
        register_op_redo_test("xor", |a, b| apply_elementwise(a, b, &|(c, d)| c ^ d))
    }

    #[test]
    fn register_nor_redo_test<'a>() -> App<'a> {
        register_op_redo_test("nor", |a, b| apply_elementwise(a, b, &|(c, d)| !(c | d)))
    }

    #[test]
    fn register_nand_redo_test<'a>() -> App<'a> {
        register_op_redo_test("nand", |a, b| apply_elementwise(a, b, &|(c, d)| !(c & d)))
    }

    #[test]
    fn register_xnor_redo_test<'a>() -> App<'a> {
        register_op_redo_test("xnor", |a, b| apply_elementwise(a, b, &|(c, d)| !(c ^ d)))
    }

    #[test]
    fn register_cat_redo_test<'a>() -> App<'a> {
        register_op_redo_test("cat", |a, b| a.clone().into_iter().chain(b.clone().into_iter()).collect())
    }

    #[test]
    fn register_ops_test() {
        let mut app = App::new(50, 50);
        let mut window = None;
        app.init(&mut window, r"tests\exif2c08.png".to_string(), FileManagerType::RamOnly, false).unwrap();

        // Yank the first 8 bytes into register 0
        // 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
        test_driver(&mut app, "8y"); 
        // Yank the next bytes into register 1
        // 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        test_driver(&mut app, "8g1r8y"); 

        let mut r0_expected = vec![
            0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
        ];
        let mut r1_expected = vec![
            0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
        ];
        let mut r0 = app.editors.current().hex_edit.get_register(0).unwrap();
        assert_eq!(r0_expected, r0);
        let mut r1 = app.editors.current().hex_edit.get_register(1).unwrap();
        assert_eq!(r1_expected, r1);

        test_driver(&mut app, ":not r1\n"); 
        r1_expected = vec![
            0xff, 0xff, 0xff, 0xf2, 0xb6, 0xb7, 0xbb, 0xad
        ];
        r1 = app.editors.current().hex_edit.get_register(1).unwrap();
        assert_eq!(r1_expected, r1);

        test_driver(&mut app, ":and r0 r1\n"); 
        r0_expected = vec![
            0x89, 0x50, 0x4e, 0x42, 0x04, 0x02, 0x1a, 0x08
        ];
        r0 = app.editors.current().hex_edit.get_register(0).unwrap();
        assert_eq!(r0_expected, r0);
        
    }
}