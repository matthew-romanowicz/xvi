mod file_manager;
pub use crate::hex_edit::file_manager::{FileManagerType, FileManager};

extern crate pancurses;
use pancurses::{Input, Window, Attributes, chtype};

fn clip_to_range(n: usize, min: usize, max: usize) -> usize {
    if n < min {
        min
    } else if n > max {
        max
    } else {
        n
    }
}

pub enum Nibble {
    Left,
    Right,
}

pub enum EditMode {
    HexOverwrite,
    AsciiOverwrite,
    HexInsert,
    AsciiInsert
}

pub struct Highlight {
    pub start: usize,
    pub span: usize
}

pub enum FillType {
    Bytes(Vec<u8>),
    Register(u8)
}

pub trait Action {
    fn undo(&mut self, hex_edit: &mut HexEdit) -> ActionResult;
    fn redo(&mut self, hex_edit: &mut HexEdit) -> ActionResult;
    fn size(&self) -> usize;
}

pub struct InsertAction {
    index: usize,
    n_bytes: usize,
    bytes: Vec<u8>
}

impl InsertAction {
    fn new(index: usize, n_bytes: usize) -> InsertAction {
        InsertAction {
            index,
            n_bytes,
            bytes: Vec::<u8>::new()
        }
    }
}

impl Action for InsertAction {
    fn undo(&mut self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.delete_bytes(self.index, self.n_bytes) 
    }

    fn redo(&mut self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.insert_bytes(self.index, &self.bytes)
    }

    fn size(&self) -> usize{
        2*std::mem::size_of::<usize>() + self.bytes.len()
    }
}

pub struct OverwriteAction {
    index: usize,
    bytes: Vec<u8>
}

impl OverwriteAction {
    fn new(index: usize, bytes: Vec<u8>) -> OverwriteAction {
        OverwriteAction {
            index,
            bytes
        }
    }
}

impl Action for OverwriteAction {
    fn undo(&mut self, hex_edit: &mut HexEdit) -> ActionResult {
        let mut buffer = vec![0; self.bytes.len()];
        match hex_edit.get_bytes(self.index, &mut buffer) {
            Ok(_) => {
                match hex_edit.overwrite_bytes(self.index, &self.bytes) {
                    Ok(_) => {
                        self.bytes = buffer;
                        ActionResult::no_error(UpdateDescription::All)
                    },
                    Err(msg) => ActionResult::error(msg.to_string())
                }
            },
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    fn redo(&mut self, hex_edit: &mut HexEdit) -> ActionResult {
        let mut buffer = vec![0; self.bytes.len()];
        match hex_edit.get_bytes(self.index, &mut buffer) {
            Ok(_) => {
                match hex_edit.overwrite_bytes(self.index, &self.bytes) {
                    Ok(_) => {
                        self.bytes = buffer;
                        ActionResult::no_error(UpdateDescription::All)
                    },
                    Err(msg) => ActionResult::error(msg.to_string())
                }
            },
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    fn size(&self) -> usize{
        std::mem::size_of::<usize>() + self.bytes.len()
    }
}

pub struct ActionStack {
    index: usize,
    actions: Vec<Box<dyn Action>>
}

impl ActionStack {
    fn new() -> ActionStack {
        ActionStack {
            index: 0,
            actions: Vec::new()
        }
    }

    fn add(&mut self, action: Box<dyn Action>){
        self.actions.truncate(self.index);
        self.actions.push(action);
    }

    fn undo(&mut self, hex_edit: &mut HexEdit) -> Result<(), ()> {
        if self.index > 1 {
            self.actions[self.index - 1].undo(hex_edit);
            self.index -= 1;
            Ok(())
        } else {
            Err(())
        }
    }

    fn redo(&mut self, hex_edit: &mut HexEdit) -> Result<(), ()> {
        if self.index < self.actions.len() {
            self.actions[self.index - 1].redo(hex_edit);
            self.index += 1;
            Ok(())
        } else {
            Err(())
        }
    }
}

pub enum UpdateDescription {
    NoUpdate,
    AttrsOnly,
    All,
    After(usize),
    Range(usize, usize)
}

pub struct ActionResult {
    pub error: Option<String>,
    pub update: UpdateDescription,
}

impl ActionResult {
    pub fn empty() -> ActionResult {
        ActionResult {
            error: None,
            update: UpdateDescription::NoUpdate
        }
    }

    pub fn error(s: String) -> ActionResult {
        ActionResult {
            error: Some(s),
            update: UpdateDescription::NoUpdate
        }
    }

    pub fn no_error(update: UpdateDescription) -> ActionResult {
        ActionResult {
            error: None,
            update
        }
    }

    pub fn set_error(&mut self, s: String) {
        self.error = Some(s);
    }
}

pub struct HexEdit<'a> {
    file_manager: FileManager<'a>,
    x: usize,
    y: usize,
    width: usize,
    height: usize,
    line_length: u8,
    show_hex: bool,
    show_ascii: bool,
    invalid_ascii_char: char,
    eof_ascii_char: char,
    eof_hex_char: char,
    separator: String,
    capitalize_hex: bool,
    ignore_case: bool,
    fill: FillType,
    cursor_pos: usize,
    nibble: Nibble,
    start_line: usize,
    edit_mode: EditMode,
    highlights: Vec::<Highlight>,
    display_data: Vec::<u8>,
    valid_bytes: usize,
    action_stack: ActionStack,
    clipboard_registers: [Vec::<u8>; 32],//Needs to be 32 or less for Default::default() to work
}

impl<'a> HexEdit<'a> {

    pub fn new(file_manager: FileManager, x: usize, y: usize, width: usize, height: usize, line_length: u8, show_hex: bool, show_ascii: bool, invalid_ascii_char: char, separator: String, capitalize_hex: bool) -> HexEdit {
        HexEdit {
            file_manager,
            x,
            y,
            width,
            height,
            line_length,
            show_hex,
            show_ascii,
            invalid_ascii_char,
            eof_ascii_char: '~',
            eof_hex_char: '~',
            separator,
            capitalize_hex,
            ignore_case: false,
            fill: FillType::Bytes(vec![0]),
            cursor_pos: 0,
            nibble: Nibble::Left,
            start_line: 0,
            edit_mode: EditMode::AsciiOverwrite,
            highlights: vec![],
            display_data: Vec::<u8>::new(),
            valid_bytes: 0,
            action_stack: ActionStack::new(),
            clipboard_registers: Default::default()
        }
    }

    pub fn is_modified(&self) -> bool {
        self.file_manager.is_modified()
    }

    pub fn save(&mut self) -> ActionResult {
        match self.file_manager.save() {
            Ok(()) => ActionResult::empty(),
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    pub fn set_capitalize_hex(&mut self, caps: bool) -> ActionResult {
        self.capitalize_hex = caps;
        ActionResult::no_error(UpdateDescription::All)
    }

    pub fn get_capitalize_hex(&mut self) -> bool {
        self.capitalize_hex
    }

    pub fn set_line_length(&mut self, length: u8) -> ActionResult {
        self.line_length = length;
        match self.set_viewport_row(0) {
            Ok(()) => ActionResult::no_error(UpdateDescription::All),
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    pub fn set_show_ascii(&mut self, show_ascii: bool) -> ActionResult {
        self.show_ascii = show_ascii;
        ActionResult::no_error(UpdateDescription::All)
    }

    pub fn set_show_hex(&mut self, show_hex: bool) -> ActionResult {
        self.show_hex = show_hex;
        ActionResult::no_error(UpdateDescription::All)
    }

    pub fn set_ignore_case(&mut self, ignore_case: bool) -> ActionResult {
        self.ignore_case = ignore_case;
        ActionResult::no_error(UpdateDescription::NoUpdate)
    }

    pub fn set_edit_mode(&mut self, edit_mode: EditMode) -> ActionResult {
        self.edit_mode = edit_mode;
        ActionResult::no_error(UpdateDescription::AttrsOnly)
    }

    pub fn set_fill(&mut self, fill: FillType) -> ActionResult {
        self.fill = fill;
        ActionResult::no_error(UpdateDescription::NoUpdate)
    }

    pub fn get_cursor_pos(&self) -> usize {
        self.cursor_pos
    }

    pub fn clear_find(&mut self) -> ActionResult {
        self.highlights = Vec::<Highlight>::new();
        ActionResult::no_error(UpdateDescription::AttrsOnly)
    }

    pub fn find(&mut self, expr: &Vec<char>) -> ActionResult {
        let expr: Vec<u8> = expr.iter().map(|c| *c as u8).collect();
        for res in self.file_manager.find(&expr, self.ignore_case) {
            self.highlights.push(Highlight {start: res.start, span: res.span})
        }
        ActionResult::no_error(UpdateDescription::AttrsOnly)
    }

    pub fn seek_next(&mut self) -> ActionResult {
        for h in self.highlights.iter() {
            if h.start > self.cursor_pos {
                return self.set_cursor_pos(h.start);
            }
        }

        if self.highlights.len() > 0{
            let mut res = self.set_cursor_pos(self.highlights[0].start);
            res.set_error("Wrapped around to SOF".to_string());
            res
        } else {
            ActionResult::error("No results".to_string())
        }
    }

    pub fn seek_prev(&mut self) -> ActionResult {
        for h in self.highlights.iter().rev() {
            if h.start < self.cursor_pos {
                return self.set_cursor_pos(h.start);
            }
        }

        if self.highlights.len() > 0{
            let mut res = self.set_cursor_pos(self.highlights[self.highlights.len() - 1].start);
            res.set_error("Wrapped around to EOF".to_string());
            res
        } else {
            ActionResult::error("No results".to_string())
        }
    }

    pub fn len(&self) -> usize {
        self.file_manager.len()
    }

    fn get_fill_bytes(&self, n_bytes: usize) -> Vec<u8> {
        match &self.fill {
            FillType::Bytes(buffer) => {
                buffer.iter().cycle().take(n_bytes).cloned().collect()
            },
            FillType::Register(n) => {
                self.clipboard_registers[*n as usize].iter().cycle().take(n_bytes).cloned().collect()
            }
        }
    }

    pub fn concatenate_register(&mut self, register: u8, fill: FillType) -> ActionResult {
        if register >= 32 {
            return ActionResult::error("Register number must be less than 32".to_string())
        }
        match fill {
            FillType::Bytes(bytes) => {
                self.clipboard_registers[register as usize].extend(bytes.to_vec());
            },
            FillType::Register(n) => {
                self.clipboard_registers[register as usize].extend(self.clipboard_registers[n as usize].to_vec());
            }
        }
        ActionResult::no_error(UpdateDescription::NoUpdate)
    }

    pub fn insert_fill(&mut self, index: usize, n_bytes: usize) -> ActionResult {
        let mut start_byte = index;
        if index > self.file_manager.len() {
            start_byte = self.file_manager.len();
            if let Err(msg) = self.append_fill(index - self.file_manager.len()) {
                return ActionResult::error(msg.to_string())
            }
        }
        match self.file_manager.insert_bytes(index, &self.get_fill_bytes(n_bytes)) {
            Ok(_) => ActionResult::no_error(UpdateDescription::After(start_byte)),
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    pub fn overwrite_fill(&mut self, index: usize, n_bytes: usize) -> ActionResult {
        let mut start_byte = index;
        if index + n_bytes > self.file_manager.len() {
            start_byte = self.file_manager.len();
            if let Err(msg) = self.append_fill(index + n_bytes - self.file_manager.len()) {
                return ActionResult::error(msg.to_string())
            }
        }
        match self.file_manager.overwrite_bytes(index, &self.get_fill_bytes(n_bytes)) {
            Ok(_) => ActionResult::no_error(UpdateDescription::Range(start_byte, index + n_bytes)),
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    pub fn insert_register(&mut self, register: u8, index: usize) -> ActionResult {
        if register < 32 {
            if self.clipboard_registers[register as usize].len() > 0 {
                let mut start_byte = index;
                if index > self.file_manager.len() {
                    start_byte = self.file_manager.len();
                    if let Err(msg) = self.append_fill(index - self.file_manager.len()) {
                        return ActionResult::error(msg.to_string())
                    }
                }
                match self.file_manager.insert_bytes(index, &self.clipboard_registers[register as usize]) {
                    Ok(_) => ActionResult::no_error(UpdateDescription::After(start_byte)),
                    Err(msg) => ActionResult::error(msg.to_string())
                }
            } else {
                ActionResult::error(format!("Register {} is empty", register))
            }
        } else {
            ActionResult::error("Registers must be less than 32".to_string())
        }

    }

    pub fn overwrite_register(&mut self, register: u8, index: usize) -> ActionResult {
        if register < 32 {
            if self.clipboard_registers[register as usize].len() > 0 {
                let n = &self.clipboard_registers[register as usize].len();
                let mut start_byte = index;
                if index + n > self.file_manager.len() {
                    start_byte = self.file_manager.len();
                    if let Err(msg) = self.append_fill(index + n - self.file_manager.len()) {
                        return ActionResult::error(msg.to_string())
                    }
                }
                match self.file_manager.overwrite_bytes(index, &self.clipboard_registers[register as usize]) {
                    Ok(_) => ActionResult::no_error(UpdateDescription::Range(start_byte, index + n)),
                    Err(msg) => ActionResult::error(msg.to_string())
                }
            } else {
                ActionResult::error(format!("Register {} is empty", register))
            }
        } else {
            ActionResult::error("Registers must be less than 32".to_string())
        }
    }

    pub fn yank(&mut self, register: u8, index: usize, n_bytes: usize) -> ActionResult {
        if register < 32 {
            if index + n_bytes <= self.file_manager.len() {
                let mut buffer = vec![0; n_bytes];
                match self.file_manager.get_bytes(index, &mut buffer) {
                    Ok(_) => {
                        self.clipboard_registers[register as usize] = buffer;
                        ActionResult::empty()
                    },
                    Err(msg) => {
                        ActionResult::error(msg.to_string())
                    }
                }
            } else {
                ActionResult::error("Cannot yank past EOF".to_string())
            }
        } else {
            ActionResult::error("Registers must be less than 32".to_string())
        }
    }

    pub fn delete_bytes(&mut self, index: usize, n_bytes: usize) -> ActionResult {
        if index + n_bytes <= self.file_manager.len() {
            match self.file_manager.delete_bytes(index, n_bytes) {
                Ok(_) => ActionResult::no_error(UpdateDescription::After(index)),
                Err(msg) => ActionResult::error(msg.to_string())
            }
        } else {
            ActionResult::error("Cannot delete past EOF".to_string())
        }
    }

    pub fn insert_bytes(&mut self, index: usize, bytes: &Vec<u8>) -> ActionResult{
        // TODO
        println!("Inserting {} bytes at {}!", bytes.len(), index);
        ActionResult::empty()
    }

    pub fn overwrite_bytes(&mut self, index: usize, bytes: &Vec<u8>) -> std::io::Result<()> {
        // TODO: Make this more efficient
        //let mut original_data = vec![0; bytes.len()];
        //self.file_manager.get_bytes(index, &mut original_data);
        self.file_manager.overwrite_bytes(index, bytes)
        //self.refresh_viewport();
    }

    pub fn swap_bytes(&mut self, index: usize, n_bytes: usize) -> ActionResult {
        if index + n_bytes <= self.file_manager.len() {
            let mut data = vec![0; n_bytes];
            match self.file_manager.get_bytes(index, &mut data) {
                Ok(_) => {
                    data.reverse();
                    match self.file_manager.overwrite_bytes(index, &data) {
                        Ok(_) => ActionResult::no_error(UpdateDescription::Range(index, index + n_bytes)),
                        Err(msg) => ActionResult::error(msg.to_string())
                    }
                },
                Err(msg) => {
                    ActionResult::error(msg.to_string())
                }
            }
        } else {
            ActionResult::error("Cannot swap bytes past EOF".to_string())
        } 
    }

    pub fn get_bytes(&mut self, index: usize, buffer: &mut Vec<u8>) -> std::io::Result<usize> {
        self.file_manager.get_bytes(index, buffer)
    }

    pub fn get_byte(&mut self, index: usize) -> Option<u8> {
        self.file_manager.get_byte(index)
    }

    pub fn undo(&mut self, n: usize) -> ActionResult {
        //self.action_stack.undo(self);
        println!("Undoing {} actions", n); //TODO
        ActionResult::empty()
    }

    pub fn redo(&mut self, n: usize) -> ActionResult {
        //self.action_stack.redo(self);
        println!("Redoing {} actions", n); //TODO
        ActionResult::empty()
    }

    pub fn resize(&mut self, width: usize, height: usize) -> std::io::Result<()> {
        self.width = width;
        self.height = height;
        self.refresh_viewport()
    }

    fn bytes_to_hex_line(buffer: &Vec<u8>, eof_char: char, n_valid: usize) -> String {
        buffer.iter().enumerate()
        .map(|(i, b)| 
            if i < n_valid {
                format!("{:02x}", b).to_string()
            } else {
                eof_char.to_string().repeat(2)
            })
        .collect::<Vec<String>>()
        .join(" ")
    }
    
    fn bytes_to_ascii_line(buffer: &Vec<u8>, invalid_char: char, eof_char: char, n_valid: usize) -> String {
        buffer.iter().enumerate()
        .map(|(i, b)| 
            if i < n_valid {
                format!("{}", match b {32..=126 => *b as char, _ => invalid_char}).to_string()
            } else {
                eof_char.to_string()
            })
        .collect::<Vec<String>>()
        .join("")
    }
    
    fn read_to_composite_line(&self, buffer: &Vec<u8>, n_valid: usize) -> String {
        let mut s: String = "".to_string();
        if self.show_hex {
            if self.capitalize_hex {
                s += &HexEdit::bytes_to_hex_line(&buffer, self.eof_hex_char, n_valid).to_uppercase();
            } else {
                s += &HexEdit::bytes_to_hex_line(&buffer, self.eof_hex_char, n_valid).to_lowercase();
            }
            if self.show_ascii {
                s += &self.separator;
            }
        } 

        if self.show_ascii {
            s += &HexEdit::bytes_to_ascii_line(&buffer, self.invalid_ascii_char, self.eof_ascii_char, n_valid)
        }

        s
    }

    pub fn set_viewport_row(&mut self, row: usize) -> std::io::Result<()> {
        self.start_line = row;
        self.refresh_viewport()
    }

    fn refresh_viewport(&mut self) -> std::io::Result<()> {
        // TODO: Make this actually update a given range of data
        self.display_data = vec![0; (self.line_length as usize) * (self.height)];
        match self.file_manager.get_bytes(self.start_line * (self.line_length as usize), &mut self.display_data) {
            Ok(n) => {
                self.valid_bytes = n;
                Ok(())
            },
            Err(msg) => Err(msg)
        }
    }

    fn get_display_line(&self, line: usize) -> Vec<u8>{
        //println!("{}, {}, {}", line*(self.line_length as usize), (line + 1)*(self.line_length as usize), self.display_data.len());
        self.display_data[line*(self.line_length as usize)..(line + 1)*(self.line_length as usize)].to_vec()
    }

    fn append_fill(&mut self, n_bytes: usize) -> std::io::Result<()> {
        self.file_manager.append_bytes(&self.get_fill_bytes(n_bytes))
    }

    fn overwrite_byte(&mut self, index: usize, c: u8) -> ActionResult {
        let mut start_byte = index;
        if index >= self.file_manager.len() {
            start_byte = self.file_manager.len();
            if let Err(msg) = self.append_fill(index - self.file_manager.len() + 1) {
                return ActionResult::error(msg.to_string())
            }
        }
        match self.file_manager.overwrite_byte(index, c) {
            Ok(_) => {
                self.action_stack.add(Box::new(OverwriteAction::new(index, vec![c])));
                ActionResult::no_error(UpdateDescription::Range(start_byte, index + 1))
            },
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    fn insert_byte(&mut self, index: usize, c: u8) -> ActionResult {
        let mut start_byte = index;
        if index > self.file_manager.len() {
            start_byte = self.file_manager.len();
            if let Err(msg) = self.append_fill(index - self.file_manager.len()) {
                return ActionResult::error(msg.to_string())
            }
        }
        match self.file_manager.insert_byte(index, c) {
            Ok(_) => {
                self.action_stack.add(Box::new(InsertAction::new(index, 1)));
                ActionResult::no_error(UpdateDescription::After(start_byte))
            },
            Err(msg) => ActionResult::error(msg.to_string())
        }
        
    }

    fn delete_byte(&mut self, index: usize) -> ActionResult {
        match self.file_manager.delete_byte(index) {
            Ok(_) => ActionResult::no_error(UpdateDescription::After(index)),
            Err(msg) => ActionResult::error(msg.to_string())
        }
        //TODO: Add action record
    }

    pub fn set_cursor_pos(&mut self, index: usize) -> ActionResult {
        let line = index / (self.line_length as usize);
        self.cursor_pos = index;


        if line < self.start_line {
            match self.set_viewport_row(line){
                Ok(_) => ActionResult::no_error(UpdateDescription::All),
                Err(msg) => ActionResult::error(msg.to_string())
            }
        } else if line >= self.start_line as usize + self.height {
            //println!("Going down: {} {}", self.start_line, line - self.height + 1);
            match self.set_viewport_row(line - self.height + 1) {
                Ok(_) => ActionResult::no_error(UpdateDescription::All),
                Err(msg) => ActionResult::error(msg.to_string())
            }
        } else {
            ActionResult::no_error(UpdateDescription::AttrsOnly)
        }
    }

    fn advance_cursor(&mut self) -> ActionResult {
        match self.edit_mode {
            EditMode::HexOverwrite | EditMode::HexInsert => {
                match self.nibble {
                    Nibble::Left => {
                        self.nibble = Nibble::Right;
                        ActionResult::no_error(UpdateDescription::AttrsOnly)
                    },
                    Nibble::Right => {
                        self.nibble = Nibble::Left;
                        self.set_cursor_pos(self.cursor_pos + 1)
                    }
                }
            },
            EditMode::AsciiOverwrite | EditMode::AsciiInsert => {
                self.set_cursor_pos(self.cursor_pos + 1)
            }
        }
    }

    fn retreat_cursor(&mut self) -> ActionResult {
        match self.edit_mode {
            EditMode::HexOverwrite | EditMode::HexInsert => {
                match self.nibble {
                    Nibble::Left => {
                        if self.cursor_pos > 0 {
                            self.nibble = Nibble::Right;
                            self.set_cursor_pos(self.cursor_pos - 1)
                        } else {
                            ActionResult::empty()
                        }
                    },
                    Nibble::Right => {
                        self.nibble = Nibble::Left;
                        ActionResult::no_error(UpdateDescription::AttrsOnly)
                    }
                }
            },
            EditMode::AsciiOverwrite | EditMode::AsciiInsert => {
                if self.cursor_pos > 0 {
                    self.set_cursor_pos(self.cursor_pos - 1)
                } else {
                    ActionResult::empty()
                }
            }
        }
    }

    fn hex_cursor_x(&self) -> i32 {
        let offset = self.file_manager.line_label_len() + self.separator.len();
        (self.x + (self.cursor_pos % (self.line_length as usize))*3 + offset + match self.nibble {Nibble::Left => 0, Nibble::Right => 1}) as i32
    }

    fn ascii_cursor_x(&self) -> i32 {
        let offset = self.file_manager.line_label_len() + self.separator.len() * 2 + (self.line_length as usize) * 3 - 1;
        (self.x + (self.cursor_pos % (self.line_length as usize)) + offset) as i32
    }

    fn cursor_y(&self) -> i32 {
        (self.y + self.cursor_pos / (self.line_length as usize) - self.start_line) as i32
    }

    pub fn refresh_cursor(&self, window: &Window) {

        //let mut underline_attr = Attributes::new();
        //underline_attr.set_underline(true);
        //let underline_attr = chtype::from(underline_attr);

        match self.edit_mode {
            EditMode::HexOverwrite | EditMode::HexInsert => {
                //window.mvchgat(self.cursor_y(), self.ascii_cursor_x(), 1, underline_attr, 0);

                window.mv(self.cursor_y(), self.hex_cursor_x());

            },
            EditMode::AsciiOverwrite | EditMode::AsciiInsert => {
                //window.mvchgat(self.cursor_y(), self.hex_cursor_x(), 2, underline_attr, 0);

                window.mv(self.cursor_y(), self.ascii_cursor_x());
            },
        }
    }

    pub fn addch(&mut self, c: Input) -> ActionResult {
        match c {
            Input::KeyRight => {
                self.advance_cursor()
            },
            Input::KeyLeft => {
                self.retreat_cursor()
            },
            Input::KeyUp => {
                if self.cursor_pos >= self.line_length as usize {
                    self.set_cursor_pos(self.cursor_pos - self.line_length as usize)
                } else {
                    ActionResult::empty()
                }
            },
            Input::KeyDown => {
                self.set_cursor_pos(self.cursor_pos + self.line_length as usize)
            },
            Input::KeyNPage => {
                let row = self.start_line;
                self.set_cursor_pos(self.cursor_pos + self.height*(self.line_length as usize));
                match self.set_viewport_row(row + self.height) {
                    Ok(_) => ActionResult::no_error(UpdateDescription::All),
                    Err(msg) => ActionResult::error(msg.to_string())
                }
                
            },
            Input::KeyPPage => {
                let row = self.start_line;
                if self.cursor_pos >= self.height*(self.line_length as usize) && self.start_line >= self.height {
                    self.set_cursor_pos(self.cursor_pos - self.height*(self.line_length as usize));
                    if let Err(msg) = self.set_viewport_row(row - self.height) {
                        return ActionResult::error(msg.to_string());
                    }
                } else {
                    self.set_cursor_pos(0);
                    if let Err(msg) = self.set_viewport_row(0) {
                        return ActionResult::error(msg.to_string());
                    }
                }
                ActionResult::no_error(UpdateDescription::All)
            },
            Input::KeyHome => {
                self.set_cursor_pos(0)
            },
            Input::KeyEnd => {
                self.set_cursor_pos(self.file_manager.len())
            },
            Input::Character(ch) => {
                match self.edit_mode {
                    EditMode::HexOverwrite => {
                        return self.addch_hex_overwrite(ch) 
                    },
                    EditMode::HexInsert => {
                        return self.addch_hex_insert(ch)
                    },
                    EditMode::AsciiOverwrite => {
                        self.addch_ascii_overwrite(ch)
                    },
                    EditMode::AsciiInsert => {
                        self.addch_ascii_insert(ch)
                    }
                }
            },
            _ => ActionResult::error("Input not recognized".to_string())
        }

    }

    fn addch_hex_overwrite(&mut self, ch: char) -> ActionResult {
        let b: u8 = match self.get_byte(self.cursor_pos) {Some(x) => x, None => 0};

        let a: u8 = match ch {
            '0'..='9' => {
                (ch as u8) -  48
            },
            'a'..='f' => {
                (ch as u8) - 87
            },
            'A'..='F' => {
                (ch as u8) - 55
            },
            '\u{08}' => { // Backspace
                // Does the same thing as Key_Left in overwrite mode
                if self.cursor_pos > 0 || matches!(self.nibble, Nibble::Right) {
                    return self.retreat_cursor()
                } else {
                    return ActionResult::error("Invalid Keystroke".to_string());
                }
            },
            _ => return ActionResult::error(format!("Invalid character! {}", ch))
        };

        let result = match self.nibble {
            Nibble::Left => {
                self.overwrite_byte(self.cursor_pos, (a << 4) | (b & 0x0f))
            },
            Nibble::Right => {
                self.overwrite_byte(self.cursor_pos, (a & 0x0f) | (b & 0xf0))
            }
        };

        self.advance_cursor();
        
        result
    }

    fn addch_hex_insert(&mut self, ch: char) -> ActionResult {
        let b: u8 = match self.get_byte(self.cursor_pos) {Some(x) => x, None => 0};

        let a = match ch {
            '0'..='9' => {
                (ch as u8) -  48
            },
            'a'..='f' => {
                (ch as u8) - 87
            },
            'A'..='F' => {
                (ch as u8) - 55
            },
            '\u{08}' => { // Backspace
                if self.cursor_pos > 0 || matches!(self.nibble, Nibble::Right) {
                    self.retreat_cursor();
                    match self.nibble {
                        Nibble::Left => {
                            return self.delete_byte(self.cursor_pos);
                        },
                        Nibble::Right => {
                            return self.overwrite_byte(self.cursor_pos, b & 0xf0);
                        }
                    }
                } else {
                    return ActionResult::error("Invalid Keystroke".to_string());
                }
            },
            _ => return ActionResult::error(format!("Invalid character! {}", ch))
        };

        let result = match self.nibble {
            Nibble::Left => {
                self.insert_byte(self.cursor_pos, a << 4)
            },
            Nibble::Right => {
                self.overwrite_byte(self.cursor_pos, (a & 0x0f) | (b & 0xf0))
            }
        };

        self.advance_cursor();
        
        result
    }

    fn addch_ascii_overwrite(&mut self, ch: char) -> ActionResult {
        match ch {
            '\u{08}' => { // Backspace
                // Does the same thins as Key_Left in overwrite mode
                if self.cursor_pos > 0{
                    self.retreat_cursor()
                } else {
                    ActionResult::error("Invalid Keystroke".to_string())
                }
            },
            _ => {
                let result = self.overwrite_byte(self.cursor_pos, ch as u8);
                self.advance_cursor();
                result
            }
        }
    }

    fn addch_ascii_insert(&mut self, ch:char) -> ActionResult {
        match ch {
            '\u{08}' => { // Backspace
                if self.cursor_pos > 0{
                    let result = self.delete_byte(self.cursor_pos - 1);
                    self.retreat_cursor();
                    result
                } else {
                    ActionResult::error("Invalid Keystroke".to_string())
                }
            },
            _ => {
                let result = self.insert_byte(self.cursor_pos, ch as u8);
                self.advance_cursor();
                result
            }
        }
    }

    fn refresh_highlights(&self, window: &mut Window) {
        // TODO: clean this up!!!
        let mut rv_attr = Attributes::new();
        rv_attr.set_reverse(true);
        let rv_attr = chtype::from(rv_attr);

        let start: usize = self.start_line * (self.line_length as usize);
        let end: usize = start + self.height * (self.line_length as usize);

        let hex_offset = self.file_manager.line_label_len() + self.separator.len();
        let ascii_offset = self.file_manager.line_label_len() + self.separator.len() * 2 + (self.line_length as usize) * 3 - 1;

        for h in self.highlights.iter() {
            //println!("{}, {}", h.start, h.span);
            if h.start + h.span > start && h.start < end { // If the highlight is in the viewport
                let first_line = (h.start / (self.line_length as usize)) as isize - (self.start_line as isize);
                let last_line = ((h.start + h.span) / (self.line_length as usize)) as isize - (self.start_line as isize);
                //println!("In view: {}, {}", first_line, last_line);
                for y in std::cmp::max(first_line, 0)..std::cmp::min(last_line, self.height as isize - 1) + 1 {
                    let mut x1 = 0;
                    let mut x2 = self.line_length as usize;
                    if y == first_line {
                        x1 = h.start % (self.line_length as usize);
                    }
                    if y == last_line {
                        x2 = (h.start + h.span) % (self.line_length as usize);
                    }
                    //println!("y={}", y);
                    window.mvchgat(y as i32, (x1*3 + hex_offset) as i32, ((x2 - x1)*3 - 1) as i32, rv_attr, 0);
                    window.mvchgat(y as i32, (x1 + ascii_offset) as i32, (x2 - x1) as i32, rv_attr, 0);
                }
            }
        }
    }

    pub fn update(&mut self, window: &mut Window, update: UpdateDescription) -> std::io::Result<()> {
        match update {
            UpdateDescription::NoUpdate => {
                ()
            },
            UpdateDescription::AttrsOnly => {
                println!("Refreshing attrs");
                self.refresh_cursor(window);
                self.refresh_highlights(window);
            },
            UpdateDescription::After(n) =>{
                let line = clip_to_range(n / (self.line_length as usize), self.start_line, self.start_line + self.height);
                println!("Refreshing lines {} through {}", line - self.start_line, self.height);
                self.refresh_viewport()?;
                self.populate(window, line - self.start_line, self.height);
                self.refresh_cursor(window);
                self.refresh_highlights(window); 
            },
            UpdateDescription::Range(n1, n2) => {
                let line1 = clip_to_range(n1 / (self.line_length as usize), self.start_line, self.start_line + self.height);
                let line2 = clip_to_range((n2 - 1) / (self.line_length as usize), self.start_line, self.start_line + self.height);

                println!("Refreshing lines {} through {}", line1 - self.start_line, line2 - self.start_line);
                self.refresh_viewport()?;
                self.populate(window, line1 - self.start_line, line2 - self.start_line + 1);
                self.refresh_cursor(window);
                self.refresh_highlights(window); 
            },
            UpdateDescription::All => {
                println!("Refreshing all");
                self.refresh_viewport()?;
                self.populate(window, 0, self.height);
                self.refresh_cursor(window);
                self.refresh_highlights(window);                
            }

        }

        Ok(())
    }

    fn populate(&self, window: &mut Window, start: usize, end: usize) {


        let mut s = String::new();
        for i in start..end {
            println!("{}", i);
            if self.capitalize_hex {
                s += &format!("{:0width$x}", (self.start_line + i) * (self.line_length as usize), width=self.file_manager.line_label_len()).to_ascii_uppercase();
            } else {
                s += &format!("{:0width$x}", (self.start_line + i) * (self.line_length as usize), width=self.file_manager.line_label_len());
            }
            s += &self.separator;
            if self.valid_bytes > i * (self.line_length as usize) {
                s += &self.read_to_composite_line(&self.get_display_line(i), self.valid_bytes - i * (self.line_length as usize));
            } else {
                s += &self.read_to_composite_line(&self.get_display_line(i), 0);
            }
            
            s += &"\n".to_string();
        }

        window.mvaddstr((self.y + start) as i32, self.x as i32, s);
    }

    pub fn draw(&self, window: &mut Window) {

        self.populate(window, 0, self.height);
        self.refresh_highlights(window);
        self.refresh_cursor(window);
    }

}