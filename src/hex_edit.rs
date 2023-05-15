use std::fs::File;
use std::io;
use std::io::{Read, Seek, SeekFrom};

extern crate pancurses;
use pancurses::{Input, Window, Attributes, chtype};

struct FindResult {
    start: usize,
    span: usize
}

pub enum FileManagerType {
    RamOnly,
    SwapFile,
    LiveEdit
}

pub struct FileManager<'a> {
    filename: String,
    file_manager_type: FileManagerType,
    handle: File,
    metadata: std::fs::Metadata,
    swap_handle: Option<&'a File>, // Only used for FileManagerType::SwapFile
    swap_metadata: Option<&'a std::fs::Metadata>, // Only used for FileManagerType::SwapFile
    file_buffer: Vec<u8>, // Only used for FileManagerType::RamOnly
}

impl FileManager<'_> {
    pub fn new<'a>(filename: String, file_manager_type: FileManagerType) -> FileManager<'a>{

        let mut handle = File::open(&filename).expect("No file found");
        let metadata = handle.metadata().expect("Unable to grab metadata");

        let mut file_buffer = vec![];
        match file_manager_type {
            FileManagerType::RamOnly => {
                handle.read_to_end(&mut file_buffer);
            },
            FileManagerType::SwapFile => {
                // TODO
            },
            FileManagerType::LiveEdit => {
                // TODO
            }
        }

        FileManager {
            filename,
            file_manager_type,
            handle,
            metadata,
            swap_handle: None,
            swap_metadata: None,
            file_buffer
        }
    }

    pub fn get_bytes(&mut self, index: usize, buffer: &mut Vec<u8>) -> std::io::Result<()> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                let n = buffer.len();
                buffer.copy_from_slice(&self.file_buffer[index..(index + n)]);
                Ok(())
            },
            FileManagerType::SwapFile => {
                Ok(())// TODO

            },
            FileManagerType::LiveEdit => {
                self.handle.seek(SeekFrom::Start(index as u64));
                self.handle.read_exact(buffer)
            }
        }
    }

    pub fn overwrite_byte(&mut self, index: usize, c: u8)  -> std::io::Result<()> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer[index] = c;
                Ok(())
            }
            FileManagerType::SwapFile => {
                Ok(())// TODO

            },
            FileManagerType::LiveEdit => {
                Ok(())// TODO
            }
        }
    }

    pub fn insert_byte(&mut self, index: usize, c: u8)  -> std::io::Result<()> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer.insert(index, c);
                Ok(())
            }
            FileManagerType::SwapFile => {
                Ok(())// TODO

            },
            FileManagerType::LiveEdit => {
                Ok(())// TODO
            }
        }
    }

    pub fn delete_byte(&mut self, index: usize)  -> std::io::Result<()> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer.remove(index);
                Ok(())
            }
            FileManagerType::SwapFile => {
                Ok(())// TODO

            },
            FileManagerType::LiveEdit => {
                Ok(())// TODO
            }
        }
    }

    pub fn get_byte(&mut self, index: usize) -> u8 {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer[index]
            },
            FileManagerType::SwapFile => {
                0 // TODO

            },
            FileManagerType::LiveEdit => {
                0 // TODO
            }
        }
    }

    pub fn insert_bytes(&mut self, index: usize, bytes: &Vec<u8>)  -> std::io::Result<()> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                let second_part = self.file_buffer[index..self.file_buffer.len()].to_vec();
                unsafe {self.file_buffer.set_len(index)};
                self.file_buffer.extend(bytes);
                self.file_buffer.extend(second_part);
                Ok(())
            }
            FileManagerType::SwapFile => {
                Ok(())// TODO

            },
            FileManagerType::LiveEdit => {
                Ok(())// TODO
            }
        }
    }

    pub fn overwrite_bytes(&mut self, index: usize, bytes: &Vec<u8>)  -> std::io::Result<()> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                for (i, c) in bytes.iter().enumerate() {
                    self.file_buffer[i + index] = *c
                }
                Ok(())
            }
            FileManagerType::SwapFile => {
                Ok(())// TODO

            },
            FileManagerType::LiveEdit => {
                Ok(())// TODO
            }
        }
    }

    pub fn delete_bytes(&mut self, index: usize, n_bytes: usize)  -> std::io::Result<()> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                let second_part = self.file_buffer[(index + n_bytes)..self.file_buffer.len()].to_vec();
                unsafe {self.file_buffer.set_len(index)};
                self.file_buffer.extend(second_part);
                Ok(())
            }
            FileManagerType::SwapFile => {
                Ok(())// TODO

            },
            FileManagerType::LiveEdit => {
                Ok(())// TODO
            }
        }
    }

    pub fn len(&self) -> usize{
        self.metadata.len() as usize
    }

    pub fn line_label_len(&self) -> usize{
        format!("{:x}", self.metadata.len()).len()
    }

    pub fn find(&self, expr: &Vec<u8>, ignore_case: bool) -> Vec<FindResult>{
        let mut result = Vec::<FindResult>::new();
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                if ignore_case {
                    let slice = expr.as_slice().to_ascii_lowercase();
                    for (i, w) in self.file_buffer.windows(expr.len()).enumerate() {
                        if w.to_ascii_lowercase() == slice {
                            result.push(FindResult {start: i, span: expr.len()});
                        }
                    }
                } else {
                    let slice = expr.as_slice();
                    for (i, w) in self.file_buffer.windows(expr.len()).enumerate() {
                        if w == slice {
                            result.push(FindResult {start: i, span: expr.len()});
                        }
                    }
                }
            }
            FileManagerType::SwapFile => {
                // TODO
            },
            FileManagerType::LiveEdit => {
                // TODO
            }
        } 
        
        result
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
    fn undo(&mut self, hex_edit: &mut HexEdit);
    fn redo(&mut self, hex_edit: &mut HexEdit);
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
    fn undo(&mut self, hex_edit: &mut HexEdit) {
        hex_edit.delete_bytes(self.index, self.n_bytes);
    }

    fn redo(&mut self, hex_edit: &mut HexEdit) {
        hex_edit.insert_bytes(self.index, &self.bytes);
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
    fn undo(&mut self, hex_edit: &mut HexEdit) {
        let mut buffer = vec![0; self.bytes.len()];
        hex_edit.get_bytes(self.index, &mut buffer);
        hex_edit.overwrite_bytes(self.index, &self.bytes);
        self.bytes = buffer;
    }

    fn redo(&mut self, hex_edit: &mut HexEdit) {
        let mut buffer = vec![0; self.bytes.len()];
        hex_edit.get_bytes(self.index, &mut buffer);
        hex_edit.overwrite_bytes(self.index, &self.bytes);
        self.bytes = buffer;
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
            action_stack: ActionStack::new(),
            clipboard_registers: Default::default()
        }
    }

    pub fn set_capitalize_hex(&mut self, caps: bool) {
        self.capitalize_hex = caps;
    }

    pub fn set_line_length(&mut self, length: u8) {
        self.line_length = length;
        self.set_viewport_row(0);
    }

    pub fn set_show_ascii(&mut self, show_ascii: bool) {
        self.show_ascii = show_ascii;
    }

    pub fn set_show_hex(&mut self, show_hex: bool) {
        self.show_hex = show_hex;
    }

    pub fn set_ignore_case(&mut self, ignore_case: bool) {
        self.ignore_case = ignore_case;
    }

    pub fn set_edit_mode(&mut self, edit_mode: EditMode) {
        self.edit_mode = edit_mode;
    }

    pub fn set_fill(&mut self, fill: FillType) {
        self.fill = fill;
    }

    pub fn get_cursor_pos(&self) -> usize {
        self.cursor_pos
    }

    pub fn clear_find(&mut self) {
        self.highlights = Vec::<Highlight>::new();
    }

    pub fn find(&mut self, expr: &Vec<char>) {
        let expr: Vec<u8> = expr.iter().map(|c| *c as u8).collect();
        for res in self.file_manager.find(&expr, self.ignore_case) {
            self.highlights.push(Highlight {start: res.start, span: res.span})
        }
    }

    pub fn seek_next(&mut self) {
        for h in self.highlights.iter() {
            if h.start > self.cursor_pos {
                self.set_cursor_pos(h.start);
                return;
            }
        }
    }

    pub fn seek_prev(&mut self) {
        for h in self.highlights.iter().rev() {
            if h.start < self.cursor_pos {
                self.set_cursor_pos(h.start);
                return;
            }
        }
    }

    pub fn len(&self) -> usize {
        self.file_manager.len()
    }

    pub fn insert_fill(&mut self, index: usize, n_bytes: usize) {
        let mut bytes = Vec::<u8>::new();
        match &self.fill {
            FillType::Bytes(buffer) => {
                bytes = buffer.iter().cycle().take(n_bytes).cloned().collect();
            },
            FillType::Register(n) => {
                bytes = self.clipboard_registers[*n as usize].iter().cycle().take(n_bytes).cloned().collect();
            }
        }
        self.file_manager.insert_bytes(index, &bytes);
        self.refresh_viewport();
    }

    pub fn overwrite_fill(&mut self, index: usize, n_bytes: usize) {
        let mut bytes = Vec::<u8>::new();
        match &self.fill {
            FillType::Bytes(buffer) => {
                bytes = buffer.iter().cycle().take(n_bytes).cloned().collect();
            },
            FillType::Register(n) => {
                bytes = self.clipboard_registers[*n as usize].iter().cycle().take(n_bytes).cloned().collect();
            }
        }
        self.file_manager.overwrite_bytes(index, &bytes);
        self.refresh_viewport();
    }

    pub fn insert_register(&mut self, register: u8, index: usize) {
        self.file_manager.insert_bytes(index, &self.clipboard_registers[register as usize]);
        self.refresh_viewport();
    }

    pub fn overwrite_register(&mut self, register: u8, index: usize) {
        self.file_manager.overwrite_bytes(index, &self.clipboard_registers[register as usize]);
        self.refresh_viewport();
    }

    pub fn yank(&mut self, register: u8, index: usize, n_bytes: usize) {
        println!("Yanking {} bytes into register {}", n_bytes, register);
        assert!(register < 32);
        let mut buffer = vec![0; n_bytes];
        self.file_manager.get_bytes(index, &mut buffer);
        self.clipboard_registers[register as usize] = buffer;
    }

    pub fn delete_bytes(&mut self, index: usize, n_bytes: usize) {
        self.file_manager.delete_bytes(index, n_bytes);
        self.refresh_viewport();
    }

    pub fn insert_bytes(&mut self, index: usize, bytes: &Vec<u8>) {
        // TODO
        println!("Inserting {} bytes at {}!", bytes.len(), index);
    }

    pub fn overwrite_bytes(&mut self, index: usize, bytes: &Vec<u8>) {
        // TODO: Make this more efficient
        let mut original_data = vec![0; bytes.len()];
        self.file_manager.get_bytes(index, &mut original_data);
        self.file_manager.overwrite_bytes(index, bytes);
        self.refresh_viewport();
    }

    pub fn swap_bytes(&mut self, index: usize, n_bytes: usize) {
        let mut data = vec![0; n_bytes];
        self.file_manager.get_bytes(index, &mut data);
        data.reverse();
        self.file_manager.overwrite_bytes(index, &data);
        self.refresh_viewport();
    }

    pub fn get_bytes(&mut self, index: usize, buffer: &mut Vec<u8>) {
        self.file_manager.get_bytes(index, buffer);
    }

    pub fn get_byte(&mut self, index: usize) -> u8 {
        self.file_manager.get_byte(index)
    }

    pub fn undo(&mut self, n: usize) {
        //self.action_stack.undo(self);
        println!("Undoing {} actions", n); //TODO
    }

    pub fn redo(&mut self, n: usize) {
        //self.action_stack.redo(self);
        println!("Redoing {} actions", n); //TODO
    }

    pub fn resize(&mut self, width: usize, height: usize) {
        self.width = width;
        self.height = height;
        self.refresh_viewport();
    }

    fn bytes_to_hex_line(buffer: &Vec<u8>) -> String {
        buffer.iter()
        .map(|b| format!("{:02x}", b).to_string())
        .collect::<Vec<String>>()
        .join(" ")
    }
    
    fn bytes_to_ascii_line(buffer: &Vec<u8>, invalid: char) -> String {
        buffer.iter()
        .map(|b| format!("{}", match b {32..=126 => *b as char, _ => invalid}).to_string())
        .collect::<Vec<String>>()
        .join("")
    }
    
    fn read_to_composite_line(&self, buffer: &Vec<u8>) -> String {
        let mut s: String = "".to_string();
        if self.show_hex {
            if self.capitalize_hex {
                s += &HexEdit::bytes_to_hex_line(&buffer).to_uppercase();
            } else {
                s += &HexEdit::bytes_to_hex_line(&buffer).to_lowercase();
            }
            if self.show_ascii {
                s += &self.separator;
            }
        } 

        if self.show_ascii {
            s += &HexEdit::bytes_to_ascii_line(&buffer, self.invalid_ascii_char)
        }

        s
    }

    pub fn set_viewport_row(&mut self, row: usize) {
        self.start_line = row;
        self.refresh_viewport();
    }

    fn refresh_viewport(&mut self) {
        //println!("{}", self.start_line);
        self.display_data = vec![0; (self.line_length as usize) * (self.height)];
        //self.file_manager.seek(SeekFrom::Start((self.start_line * (self.line_length as usize)) as u64));
        self.file_manager.get_bytes(self.start_line * (self.line_length as usize), &mut self.display_data);
    }

    fn get_display_line(&self, line: usize) -> Vec<u8>{
        //println!("{}, {}, {}", line*(self.line_length as usize), (line + 1)*(self.line_length as usize), self.display_data.len());
        self.display_data[line*(self.line_length as usize)..(line + 1)*(self.line_length as usize)].to_vec()
    }

    fn overwrite_byte(&mut self, index: usize, c: u8) {
        self.file_manager.overwrite_byte(index, c);
        self.action_stack.add(Box::new(OverwriteAction::new(index, vec![c])));
        self.refresh_viewport(); //TODO: Make this more efficient
    }

    fn insert_byte(&mut self, index: usize, c: u8) {
        self.file_manager.insert_byte(index, c);
        self.action_stack.add(Box::new(InsertAction::new(index, 1)));
        self.refresh_viewport(); //TODO: Make this more efficient
    }

    fn delete_byte(&mut self, index: usize) {
        self.file_manager.delete_byte(index);
        //TODO: Add action record
        self.refresh_viewport(); //TODO: Make this more efficient
    }

    pub fn set_cursor_pos(&mut self, index: usize) -> bool {
        let line = index / (self.line_length as usize);
        self.cursor_pos = index;
        //println!("{}, {}", line, self.height);
        if line < self.start_line {
            self.set_viewport_row(line);
            true
        } else if line >= self.start_line as usize + self.height {
            //println!("Going down: {} {}", self.start_line, line - self.height + 1);
            self.set_viewport_row(line - self.height + 1);
            true
        } else {
            false
        }
    }

    fn advance_cursor(&mut self) -> bool {
        match self.edit_mode {
            EditMode::HexOverwrite | EditMode::HexInsert => {
                match self.nibble {
                    Nibble::Left => {
                        self.nibble = Nibble::Right;
                        false
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

    fn retreat_cursor(&mut self) -> bool {
        match self.edit_mode {
            EditMode::HexOverwrite | EditMode::HexInsert => {
                match self.nibble {
                    Nibble::Left => {
                        if self.cursor_pos > 0 {
                            self.nibble = Nibble::Right;
                            self.set_cursor_pos(self.cursor_pos - 1)
                        } else {
                            false
                        }
                    },
                    Nibble::Right => {
                        self.nibble = Nibble::Left;
                        false
                    }
                }
            },
            EditMode::AsciiOverwrite | EditMode::AsciiInsert => {
                if self.cursor_pos > 0 {
                    self.set_cursor_pos(self.cursor_pos - 1)
                } else {
                    false
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
        match self.edit_mode {
            EditMode::HexOverwrite | EditMode::HexInsert => {
                window.mv(self.cursor_y(), self.hex_cursor_x());
            },
            EditMode::AsciiOverwrite | EditMode::AsciiInsert => {
                window.mv(self.cursor_y(), self.ascii_cursor_x());
            },
        }
    }

    pub fn addch(&mut self, window: &Window, c: Input) -> bool {
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
                    false
                }
            },
            Input::KeyDown => {
                self.set_cursor_pos(self.cursor_pos + self.line_length as usize)
            },
            Input::KeyNPage => {
                let row = self.start_line;
                self.set_cursor_pos(self.cursor_pos + self.height*(self.line_length as usize));
                self.set_viewport_row(row + self.height);
                true
            },
            Input::KeyPPage => {
                let row = self.start_line;
                if self.cursor_pos >= self.height*(self.line_length as usize) && self.start_line >= self.height {
                    self.set_cursor_pos(self.cursor_pos - self.height*(self.line_length as usize));
                    self.set_viewport_row(row - self.height);
                } else {
                    self.set_cursor_pos(0);
                    self.set_viewport_row(0);
                }
                true
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
                        self.addch_hex_overwrite(window, ch); // TODO - make this return bool
                        true
                    },
                    EditMode::HexInsert => {
                        self.addch_hex_insert(window, ch); // TODO - make this return bool
                        true
                    },
                    EditMode::AsciiOverwrite => {
                        self.addch_ascii_overwrite(window, ch); // TODO - make this return bool
                        true
                    },
                    EditMode::AsciiInsert => {
                        self.addch_ascii_insert(window, ch); // TODO - make this return bool
                        true
                    },
                    _ => false
                }
            },
            _ => false // TODO
        }

    }

    fn addch_hex_overwrite(&mut self, window: &Window, ch: char) -> Option<String>{
        let mut a: u8 = 0;
        let b: u8 = self.get_byte(self.cursor_pos);

        match ch {
            '0'..='9' => {
                a = (ch as u8) -  48;
            },
            'a'..='f' => {
                a = (ch as u8) - 87;
            },
            'A'..='F' => {
                a = (ch as u8) - 55;
            },
            '\u{08}' => { // Backspace
                // Does the same thing as Key_Left in overwrite mode
                if self.cursor_pos > 0 || matches!(self.nibble, Nibble::Right) {
                    self.retreat_cursor();
                    return None;
                } else {
                    return Some("Invalid Keystroke".to_string());
                }
            },
            _ => return Some(format!("Invalid character! {}", ch))
        }

        match self.nibble {
            Nibble::Left => {
                self.overwrite_byte(self.cursor_pos, (a << 4) | (b & 0x0f));
            },
            Nibble::Right => {
                self.overwrite_byte(self.cursor_pos, (a & 0x0f) | (b & 0xf0));
            }
        }

        self.advance_cursor();
        None
    }

    fn addch_hex_insert(&mut self, window: &Window, ch: char) -> Option<String>{
        let mut a: u8 = 0;
        let b: u8 = self.get_byte(self.cursor_pos);

        match ch {
            '0'..='9' => {
                a = (ch as u8) -  48;
            },
            'a'..='f' => {
                a = (ch as u8) - 87;
            },
            'A'..='F' => {
                a = (ch as u8) - 55;
            },
            '\u{08}' => { // Backspace
                if self.cursor_pos > 0 || matches!(self.nibble, Nibble::Right) {
                    self.retreat_cursor();
                    match self.nibble {
                        Nibble::Left => {
                            self.delete_byte(self.cursor_pos);
                        },
                        Nibble::Right => {
                            self.overwrite_byte(self.cursor_pos, b & 0xf0);
                        }
                    }
                    return None;
                } else {
                    return Some("Invalid Keystroke".to_string());
                }
            },
            _ => return Some(format!("Invalid character! {}", ch))
        }

        match self.nibble {
            Nibble::Left => {
                self.insert_byte(self.cursor_pos, a << 4);
            },
            Nibble::Right => {
                self.overwrite_byte(self.cursor_pos, (a & 0x0f) | (b & 0xf0));
            }
        }

        self.advance_cursor();
        None
    }

    fn addch_ascii_overwrite(&mut self, window: &Window, ch: char) -> Option<String>{
        match ch {
            '\u{08}' => { // Backspace
                // Does the same thins as Key_Left in overwrite mode
                if self.cursor_pos > 0{
                    self.retreat_cursor();
                } else {
                    return Some("Invalid Keystroke".to_string());
                }
            },
            _ => {
                self.overwrite_byte(self.cursor_pos, ch as u8);
                self.advance_cursor();
            }
        }
        None
    }

    fn addch_ascii_insert(&mut self, window: &Window, ch:char) -> Option<String> {
        match ch {
            '\u{08}' => { // Backspace
                if self.cursor_pos > 0{
                    self.delete_byte(self.cursor_pos - 1);
                    self.retreat_cursor();
                } else {
                    return Some("Invalid Keystroke".to_string());
                }
            },
            _ => {
                self.insert_byte(self.cursor_pos, ch as u8);
                self.advance_cursor();
            }
        }
        None
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

    pub fn draw(&self, window: &mut Window) {

        let mut s = String::new();
        for i in 0..self.height {
            s += &(format!("{:0width$x}", (self.start_line + i) * (self.line_length as usize), width=self.file_manager.line_label_len()) 
                + &self.separator 
                + &self.read_to_composite_line(&self.get_display_line(i)) 
                + &"\n".to_string());
        }

        window.mvaddstr(self.x as i32, self.y as i32, s);
        self.refresh_highlights(window);
        self.refresh_cursor(window);
    }

}