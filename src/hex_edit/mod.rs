use std::rc::Rc;
use std::io::SeekFrom;
use std::io::Write;

use flate2::write::DeflateEncoder;
use flate2::write::DeflateDecoder;
use flate2::Compression;

use bitutils2::{BitIndex};

use crate::globals;

use crate::AlertType;
use crate::expr::ExprValue;

mod file_manager;
pub use crate::hex_edit::file_manager::{FileManagerType, FileManager};

mod file_specs;
pub use crate::hex_edit::file_specs::{BinaryField, FileMap, FileRegion, Structure, make_png};


extern crate pancurses;
use pancurses::{Input, Window, Attributes, chtype, start_color, init_pair};

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

#[derive(Copy, Clone)]
pub enum EditMode {
    HexOverwrite,
    AsciiOverwrite,
    HexInsert,
    AsciiInsert
}

pub enum ShowType {
    Off,
    Dec,
    Hex
}

#[derive(Copy, Clone)]
pub enum ByteOperation {
    And,
    Or,
    Nand,
    Nor,
    Xor,
    Xnor
}

pub struct Highlight {
    pub start: usize,
    pub span: usize
}

pub enum FillType {
    Bytes(Vec<u8>),
    Register(u8)
}

pub enum DataSource {
    Bytes(Vec<u8>),
    Fill(usize),
    Register(u8)
}


pub trait Action {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult;
    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult;
    fn size(&self) -> usize;
    //fn copy(&self) -> Self;
}

pub struct InsertAction {
    data: DataSource,
    file_end: usize
}

impl InsertAction {
    fn new(data: DataSource, file_end: usize) -> InsertAction {
        InsertAction {
            data, file_end
        }
    }
}

impl Action for InsertAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        let n_bytes = match &self.data {
            DataSource::Bytes(b) => b.len(),
            DataSource::Fill(n) => *n,
            DataSource::Register(n) => hex_edit.register_len(*n)
        };
        let seek_res = hex_edit.seek(SeekFrom::Current(-(n_bytes as i64)));
        let mut delete_res = hex_edit.delete_bytes(n_bytes); // TODO: Process error
        delete_res.update = combine_update(&seek_res.update, &delete_res.update);
        if self.file_end < hex_edit.len() {
            let truncate_res = hex_edit.truncate(self.file_end); // TODO: Process error
            delete_res.update = combine_update(&delete_res.update, &truncate_res.update);
        }
        delete_res
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.insert(match &self.data {
            DataSource::Bytes(b) => DataSource::Bytes(b.to_vec()),
            DataSource::Fill(n) => DataSource::Fill(*n),
            DataSource::Register(n) => DataSource::Register(*n)
        })
    }

    fn size(&self) -> usize{
        0 // TODO: fix his
    }
}

pub struct OverwriteAction {
    data: DataSource,
    original_bytes: Vec<u8>,
    file_end: usize
}

impl OverwriteAction {
    fn new(data: DataSource, original_bytes: Vec<u8>, file_end: usize) -> OverwriteAction {
        OverwriteAction {
            data,
            original_bytes,
            file_end
        }
    }
}

impl Action for OverwriteAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        let n_bytes = match &self.data {
            DataSource::Bytes(b) => b.len(),
            DataSource::Fill(n) => *n,
            DataSource::Register(n) => hex_edit.register_len(*n)
        };
        let seek_res = hex_edit.seek(SeekFrom::Current(-(n_bytes as i64)));
        let mut overwrite_res = hex_edit.overwrite(DataSource::Bytes(self.original_bytes.to_vec())) ; // TODO: Process error
        let seek_res_2 = hex_edit.seek(SeekFrom::Current(-(n_bytes as i64)));
        overwrite_res.update = combine_update(&seek_res.update, &overwrite_res.update);
        overwrite_res.update = combine_update(&overwrite_res.update, &seek_res_2.update);
        if self.file_end < hex_edit.len() {
            let truncate_res = hex_edit.truncate(self.file_end); // TODO: Process error
            overwrite_res.update = combine_update(&overwrite_res.update, &truncate_res.update);
        }
        overwrite_res
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.overwrite(match &self.data {
            DataSource::Bytes(b) => DataSource::Bytes(b.to_vec()),
            DataSource::Fill(n) => DataSource::Fill(*n),
            DataSource::Register(n) => DataSource::Register(*n)
        })
    }

    fn size(&self) -> usize{
        0 // TODO: fix his
    }
}

pub struct DeleteAction {
    bytes: Vec<u8>
}

impl DeleteAction {
    fn new(bytes: Vec<u8>) -> DeleteAction {
        DeleteAction {
            bytes
        }
    }
}

impl Action for DeleteAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.insert(DataSource::Bytes(self.bytes.to_vec()))
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.delete_bytes(self.bytes.len()) 
    }

    fn size(&self) -> usize{
        self.bytes.len()
    }
}

pub struct TruncateAction {
    size: usize,
    bytes: Vec<u8>
}

impl TruncateAction {
    fn new(size: usize, bytes: Vec<u8>) -> TruncateAction {
        TruncateAction {
            size,
            bytes
        }
    }
}

impl Action for TruncateAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.append_bytes(self.bytes.to_vec())
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.truncate(self.size) 
    }

    fn size(&self) -> usize{
        std::mem::size_of::<usize>() + self.bytes.len()
    }
}


pub struct SwapAction {
    n_bytes: usize
}

impl SwapAction {
    fn new(n_bytes: usize) -> SwapAction {
        SwapAction {
            n_bytes
        }
    }
}

impl Action for SwapAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.swap_bytes(self.n_bytes) 
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.swap_bytes(self.n_bytes)
    }

    fn size(&self) -> usize{
        std::mem::size_of::<usize>()
    }
}


pub struct YankAction {
    register: u8,
    n_bytes: usize,
    original_bytes: Vec<u8>
}

impl YankAction {
    pub fn new(register: u8, n_bytes: usize, original_bytes: Vec<u8>) -> YankAction {
        YankAction {
            register,
            n_bytes,
            original_bytes
        }
    }
}

impl Action for YankAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.set_register(self.register, &self.original_bytes)
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.yank(self.register, self.n_bytes)
    }

    fn size(&self) -> usize{
        1 + std::mem::size_of::<usize>() + self.original_bytes.len()
    }
}

pub struct SetRegisterAction {
    register: u8,
    new_bytes: Vec<u8>,
    original_bytes: Vec<u8>
}

impl SetRegisterAction {
    pub fn new(register: u8, new_bytes: Vec<u8>, original_bytes: Vec<u8>) -> SetRegisterAction {
        SetRegisterAction {
            register,
            new_bytes,
            original_bytes
        }
    }
}

impl Action for SetRegisterAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.set_register(self.register, &self.original_bytes)
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.set_register(self.register, &self.new_bytes)
    }

    fn size(&self) -> usize{
        1 + std::mem::size_of::<usize>() + self.original_bytes.len()
    }
}

pub struct SwapRegisterAction {
    register: u8
}

impl SwapRegisterAction {
    pub fn new(register: u8) -> SwapRegisterAction {
        SwapRegisterAction {
            register
        }
    }
}

impl Action for SwapRegisterAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.swap_register(self.register)
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.swap_register(self.register)
    }

    fn size(&self) -> usize{
        1
    }
}

pub struct InvertRegisterAction {
    register: u8
}

impl InvertRegisterAction {
    pub fn new(register: u8) -> InvertRegisterAction {
        InvertRegisterAction {
            register
        }
    }
}

impl Action for InvertRegisterAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.invert_register(self.register)
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.invert_register(self.register)
    }

    fn size(&self) -> usize{
        1
    }
}

pub struct ShiftRegisterAction {
    register: u8,
    shift: i8
}

impl ShiftRegisterAction {
    pub fn new(register: u8, shift: i8) -> ShiftRegisterAction {
        ShiftRegisterAction {
            register,
            shift
        }
    }
}

impl Action for ShiftRegisterAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.shift_register(self.register, -self.shift)
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.shift_register(self.register, self.shift)
    }

    fn size(&self) -> usize{
        2
    }
}

pub struct DeflateRegisterAction {
    register: u8,
    level: u8
}

impl DeflateRegisterAction {
    pub fn new(register: u8, level: u8) -> DeflateRegisterAction {
        DeflateRegisterAction {
            register,
            level
        }
    }
}

impl Action for DeflateRegisterAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.inflate_register(self.register)
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.deflate_register(self.register, self.level as u8)
    }

    fn size(&self) -> usize{
        2
    }
}

pub struct RegisterOpAction {
    register: u8,
    fill: FillType,
    original_bytes: Vec<u8>,
    op: ByteOperation
}

impl RegisterOpAction {
    pub fn new(register: u8, fill: FillType, original_bytes: Vec<u8>, op: ByteOperation) -> RegisterOpAction {
        RegisterOpAction {
            register,
            fill,
            original_bytes,
            op
        }
    }
}

impl Action for RegisterOpAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.set_register(self.register, &self.original_bytes)
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        let fill = match &self.fill {
            FillType::Register(n) => FillType::Register(*n),
            FillType::Bytes(v) => FillType::Bytes(v.to_vec())
        };
        hex_edit.manipulate_register(self.register, fill, self.op)
    }

    fn size(&self) -> usize {
        0 // TODO: make this accurate
    }
}

struct ConcatenateRegisterAction {
    register: u8,
    fill: FillType
}

impl ConcatenateRegisterAction {
    fn new(register: u8, fill: FillType) -> ConcatenateRegisterAction {
        ConcatenateRegisterAction {
            register,
            fill
        }
    }
}

impl Action for ConcatenateRegisterAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        let length = match &self.fill {
            FillType::Register(n) => hex_edit.register_len(*n),
            FillType::Bytes(v) => v.len()
        };
        if length <= hex_edit.register_len(self.register) {
            match hex_edit.get_register(self.register) {
                Ok(v) => hex_edit.set_register(self.register, &v[0..(hex_edit.register_len(self.register) - length)].to_vec()),
                Err(msg) => ActionResult::error(msg)
            }
        } else {
            ActionResult::error("Register does not have enough bytes to truncate".to_string())
        }
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        let fill = match &self.fill {
            FillType::Register(n) => FillType::Register(*n),
            FillType::Bytes(v) => FillType::Bytes(v.to_vec())
        };
        hex_edit.concatenate_register(self.register, fill)
    }

    fn size(&self) -> usize{
        16 // TODO this is wrong
    }
}

struct SliceRegisterAction {
    register: u8,
    left: Vec<u8>,
    right: Vec<u8>
}

impl SliceRegisterAction {
    fn new(register: u8, left: Vec<u8>, right: Vec<u8>) -> SliceRegisterAction {
        SliceRegisterAction {
            register,
            left,
            right
        }
    }
}

impl Action for SliceRegisterAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        let mut new = self.left.to_vec();
        match hex_edit.get_register(self.register) {
            Ok(mid) => {
                new.extend(mid);
                new.extend(&self.right);
                hex_edit.set_register(self.register, &new)
            },
            Err(msg) => ActionResult::error(msg)
        }
    } 

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        let n1 = self.left.len();
        let n2 = hex_edit.register_len(self.register) - self.right.len();
        hex_edit.slice_register(self.register, n1, n2)
    }

    fn size(&self) -> usize{
        16 // TODO this is wrong
    }
}


pub struct SeekAction {
    original_index: usize,
    seek: SeekFrom
}

impl SeekAction {
    pub fn new(original_index: usize, seek: SeekFrom) -> SeekAction {
        SeekAction {
            original_index,
            seek
        }
    }
}

impl Action for SeekAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.set_cursor_pos(self.original_index)
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        hex_edit.seek(self.seek)
    }

    fn size(&self) -> usize{
        16 // TODO confirm this is correct
    }
}

pub struct CompoundAction {
    actions: Vec<Rc<dyn Action>>
}

impl Action for CompoundAction {
    fn undo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        for action in self.actions.iter().rev() {
            (*action).undo(hex_edit);
        }
        ActionResult::no_error(UpdateDescription::All) // TODO make this more specific
    }

    fn redo(&self, hex_edit: &mut HexEdit) -> ActionResult {
        let mut res = Vec::<Rc<dyn Action>>::new();
        for action in self.actions.iter() {
            if let Some(a) = (*action).redo(hex_edit).action {
                res.push(Rc::clone(&a));
            }
        }
        ActionResult {
            alert: None,
            alert_type: AlertType::None,
            update: UpdateDescription::All, // TODO make this more specific
            action: Some(Rc::new(CompoundAction{actions: res}))
        }
    }

    fn size(&self) -> usize {
        let mut s: usize = 0;
        for action in self.actions.iter() {
            s += (*action).size();
        }
        s
    }
}

pub struct ActionStack {
    length: usize,
    index: usize,
    actions: Vec<Rc<dyn Action>>
}

impl ActionStack {
    pub fn new(length: usize) -> ActionStack {
        ActionStack {
            length,
            index: 0,
            actions: Vec::new()
        }
    }

    pub fn clear(&mut self) {
        self.index = 0;
        self.actions = Vec::new();
    }

    pub fn set_length(&mut self, length: usize) {
        self.length = length;
        while self.actions.len() > self.length && self.index > 0 {
            self.actions.remove(0);
            self.index -= 1;
        }        
    }

    pub fn current_index(&self) -> usize {
        self.index
    }

    pub fn add(&mut self, action: Rc<dyn Action>){
        self.actions.truncate(self.index);
        self.actions.push(Rc::clone(&action));
        self.index += 1;
        while self.actions.len() > self.length && self.index > 0 {
            self.actions.remove(0);
            self.index -= 1;
        }
    }

    pub fn undo(&mut self, hex_edit: &mut HexEdit) -> ActionResult {
        if self.index >= 1 {
            self.index -= 1;
            let mut res = self.actions[self.index].undo(hex_edit);
            res.set_action(None);
            res
        } else {
            ActionResult::error("No actions in stack".to_string())
        }
    }

    pub fn redo(&mut self, hex_edit: &mut HexEdit) -> ActionResult {
        if self.index < self.actions.len() {
            self.index += 1;
            let mut res = self.actions[self.index - 1].redo(hex_edit);
            res.set_action(None);
            res
        } else {
            ActionResult::error("No actions in stack".to_string())
        }
    }

    pub fn combine(&self, start_index: usize, stop_index: usize) -> CompoundAction {
        let mut res = Vec::<Rc<dyn Action>>::new();
        for i in start_index..stop_index {
            res.push(Rc::clone(&self.actions[i]));
        }
        CompoundAction{actions:res}
    }

}

pub enum UpdateDescription {
    NoUpdate,
    AttrsOnly,
    All,
    After(usize),
    Range(usize, usize)
}

fn copy_update(u: &UpdateDescription) -> UpdateDescription {
    match u {
        UpdateDescription::NoUpdate => UpdateDescription::NoUpdate,
        UpdateDescription::AttrsOnly => UpdateDescription::AttrsOnly,
        UpdateDescription::All => UpdateDescription::All,
        UpdateDescription::After(n) => UpdateDescription::After(*n),
        UpdateDescription::Range(n1, n2) => UpdateDescription::Range(*n1, *n2)
    }
}

fn combine_update(u1: &UpdateDescription, u2: &UpdateDescription) -> UpdateDescription {
    match (u1, u2) {
        (UpdateDescription::NoUpdate, update) => copy_update(update),
        (update, UpdateDescription::NoUpdate) => copy_update(update),
        (UpdateDescription::All, _) => UpdateDescription::All,
        (_, UpdateDescription::All) => UpdateDescription::All,
        (UpdateDescription::AttrsOnly, update) => copy_update(update),
        (update, UpdateDescription::AttrsOnly) => copy_update(update),
        (UpdateDescription::After(n1), UpdateDescription::After(n2)) => UpdateDescription::After(std::cmp::min(*n1, *n2)),
        (UpdateDescription::After(n1), UpdateDescription::Range(n2, _)) => UpdateDescription::After(std::cmp::min(*n1, *n2)),
        (UpdateDescription::Range(n1, _), UpdateDescription::After(n2)) => UpdateDescription::After(std::cmp::min(*n1, *n2)),
        (UpdateDescription::Range(n1, n2), UpdateDescription::Range(n3, n4)) => {
            UpdateDescription::Range(std::cmp::min(*n1, *n3), std::cmp::max(*n2, *n4))
        }
    }
}

pub struct ActionResult {
    pub alert: Option<String>,
    pub alert_type: AlertType,
    pub update: UpdateDescription,
    pub action: Option<Rc<dyn Action>>
}

impl ActionResult {

    pub fn empty() -> ActionResult {
        ActionResult {
            alert: None,
            alert_type: AlertType::None,
            update: UpdateDescription::NoUpdate,
            action: None
        }
    }

    pub fn error(s: String) -> ActionResult {
        ActionResult {
            alert: Some(s),
            alert_type: AlertType::Error,
            update: UpdateDescription::NoUpdate,
            action: None
        }
    }

    pub fn warn(s: String) -> ActionResult {
        ActionResult {
            alert: Some(s),
            alert_type: AlertType::Warn,
            update: UpdateDescription::NoUpdate,
            action: None
        }
    }

    pub fn info(s: String) -> ActionResult {
        ActionResult {
            alert: Some(s),
            alert_type: AlertType::Info,
            update: UpdateDescription::NoUpdate,
            action: None
        }
    }

    pub fn no_error(update: UpdateDescription) -> ActionResult {
        ActionResult {
            alert: None,
            alert_type: AlertType::None,
            update,
            action: None
        }
    }

    pub fn set_error(&mut self, s: String) {
        self.alert = Some(s);
        self.alert_type = AlertType::Error;
    }

    pub fn set_info(&mut self, s: String) {
        if self.alert_type < AlertType::Info {
            self.alert_type = AlertType::Info;
            self.alert = Some(s);
        }
    }

    pub fn set_action(&mut self, action: Option<Rc<dyn Action>>) {
        self.action = action;
    }

    pub fn combine(&self, other: ActionResult) -> ActionResult {
        let action: Option<Rc<dyn Action>> = match (&self.action, &other.action) {
            (None, None) => None,
            (Some(a), None) => Some(Rc::clone(&a)),
            (None, Some(a)) => Some(Rc::clone(&a)),
            (Some(a), Some(b)) => Some(Rc::new(CompoundAction { actions: vec![Rc::clone(&a), Rc::clone(&b)]}))
        };
        let (alert, alert_type): (Option<String>, AlertType) = if self.alert_type > other.alert_type {
            (self.alert.clone(), self.alert_type.clone())
        } else {
            (other.alert.clone(), other.alert_type.clone())
        };
        ActionResult {
            // alert: match &self.alert {
            //     None => match &other.alert {
            //         None => None, 
            //         Some(msg) => Some(msg.to_string())
            //     }, 
            //     Some(msg) => Some(msg.to_string())
            // },
            alert,
            alert_type,
            update: combine_update(&self.update, &other.update),
            action: match action {None => None, Some(a) => Some(a)}
        }
    }

}

enum CharacterFormat {
    EndOfFile,
    InvalidAscii
}

struct FormatSummary {
    start: usize,
    span: usize,
    format: CharacterFormat
}

pub struct HexEdit<'a> {
    file_manager: FileManager<'a>,
    x: usize,
    y: usize,
    width: usize,
    height: usize,
    line_length: u8,
    show_filename: bool,
    show_hex: bool,
    show_ascii: bool,
    show_lnum: ShowType,
    invalid_ascii_char: char,
    eof_ascii_char: char,
    eof_hex_char: char,
    separator: String,
    capitalize_hex: bool,
    ignore_case: bool,
    use_regex: bool,
    fill: FillType,
    cursor_pos: usize,
    nibble: Nibble,
    start_line: usize,
    edit_mode: EditMode,
    highlights: Vec::<Highlight>,
    display_data: Vec::<u8>,
    valid_bytes: usize,
    clipboard_registers: [Vec::<u8>; 32], //Needs to be 32 or less for Default::default() to work
    file_spec: Option<FileMap<'a>>,
    current_field: Option<std::ops::Range<BitIndex>>
}

pub fn shift_vector(buffer: &mut Vec<u8>, shift: i8) -> Result<(), String> { // TODO: 8 and -8 cause panic
    if shift < -8 || shift > 8 {
        return Err("Shift value must be less than or equal to 8".to_string())
    }
    let mut curr;
    if shift > 0 {
        let cshift = 8 - shift;
        let mask = 0xff >> cshift;
        let mut prev = buffer[buffer.len() - 1];
        for i in 0..buffer.len() {
            curr = buffer[i];
            buffer[i] = (curr >> shift) + ((prev & mask) << cshift);
            prev = curr;
        }
    } else {
        let shift = -shift;
        let cshift = 8 - shift;
        let mask = 0xff << cshift;
        let mut prev = buffer[0];
        for i in (0..buffer.len()).rev() {
            curr = buffer[i];
            buffer[i] = (curr << shift) + ((prev & mask) >> cshift);
            prev = curr;
        }            
    }

    Ok(())
}

pub fn vector_op<F>(buffer1: &mut Vec<u8>, buffer2: &Vec<u8>, op: F) where F: Fn(u8, u8) -> u8 {
    let mut buff2_iter = buffer2.iter().cycle();
    for i in 0..buffer1.len() {
        buffer1[i] = op(buffer1[i], *buff2_iter.next().unwrap());
    }
}

impl<'a> HexEdit<'a> {

    pub fn new(mut file_manager: FileManager, x: usize, y: usize, width: usize, height: usize, line_length: u8, show_filename: bool, 
                show_hex: bool, show_ascii: bool, invalid_ascii_char: char, separator: String, capitalize_hex: bool) -> HexEdit {

        // let fs = PngFileSpec::new(&mut file_manager);
        // let png_struct = make_png();
        // let fs = FileMap::new(&png_struct);

        HexEdit {
            file_manager,
            x,
            y,
            width,
            height,
            line_length,
            show_filename,
            show_hex,
            show_ascii,
            show_lnum: ShowType::Hex,
            invalid_ascii_char,
            eof_ascii_char: '~',
            eof_hex_char: '~',
            separator,
            capitalize_hex,
            ignore_case: false,
            use_regex: true,
            fill: FillType::Bytes(vec![0]),
            cursor_pos: 0,
            nibble: Nibble::Left,
            start_line: 0,
            edit_mode: EditMode::AsciiOverwrite,
            highlights: vec![],
            display_data: Vec::<u8>::new(),
            valid_bytes: 0,
            clipboard_registers: Default::default(),
            file_spec: None,
            current_field: None
        }
    }

    pub fn is_modified(&self) -> bool {
        self.file_manager.is_modified()
    }

    pub fn filename(&self) -> String {
        self.file_manager.filename.clone()
    }

    pub fn save(&mut self) -> ActionResult {
        match self.file_manager.save() {
            Ok(()) => ActionResult::empty(),
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    pub fn set_block_size(&mut self, block_size: usize) -> ActionResult {
        self.file_manager.set_block_size(block_size);
        ActionResult::empty()
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

    pub fn set_show_lnum(&mut self, show_lnum: ShowType) -> ActionResult {
        self.show_lnum = show_lnum;
        ActionResult::no_error(UpdateDescription::All)
    }

    pub fn set_show_filename(&mut self, show_filename: bool) -> ActionResult {
        self.show_filename = show_filename;
        ActionResult::no_error(UpdateDescription::All)
    }

    pub fn set_ignore_case(&mut self, ignore_case: bool) -> ActionResult {
        self.ignore_case = ignore_case;
        ActionResult::no_error(UpdateDescription::NoUpdate)
    }

    pub fn set_use_regex(&mut self, use_regex: bool) -> ActionResult {
        self.use_regex = use_regex;
        ActionResult::no_error(UpdateDescription::NoUpdate)
    }

    pub fn set_edit_mode(&mut self, edit_mode: EditMode) -> ActionResult {
        self.edit_mode = edit_mode;
        ActionResult::no_error(UpdateDescription::AttrsOnly)
    }

    pub fn set_fill(&mut self, fill: FillType) -> ActionResult {
        self.fill = fill;
        ActionResult::no_error(UpdateDescription::NoUpdate) // TODO: Make this return an action
    }

    pub fn set_syntax(&mut self, syntax: Option<String>) -> ActionResult {
        if let Some(syntax) = syntax {
            let fname = format!("{}-spec.xml", syntax);
            let xml = match std::fs::read_to_string(&fname) {
                Ok(string) => string,
                Err(err) => return ActionResult::error(format!("Could not read file '{}'", fname).to_string())
            };
            let s = Structure::from_xml(&xml).unwrap();
            let mut fs = FileMap::new(Rc::new(s));
            fs.initialize(&mut self.file_manager);
            self.file_spec = Some(fs);
        } else {
            self.file_spec = None;
        }
        
        let mut res = ActionResult::no_error(UpdateDescription::AttrsOnly);
        if let Some(info_text) = self.update_syntax_highlight() {
            res.set_info(info_text);
        }
        res
    }

    pub fn set_file_spec(&mut self, fs: Rc<Structure>) {
        let mut fs = FileMap::new(fs);
        fs.initialize(&mut self.file_manager);
        self.file_spec = Some(fs)
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
        for res in self.file_manager.find(&expr, self.ignore_case, self.use_regex) {
            self.highlights.push(Highlight {start: res.start, span: res.span})
        }
        ActionResult::no_error(UpdateDescription::AttrsOnly)
    }

    pub fn find_bytes(&mut self, bytes: &Vec<u8>) -> ActionResult {
        for res in self.file_manager.find_bytes(&bytes) {
            self.highlights.push(Highlight {start: res.start, span: res.span})
        }
        ActionResult::no_error(UpdateDescription::AttrsOnly)
    }

    pub fn seek_next(&mut self) -> ActionResult {
        let num_highlights = self.highlights.len();

        for (i, h) in self.highlights.iter().enumerate() {
            if h.start > self.cursor_pos {
                let info_result = ActionResult::info(format!("Result {} of {}", i + 1, num_highlights));
                let res = self.set_cursor_pos(h.start); // TODO make this action actually perform a find when redone
                return info_result.combine(res)
            }
        }

        if num_highlights > 0 {
            let warn_result = ActionResult::warn(format!("Wrapped to result {} of {}", 1, num_highlights));
            let mut res = self.set_cursor_pos(self.highlights[0].start); // TODO make this action actually perform a find when redone
            warn_result.combine(res)
        } else {
            ActionResult::error("No results".to_string())
        }
    }

    pub fn seek_prev(&mut self) -> ActionResult {
        let num_highlights = self.highlights.len();

        for (i, h) in self.highlights.iter().enumerate().rev() {
            if h.start < self.cursor_pos {
                let info_result = ActionResult::info(format!("Result {} of {}", i + 1, num_highlights));
                let res = self.set_cursor_pos(h.start); // TODO make this action actually perform a find when redone
                return info_result.combine(res)
            }
        }

        if num_highlights > 0 {
            let warn_result = ActionResult::warn(format!("Wrapped to result {} of {}", num_highlights, num_highlights));
            let mut res = self.set_cursor_pos(self.highlights[self.highlights.len() - 1].start); // TODO make this action actually perform a find when redone
            warn_result.combine(res)
        } else {
            ActionResult::error("No results".to_string())
        }
    }

    pub fn len(&self) -> usize {
        self.file_manager.len()
    }

    pub fn truncate(&mut self, size: usize) -> ActionResult {
        if size < self.len() {
            let mut buffer:Vec<u8> = vec![0; self.len() - size];
            if let Err(msg) = self.file_manager.get_bytes(self.len(), &mut buffer) {
                return ActionResult::error(msg.to_string())
            }
        
            match self.file_manager.truncate(size) {
                Ok(_) => ActionResult {
                    alert: None,
                    alert_type: AlertType::None,
                    update: UpdateDescription::After(size),
                    action: Some(Rc::new(TruncateAction::new(size, buffer)))
                },
                Err(msg) => ActionResult::error(msg.to_string())
            }
        } else {
            ActionResult::error("Cannot truncate to size greater than file length".to_string())
        }
    }

    fn register_len(&self, register: u8) -> usize {
        self.clipboard_registers[register as usize].len()
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
        match &fill {
            FillType::Bytes(bytes) => {
                self.clipboard_registers[register as usize].extend(bytes.to_vec()); // TODO add action to this
            },
            FillType::Register(n) => {
                self.clipboard_registers[register as usize].extend(self.clipboard_registers[*n as usize].to_vec()); // TODO add action to this
            }
        }
        ActionResult {
            alert: None,
            alert_type: AlertType::None,
            update: UpdateDescription::NoUpdate,
            action: Some(Rc::new(ConcatenateRegisterAction::new(register, fill)))
        }
    }

    pub fn swap_register(&mut self, register: u8) -> ActionResult {
        if register >= 32 {
            return ActionResult::error("Register number must be less than 32".to_string())
        }
        self.clipboard_registers[register as usize].reverse();
        ActionResult {
            alert: None,
            alert_type: AlertType::None,
            update: UpdateDescription::NoUpdate,
            action: Some(Rc::new(SwapRegisterAction::new(register)))
        }
    }

    pub fn invert_register(&mut self, register: u8) -> ActionResult {
        if register >= 32 {
            return ActionResult::error("Register number must be less than 32".to_string())
        }
        let new = self.clipboard_registers[register as usize].iter().map(|x| !x).collect();
        self.clipboard_registers[register as usize] = new;
        ActionResult {
            alert: None,
            alert_type: AlertType::None,
            update: UpdateDescription::NoUpdate,
            action: Some(Rc::new(InvertRegisterAction::new(register)))
        }
    }

    pub fn deflate_register(&mut self, register: u8, level: u8) -> ActionResult {
        if register >= 32 {
            return ActionResult::error("Register number must be less than 32".to_string())
        }

        let mut e = DeflateEncoder::new(Vec::new(), Compression::new(level as u32));
        if let Err(msg) = e.write_all(&self.clipboard_registers[register as usize]) {
            ActionResult::error(msg.to_string())
        } else {
            match e.finish() {
                Ok(w) => {
                    self.clipboard_registers[register as usize] = w;
                    ActionResult {
                        alert: None,
                        alert_type: AlertType::None,
                        update: UpdateDescription::NoUpdate,
                        action: Some(Rc::new(DeflateRegisterAction::new(register, level)))
                    }
                },
                Err(msg) => ActionResult::error(msg.to_string())
            }
        }
    }

    pub fn inflate_register(&mut self, register: u8) -> ActionResult {
        if register >= 32 {
            return ActionResult::error("Register number must be less than 32".to_string())
        }

        let mut e = DeflateDecoder::new(Vec::new());
        if let Err(msg) = e.write_all(&self.clipboard_registers[register as usize]) {
            ActionResult::error(msg.to_string())
        } else {
            match e.finish() {
                Ok(w) => {
                    self.clipboard_registers[register as usize] = w;
                    ActionResult::empty() // TODO: Add action to this... It's tough to undo since it doesn't tell you the compression level
                },
                Err(msg) => ActionResult::error(msg.to_string())
            }
        }
    }

    pub fn manipulate_register(&mut self, register: u8, fill: FillType, op: ByteOperation) -> ActionResult {

        if register >= 32 {
            return ActionResult::error("Register number must be less than 32".to_string())
        }
        let original_bytes = self.clipboard_registers[register as usize].to_vec();
        let op_bytes = match &fill {
            FillType::Bytes(bytes) => {
                bytes.to_vec()
            },
            FillType::Register(n) => {
                self.clipboard_registers[*n as usize].to_vec()
            }
        };
        match op {
            ByteOperation::And => vector_op(&mut self.clipboard_registers[register as usize], &op_bytes, |a, b| a & b),
            ByteOperation::Or => vector_op(&mut self.clipboard_registers[register as usize], &op_bytes, |a, b| a | b),
            ByteOperation::Nand => vector_op(&mut self.clipboard_registers[register as usize], &op_bytes, |a, b| !(a & b)),
            ByteOperation::Nor => vector_op(&mut self.clipboard_registers[register as usize], &op_bytes, |a, b| !(a | b)),
            ByteOperation::Xor => vector_op(&mut self.clipboard_registers[register as usize], &op_bytes, |a, b| a ^ b),
            ByteOperation::Xnor => vector_op(&mut self.clipboard_registers[register as usize], &op_bytes, |a, b| !(a ^ b))
        };
        ActionResult {
            alert: None,
            alert_type: AlertType::None,
            update: UpdateDescription::NoUpdate,
            action: Some(Rc::new(RegisterOpAction::new(register, fill, original_bytes, op)))
        }
    }

    pub fn shift_register(&mut self, register: u8, shift: i8) -> ActionResult { // negative is left
        if register >= 32 {
            return ActionResult::error("Register number must be less than 32".to_string())
        }
        match shift_vector(&mut self.clipboard_registers[register as usize], shift) {
            Ok(_) =>  ActionResult {
                alert: None,
                alert_type: AlertType::None,
                update: UpdateDescription::NoUpdate,
                action: Some(Rc::new(ShiftRegisterAction::new(register, shift)))
            },
            Err(msg) => ActionResult::error(msg)
        }

    }

    pub fn slice_register(&mut self, register: u8, n1: usize, n2: usize) -> ActionResult {
        if register >= 32 {
            return ActionResult::error("Register number must be less than 32".to_string())
        }

        if n2 >= self.clipboard_registers[register as usize].len() {
            ActionResult::error("Slice index outside of register contents bounds".to_string())
        } else if n1 > n2 {
            ActionResult::error("Slice start index greater than slice end index".to_string())
        } else {
            let temp = &self.clipboard_registers[register as usize][n1..n2];
            let left = self.clipboard_registers[register as usize][..n1].to_vec();
            let right = self.clipboard_registers[register as usize][n2..].to_vec();
            self.clipboard_registers[register as usize] = temp.to_vec();
            ActionResult {
                alert: None,
                alert_type: AlertType::None,
                update: UpdateDescription::NoUpdate,
                action: Some(Rc::new(SliceRegisterAction::new(register, left, right)))
            }
        }
    }

    pub fn insert(&mut self, data: DataSource) -> ActionResult {
        let mut start_byte = self.cursor_pos;
        let file_end = self.file_manager.len();
        if self.cursor_pos > self.file_manager.len() {
            start_byte = self.file_manager.len();
            if let Err(msg) = self.append_fill(self.cursor_pos - self.file_manager.len()) {
                return ActionResult::error(msg.to_string())
            }
        }
        let bytes = match &data {
            DataSource::Bytes(b) => b.to_vec(),
            DataSource::Fill(n) => self.get_fill_bytes(*n),
            DataSource::Register(n) => self.clipboard_registers[*n as usize].to_vec()
        };
        match self.file_manager.insert_bytes(self.cursor_pos, &bytes) {
            Ok(_) => {
                let seek_res = self.seek(SeekFrom::Current(bytes.len() as i64));
                ActionResult {
                    alert: None,
                    alert_type: AlertType::None,
                    update: combine_update(&UpdateDescription::After(start_byte), &seek_res.update),
                    action: Some(Rc::new(InsertAction::new(data, file_end)))
                }
            },
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    pub fn overwrite(&mut self, data: DataSource) -> ActionResult {
        let mut start_byte = self.cursor_pos;
        let file_end = self.file_manager.len();
        let bytes = match &data {
            DataSource::Bytes(b) => b.to_vec(),
            DataSource::Fill(n) => self.get_fill_bytes(*n),
            DataSource::Register(n) => self.clipboard_registers[*n as usize].to_vec()
        };
        if self.cursor_pos + bytes.len() > self.file_manager.len() {
            start_byte = self.file_manager.len();
            if let Err(msg) = self.append_fill(self.cursor_pos + bytes.len() - self.file_manager.len()) {
                return ActionResult::error(msg.to_string())
            }
        }
        let mut original_bytes: Vec<u8> = vec![0; bytes.len()];
        if let Err(msg) = self.file_manager.get_bytes(self.cursor_pos, &mut original_bytes) {
            return ActionResult::error(msg.to_string()) // TODO: This doesn't account for the changes to the file made earlier
        }
        match self.file_manager.overwrite_bytes(self.cursor_pos, &bytes) {
            Ok(_) => {
                let seek_res = self.seek(SeekFrom::Current(bytes.len() as i64));
                ActionResult {
                    alert: None,
                    alert_type: AlertType::None,
                    update: combine_update(&UpdateDescription::Range(start_byte, self.cursor_pos + bytes.len()), &seek_res.update),
                    action: Some(Rc::new(OverwriteAction::new(data, original_bytes, file_end)))
                }
            },
            Err(msg) => ActionResult::error(msg.to_string())
        }
    }

    pub fn get_register(&mut self, register: u8) -> Result<Vec<u8>, String>{
        if register < 32 {
            Ok(self.clipboard_registers[register as usize].to_vec())
        } else {
            Err("Registers must be less than 32".to_string())
        }
    }

    pub fn set_register(&mut self, register: u8, bytes: &Vec<u8>) -> ActionResult {
        if register < 32 {
            let original_bytes = self.clipboard_registers[register as usize].to_vec();
            self.clipboard_registers[register as usize] = bytes.to_vec();
            ActionResult {
                alert: None,
                alert_type: AlertType::None,
                update: UpdateDescription::NoUpdate,
                action: Some(Rc::new(SetRegisterAction::new(register, bytes.to_vec(), original_bytes)))
            }
        } else {
            ActionResult::error("Registers must be less than 32".to_string())
        }
    }

    pub fn yank(&mut self, register: u8, n_bytes: usize) -> ActionResult {
        let index = self.cursor_pos;
        if register < 32 {
            if index + n_bytes <= self.file_manager.len() {
                let mut buffer = vec![0; n_bytes];
                match self.file_manager.get_bytes(index, &mut buffer) {
                    Ok(_) => {
                        let original_bytes = self.clipboard_registers[register as usize].to_vec();
                        self.clipboard_registers[register as usize] = buffer;
                        ActionResult {
                            alert: None,
                            alert_type: AlertType::None,
                            update: UpdateDescription::NoUpdate,
                            action: Some(Rc::new(YankAction::new(register, n_bytes, original_bytes)))
                        }
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

    pub fn delete_bytes(&mut self, n_bytes: usize) -> ActionResult {
        let index = self.cursor_pos;
        if index + n_bytes <= self.file_manager.len() {
            let mut original_bytes: Vec<u8> = vec![0; n_bytes];
            if let Err(msg) = self.file_manager.get_bytes(index, &mut original_bytes) {
                return ActionResult::error(msg.to_string())
            }
            match self.file_manager.delete_bytes(index, n_bytes) {
                Ok(_) => ActionResult {
                    alert: None,
                    alert_type: AlertType::None,
                    update: UpdateDescription::After(index),
                    action: Some(Rc::new(DeleteAction::new(original_bytes)))
                },
                Err(msg) => ActionResult::error(msg.to_string())
            }
        } else {
            ActionResult::error("Cannot delete past EOF".to_string())
        }
    }

    pub fn swap_bytes(&mut self, n_bytes: usize) -> ActionResult {
        let index = self.cursor_pos;
        if index + n_bytes <= self.file_manager.len() {
            let mut data = vec![0; n_bytes];
            match self.file_manager.get_bytes(index, &mut data) {
                Ok(_) => {
                    data.reverse();
                    match self.file_manager.overwrite_bytes(index, &data) {
                        Ok(_) => ActionResult {
                            alert: None,
                            alert_type: AlertType::None,
                            update: UpdateDescription::Range(index, index + n_bytes), // TODO: add action here
                            action: Some(Rc::new(SwapAction::new(n_bytes)))
                        },
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

    pub fn resize(&mut self, width: usize, height: usize) {
        self.width = width;
        self.height = height;
        //self.refresh_viewport()
    }

    pub fn content_height(&self) -> usize {
        if self.show_filename {
            self.height - 1
        } else {
            self.height
        }
    }

    pub fn top_margin(&self) -> usize {
        if self.show_filename {
            1
        } else {
            0
        }
    }

    fn bytes_to_hex_line(buffer: &Vec<u8>, eof_char: char, n_valid: usize, format_summary: &mut Vec<FormatSummary>) -> String {
        let mut bytes = Vec::<String>::new();
        for (i, b) in buffer.iter().enumerate() {
            if i < n_valid {
                bytes.push(format!("{:02x}", b).to_string());
            } else {
                bytes.push(eof_char.to_string().repeat(2));
                format_summary.push(FormatSummary{start: i*3, span: 2, format: CharacterFormat::EndOfFile});
            }
        }

        bytes.join(" ")
        // buffer.iter().enumerate()
        // .map(|(i, b)| 
        //     if i < n_valid {
        //         format!("{:02x}", b).to_string()
        //     } else {
        //         eof_char.to_string().repeat(2)
        //     })
        // .collect::<Vec<String>>()
        // .join(" ")
    }
    
    fn bytes_to_ascii_line(buffer: &Vec<u8>, invalid_char: char, eof_char: char, n_valid: usize, offset: usize, format_summary: &mut Vec<FormatSummary>) -> String {
        let mut bytes = Vec::<String>::new();
        for (i, b) in buffer.iter().enumerate() {
            if i < n_valid {
                bytes.push(match b {
                    32..=126 => (*b as char).to_string(),
                    _ => {
                        format_summary.push(FormatSummary{start: i + offset, span: 1, format: CharacterFormat::InvalidAscii});
                        invalid_char.to_string()
                    }
                });
            } else {
                bytes.push(eof_char.to_string());
                format_summary.push(FormatSummary{start: i + offset, span: 1, format: CharacterFormat::EndOfFile});
            }
        }

        bytes.join("")
        // buffer.iter().enumerate()
        // .map(|(i, b)| 
        //     if i < n_valid {
        //         format!("{}", match b {32..=126 => *b as char, _ => invalid_char}).to_string()
        //     } else {
        //         eof_char.to_string()
        //     })
        // .collect::<Vec<String>>()
        // .join("")
    }
    
    fn read_to_composite_line(&self, buffer: &Vec<u8>, n_valid: usize, format_summary: &mut Vec<FormatSummary>) -> String {
        let mut s: String = "".to_string();
        if self.show_hex {
            if self.capitalize_hex {
                s += &HexEdit::bytes_to_hex_line(&buffer, self.eof_hex_char, n_valid, format_summary).to_uppercase();
            } else {
                s += &HexEdit::bytes_to_hex_line(&buffer, self.eof_hex_char, n_valid, format_summary).to_lowercase();
            }
            if self.show_ascii {
                s += &self.separator;
            }
        } 

        if self.show_ascii {
            let offset = s.len();
            s += &HexEdit::bytes_to_ascii_line(&buffer, self.invalid_ascii_char, self.eof_ascii_char, n_valid, offset, format_summary)
        }

        s
    }

    pub fn set_viewport_row(&mut self, row: usize) -> std::io::Result<()> {
        self.start_line = row;
        self.refresh_viewport()
    }

    fn refresh_viewport(&mut self) -> std::io::Result<()> {
        // TODO: Make this actually update a given range of data
        self.display_data = vec![0; (self.line_length as usize) * (self.content_height())];
        match self.file_manager.get_bytes(self.start_line * (self.line_length as usize), &mut self.display_data) {
            Ok(n) => {
                self.valid_bytes = n;
                Ok(())
            },
            Err(msg) => Err(msg)
        }
    }

    fn get_display_line(&self, line: usize) -> Vec<u8>{
        self.display_data[line*(self.line_length as usize)..(line + 1)*(self.line_length as usize)].to_vec()
    }

    fn append_bytes(&mut self, bytes: Vec<u8>) -> ActionResult {
        let n = self.len();
        match self.file_manager.append_bytes(&bytes) {
            Ok(()) => ActionResult::no_error(UpdateDescription::After(n)), // TODO: Add action here?
            Err(msg) => ActionResult::error(msg.to_string())
        }
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
                //self.action_stack.add(Rc::new(OverwriteBytesAction::new(index, vec![c])));
                ActionResult::no_error(UpdateDescription::Range(start_byte, index + 1)) // TODO: add action here
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
                //self.action_stack.add(Rc::new(InsertBytesAction::new(index, 1)));
                ActionResult::no_error(UpdateDescription::After(start_byte)) // TODO: add action here
            },
            Err(msg) => ActionResult::error(msg.to_string())
        }
        
    }

    fn delete_byte(&mut self, index: usize) -> ActionResult {
        match self.file_manager.delete_byte(index) {
            Ok(_) => ActionResult::no_error(UpdateDescription::After(index)), // TODO: add action here
            Err(msg) => ActionResult::error(msg.to_string())
        }
        //TODO: Add action record
    }

    pub fn seek(&mut self, seek: SeekFrom) -> ActionResult {
        let current_pos = self.cursor_pos;
        let mut res = match seek {
            SeekFrom::Start(index) => {
                self.set_cursor_pos(index as usize)
            },
            SeekFrom::End(index) => {
                self.set_cursor_pos(self.len() - (index as usize))
            },
            SeekFrom::Current(index) => {
                self.set_cursor_pos((current_pos as isize + (index as isize)) as usize)
            }
        };
        
        res.set_action(Some(Rc::new(SeekAction::new(current_pos, seek))));
        res
    }

    fn update_syntax_highlight(&mut self) -> Option<String> {
        if let Some(ref mut fs) = &mut self.file_spec {
            // self.current_field = fs.field_at(self.cursor_pos, &mut self.file_manager);
            if self.cursor_pos < self.file_manager.len() {
                let region = fs.region_at(BitIndex::bytes(self.cursor_pos), &mut self.file_manager);

                match region {
                    FileRegion::Field(field_id) => {
                        let s_name = fs.structure_name(field_id.structure.clone());
                        let field = fs.get_field(field_id.clone(), &mut self.file_manager);
                        self.current_field = Some(field.start..(field.start + field.span));
                        let value = field.parse(&mut self.file_manager).unwrap();
                        let field_data = fs.format_field_data(field_id, value);
                        let field_info = format!("[{}] {}: {}", s_name, field.name, field_data).to_string();
                        let mut field_info = field_info.replace("\x00", "\\x00");
                        // let field_info = match value {
                        //     ExprValue::Integer(v) => format!("[{}] {}: {}", s_name, field.name, v).to_string(),
                        //     ExprValue::String(v) => format!("[{}] {}: {}", s_name, field.name, v).to_string(),
                        //     _ => todo!()
                        // };
                        // res.set_info(field_info);
                        Some(field_info)
                        
                    },
                    FileRegion::Spare(struct_id, rng) => {
                        self.current_field = Some(rng);
                        let s_name = fs.structure_name(struct_id);
                        Some(format!("[{}] Spare", s_name).to_string())
                    }
                    _ => todo!()
                }
            } else {
                self.current_field = None;
                None
            }
        } else {
            self.current_field = None;
            None
        }
    }

    pub fn set_cursor_pos(&mut self, index: usize) -> ActionResult {
        let line = index / (self.line_length as usize);
        let action = SeekAction::new(self.cursor_pos, SeekFrom::Start(index as u64));
        self.cursor_pos = index;

        //println!("line={} start_line={} content_height={} height={}", line, self.start_line, self.content_height(), self.height);
        let mut res = if line < self.start_line {
            match self.set_viewport_row(line){
                Ok(_) => ActionResult::no_error(UpdateDescription::All), // TODO: add action here
                Err(msg) => ActionResult::error(msg.to_string())
            }
        } else if line >= self.start_line as usize + self.content_height() {
            //println!("Going down: {} {}", self.start_line, line - self.content_height() + 1);
            match self.set_viewport_row(line - self.content_height() + 1) {
                Ok(_) => ActionResult::no_error(UpdateDescription::All),
                Err(msg) => ActionResult::error(msg.to_string())
            }
        } else {
            ActionResult::no_error(UpdateDescription::AttrsOnly)
        };

        if let Some(info_text) = self.update_syntax_highlight() {
            res.set_info(info_text);
        }

        res.set_action(Some(Rc::new(action)));
        
        res
    }

    fn advance_cursor(&mut self) -> ActionResult {
        match self.edit_mode {
            EditMode::HexOverwrite | EditMode::HexInsert => {
                match self.nibble {
                    Nibble::Left => {
                        self.nibble = Nibble::Right;
                        ActionResult::no_error(UpdateDescription::AttrsOnly) // TODO: add action here
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
                        ActionResult::no_error(UpdateDescription::AttrsOnly) // TODO: add action here
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
        let offset = self.line_label_len() + self.separator.len();
        (self.x + (self.cursor_pos % (self.line_length as usize))*3 + offset + match self.nibble {Nibble::Left => 0, Nibble::Right => 1}) as i32
    }

    fn ascii_cursor_x(&self) -> i32 {
        let offset = self.line_label_len() + self.separator.len() * 2 + (self.line_length as usize) * 3 - 1;
        (self.x + (self.cursor_pos % (self.line_length as usize)) + offset) as i32
    }

    fn cursor_y(&self) -> i32 {
        (self.top_margin() + self.y + self.cursor_pos / (self.line_length as usize) - self.start_line) as i32
    }

    pub fn refresh_cursor(&self, window: &Window) {
        //println!("Refreshing Cursor!");

        // start_color();
        // init_pair(4, pancurses::COLOR_BLACK, pancurses::COLOR_WHITE);

        let mut underline_attr = Attributes::new();
        //underline_attr.set_underline(true);
        underline_attr.set_color_pair(pancurses::ColorPair(globals::CURSOR_COLOR));
        let underline_attr = chtype::from(underline_attr);

        match self.edit_mode {
            EditMode::HexOverwrite | EditMode::HexInsert => {
                window.mvchgat(self.cursor_y(), self.ascii_cursor_x(), 1, underline_attr, 0);

                window.mv(self.cursor_y(), self.hex_cursor_x());

            },
            EditMode::AsciiOverwrite | EditMode::AsciiInsert => {
                window.mvchgat(self.cursor_y(), self.hex_cursor_x(), 2, underline_attr, 0);

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
                    self.seek(SeekFrom::Current(-(self.line_length as i64)))
                } else {
                    ActionResult::empty()
                }
            },
            Input::KeyDown => {
                self.seek(SeekFrom::Current(self.line_length as i64))
            },
            Input::KeyNPage => { // TODO: Return action for this
                let row = self.start_line;
                self.set_cursor_pos(self.cursor_pos + self.content_height()*(self.line_length as usize));
                match self.set_viewport_row(row + self.content_height()) {
                    Ok(_) => ActionResult::no_error(UpdateDescription::All),
                    Err(msg) => ActionResult::error(msg.to_string())
                }
                
            },
            Input::KeyPPage => { // TODO: Return action for this
                let row = self.start_line;
                if self.cursor_pos >= self.content_height()*(self.line_length as usize) && self.start_line >= self.content_height() {
                    self.set_cursor_pos(self.cursor_pos - self.content_height()*(self.line_length as usize));
                    if let Err(msg) = self.set_viewport_row(row - self.content_height()) {
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
                self.seek(SeekFrom::Start(0))
            },
            Input::KeyEnd => {
                self.seek(SeekFrom::End(0))
            },
            Input::Character(ch) => { // TODO: Return action for this
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
                            let c = match self.get_byte(self.cursor_pos) {Some(x) => x, None => 0};
                            return self.overwrite_byte(self.cursor_pos, c & 0xf0);
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
                // Does the same thing as Key_Left in overwrite mode
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

    fn refresh_filename(&self, window: &mut Window) {
        if self.show_filename {
            let fname_len = self.file_manager.filename.len();
            //let filename = self.file_manager.filename.clone().push_str(" ".repeat(self.width - fname_len));
            let filename = format!("{:0width$}", self.file_manager.filename, width=self.width);
            window.mvaddstr(0, 0, &filename);
        }
    }

    fn refresh_highlights(&self, window: &mut Window) {
        // println!("Refreshing Highlights: {}", self.highlights.len());

        // start_color();
        

        // TODO: clean this up!!!
        let mut rv_attr = Attributes::new();
        //rv_attr.set_reverse(true);
        rv_attr.set_color_pair(pancurses::ColorPair(globals::HIGHLIGHT_COLOR));
        let rv_attr = chtype::from(rv_attr);

        

        let start: usize = self.start_line * (self.line_length as usize);
        let end: usize = start + self.content_height() * (self.line_length as usize);

        let hex_offset = self.line_label_len() + self.separator.len();
        let ascii_offset = self.line_label_len() + self.separator.len() * 2 + (self.line_length as usize) * 3 - 1;

        let top_margin = self.top_margin() as i32;

        // TODO: Merge this processing with the below processing

        let mut field_attr = Attributes::new();
        //rv_attr.set_reverse(true);
        field_attr.set_color_pair(pancurses::ColorPair(globals::CURRENT_FIELD_COLOR));
        let field_attr = chtype::from(field_attr);

        if let Some(h) = &self.current_field {
            let h_end = h.end.byte();
            let h_start = h.start.byte();
            if h_end > start && h_start < end { // If the highlight is in the viewport
                let first_line = (h_start / (self.line_length as usize)) as isize - (self.start_line as isize);
                let last_line = ((h_end) / (self.line_length as usize)) as isize - (self.start_line as isize);
                // if (h_start + h.span) % (self.line_length as usize) == 0 {
                //     last_line -= 1;
                // }
                //println!("In view: {}, {}", first_line, last_line);
                for y in std::cmp::max(first_line, 0)..std::cmp::min(last_line, self.content_height() as isize - 1) + 1 {
                    let mut x1 = 0;
                    let mut x2 = self.line_length as usize;
                    if y == first_line {
                        x1 = h_start % (self.line_length as usize);
                    }
                    if y == last_line {
                        x2 = (h_end) % (self.line_length as usize);
                    }
                    // println!("y={}, x1={}, x2={}, h_start={}, h.span={}, line_length={}", y, x1, x2, h_start, h.span, self.line_length);
                    if x1 != x2 {
                        window.mvchgat(top_margin + y as i32, (x1*3 + hex_offset) as i32, ((x2 - x1)*3 - 1) as i32, field_attr, 0);
                        //window.mvchgat(top_margin + y as i32, (x1 + ascii_offset) as i32, (x2 - x1) as i32, field_attr, 0);
                    }
                }
            }
        }

        // This is the below processing

        for h in self.highlights.iter() {
            //println!("{}, {}", h.start, h.span);
            if h.start + h.span > start && h.start < end { // If the highlight is in the viewport
                let first_line = (h.start / (self.line_length as usize)) as isize - (self.start_line as isize);
                let last_line = ((h.start + h.span) / (self.line_length as usize)) as isize - (self.start_line as isize);
                // if (h.start + h.span) % (self.line_length as usize) == 0 {
                //     last_line -= 1;
                // }
                //println!("In view: {}, {}", first_line, last_line);
                for y in std::cmp::max(first_line, 0)..std::cmp::min(last_line, self.content_height() as isize - 1) + 1 {
                    let mut x1 = 0;
                    let mut x2 = self.line_length as usize;
                    if y == first_line {
                        x1 = h.start % (self.line_length as usize);
                    }
                    if y == last_line {
                        x2 = (h.start + h.span) % (self.line_length as usize);
                    }
                    // println!("y={}, x1={}, x2={}, h.start={}, h.span={}, line_length={}", y, x1, x2, h.start, h.span, self.line_length);
                    if x1 != x2 {
                        window.mvchgat(top_margin + y as i32, (x1*3 + hex_offset) as i32, ((x2 - x1)*3 - 1) as i32, rv_attr, 0);
                        window.mvchgat(top_margin + y as i32, (x1 + ascii_offset) as i32, (x2 - x1) as i32, rv_attr, 0);
                    }
                }
            }
        }
    }

    pub fn update(&mut self, window: &mut Window, update: UpdateDescription) -> std::io::Result<()> {
        // start_color();
        // init_pair(3, pancurses::COLOR_BLACK, pancurses::COLOR_YELLOW);
        // init_pair(4, pancurses::COLOR_BLACK, pancurses::COLOR_WHITE);
        match update {
            UpdateDescription::NoUpdate => {
                ()
            },
            UpdateDescription::AttrsOnly => {
                // println!("Refreshing attrs");
                self.populate(window, 0, self.content_height()); //TODO: Get rid of this! It's only needed to refresh the attributes.
                self.refresh_highlights(window);
                self.refresh_cursor(window);
            },
            UpdateDescription::After(n) =>{
                let line = clip_to_range(n / (self.line_length as usize), self.start_line, self.start_line + self.content_height());
                //println!("Refreshing lines {} through {}", line - self.start_line, self.content_height());
                self.refresh_viewport()?;
                self.populate(window, line - self.start_line, self.content_height());
                self.refresh_highlights(window);
                self.refresh_cursor(window);
            },
            UpdateDescription::Range(n1, n2) => {
                let line1 = clip_to_range(n1 / (self.line_length as usize), self.start_line, self.start_line + self.content_height());
                let line2 = clip_to_range((n2 - 1) / (self.line_length as usize), self.start_line, self.start_line + self.content_height());

                //println!("Refreshing lines {} through {}", line1 - self.start_line, line2 - self.start_line);
                self.refresh_viewport()?;
                self.populate(window, line1 - self.start_line, line2 - self.start_line + 1);
                self.refresh_highlights(window);
                self.refresh_cursor(window);
            },
            UpdateDescription::All => {
                // println!("Refreshing all");
                self.refresh_viewport()?;
                self.populate(window, 0, self.content_height());
                self.refresh_filename(window);
                self.refresh_highlights(window);
                self.refresh_cursor(window);               
            }

        }

        Ok(())
    }

    fn line_label_len(&self) -> usize {
        match self.show_lnum {
            ShowType::Off => 0,
            ShowType::Dec => format!("{}", self.len()).len(),
            ShowType::Hex => format!("{:x}", self.len()).len()
        }
    }

    fn format_line_label(&self, lnum: usize) -> String {
        match self.show_lnum {
            ShowType::Off => "".to_string(),
            ShowType::Dec => format!("{:0width$}", lnum, width=self.line_label_len()),
            ShowType::Hex => match self.capitalize_hex {
                true => format!("{:0width$x}", lnum, width=self.line_label_len()).to_ascii_uppercase(),
                false => format!("{:0width$x}", lnum, width=self.line_label_len())
            }
        }
    }

    fn populate(&self, window: &mut Window, start: usize, end: usize) {

        let mut eof_attr = Attributes::new();
        //rv_attr.set_reverse(true);
        eof_attr.set_color_pair(pancurses::ColorPair(globals::EOF_COLOR));
        let eof_attr = chtype::from(eof_attr);

        let mut invalid_ascii_attr = Attributes::new();
        //rv_attr.set_reverse(true);
        invalid_ascii_attr.set_color_pair(pancurses::ColorPair(globals::BAD_ASCII_COLOR));
        let invalid_ascii_attr = chtype::from(invalid_ascii_attr);

        let start = 0; //TODO: GET RID OF THIS!!!
        let end = self.content_height();

        let mut format_summaries = Vec::<Vec<FormatSummary>>::new();

        let mut s = String::new();
        for i in start..end {
            //println!("{}", i);
            let mut format_summary = Vec::<FormatSummary>::new();
            s += &self.format_line_label((self.start_line + i) * (self.line_length as usize));
            s += &self.separator;
            if self.valid_bytes > i * (self.line_length as usize) {
                s += &self.read_to_composite_line(&self.get_display_line(i), self.valid_bytes - i * (self.line_length as usize), &mut format_summary);
            } else {
                s += &self.read_to_composite_line(&self.get_display_line(i), 0, &mut format_summary);
            }

            format_summaries.push(format_summary);
            
            s += &"\n".to_string();
        }

        let top_margin = self.top_margin();
        window.mvaddstr((self.y + top_margin) as i32, self.x as i32, s);

        let x_offset = self.line_label_len() + self.separator.len();

        for (i, format_summary) in format_summaries.iter().enumerate() {
            for f in format_summary {
                match f.format {
                    CharacterFormat::EndOfFile => {
                        window.mvchgat((top_margin + i) as i32, (x_offset + f.start) as i32, f.span as i32, eof_attr, 0);
                    },
                    CharacterFormat::InvalidAscii => {
                        window.mvchgat((top_margin + i) as i32, (x_offset + f.start) as i32, f.span as i32, invalid_ascii_attr, 0);
                    }
                }
                
            }
        }
    }

    pub fn draw(&self, window: &mut Window) {

        self.populate(window, 0, self.content_height());
        self.refresh_filename(window);
        self.refresh_highlights(window);
        self.refresh_cursor(window);
    }

}