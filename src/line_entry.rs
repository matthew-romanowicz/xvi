use pancurses::{Input, Window, chtype, Attributes, start_color, init_pair};

pub struct LineEntry {
    length: usize,
    pos_x: usize,
    pos_y: usize,
    text: Vec::<char>,
    cursor_pos: usize,
    offset: usize,
    path_autocomplete: bool,
    autocomplete_options: Option<Vec<String>>,
    autocomplete_index: usize,
    autocomplete_range: (usize, usize),
    original_text: Vec<char>,
    alert_text: Vec<char>,
    alert_active: bool
}

fn get_word_range(text: &Vec<char>, index: usize) -> Option<(usize, usize)> {
    let mut in_quotes: bool = false;
    let mut word_start: usize = 0;
    for (i, c) in text.iter().enumerate() {
        match c {
            ' ' if !in_quotes => {
                if i >= index {
                    return Some((word_start, i))
                } else {
                    word_start = i + 1;
                }
            },
            '"' => {
                in_quotes = !in_quotes;
            },
            _ => ()
        }
    }

    if text.len() >= index {
        Some((word_start, text.len()))
    } else {
        None
    }
}

fn autocomplete_path(text: &String) -> Vec<String> {
    let mut res = Vec::<String>::new();

    let mut path = std::path::Path::new(text);
    let mut fname = "";

    match path.file_name() {
        Some(f) => {
            if !(text.ends_with(r"\") || text.ends_with(r"/")) {
                fname = f.to_str().unwrap();
                //println!("{}", fname);
                match path.parent() {
                    Some(p) => {
                        path = p;
                    },
                    None => return res
                }
            }
        },
        None => return res
    }

    match std::fs::read_dir(path) {
        Ok(reader) => {
            for subpath in reader {
                if let Ok(entry) = subpath {
                    if let Some(fname2) = entry.path().file_name() {
                        if fname2.to_str().unwrap().starts_with(fname) {
                            res.push(entry.path().display().to_string());
                        }
                    }
                }
            }
            res
        },
        _ => res
    }

}


impl LineEntry {

    pub fn new(length: usize, pos_x: usize, pos_y:usize) -> LineEntry {
        LineEntry {
            length,
            pos_x,
            pos_y,
            text: Vec::<char>::new(),
            cursor_pos: 0,
            offset: 0,
            path_autocomplete: true,
            autocomplete_options: None,
            autocomplete_index: 0,
            autocomplete_range: (0, 0),
            original_text: Vec::<char>::new(),
            alert_text: Vec::<char>::new(),
            alert_active: false
        }
    }

    pub fn get_text(&self) -> Vec::<char> {
        self.text.to_vec()
    }

    pub fn set_text(&mut self, text: Vec<char>) {
        self.text = text;
        if self.text.len() > self.length {
            self.offset = self.text.len() - self.length;
            self.cursor_pos = self.length;
        } else {
            self.offset = 0;
            self.cursor_pos = self.text.len();
        }
    }

    pub fn clear(&mut self) {
        self.text = Vec::<char>::new();
        self.cursor_pos = 0;
        self.offset = 0;
    }

    pub fn alert(&mut self, text: Vec<char>) {
        self.alert_text = text;
        self.alert_active = true;
    }

    pub fn unalert(&mut self) {
        self.alert_active = false;
    }

    pub fn alerting(&self) -> bool {
        self.alert_active
    }

    pub fn reset_geometry(&mut self, length: usize, pos_x: usize, pos_y: usize) {
        self.pos_x = pos_x;
        self.pos_y = pos_y;
        self.length = length;
        // TODO: finish this up
    }

    fn advance_cursor(&mut self) {
        if self.cursor_pos + self.offset >= self.text.len() {
            ();
        } else if self.cursor_pos + 1 >= self.length {
            self.offset += 1;
        } else {
            self.cursor_pos += 1
        };
    }

    fn retreat_cursor(&mut self) {
        if self.cursor_pos <= 0 {
            if self.offset > 0 {
                self.offset -= 1;
            };
        } else {
            self.cursor_pos -= 1;
        };
    }

    pub fn addch(&mut self, ch: Input) {
        //println!("cursor_pos={}, offset={}, text.len={}, length={}", self.cursor_pos, self.offset, self.text.len(), self.length);
        self.alert_active = false;
        if !matches!(ch, Input::Character('\t')) {
            self.autocomplete_options = None;
        }
        match ch {
            Input::KeyLeft => {
                self.retreat_cursor();
            },
            Input::KeyRight => {
                self.advance_cursor();
            },
            Input::KeyDC => {
                if self.cursor_pos + self.offset < self.text.len() {
                    self.text.remove(self.cursor_pos + self.offset);

                    if self.length >= self.text.len() {
                        //println!("Case 1");
                        self.cursor_pos += self.offset;
                        self.offset = 0;
                    } else if self.offset + self.length > self.text.len() { 
                        // If the end of the text is before the end of the edit, adjust the offset so the ends line up

                        // hello world!     =>  hello orld!   
                        //     __^_______   =>   _____^____   

                        // hello world       =>  hello wold
                        //    _____^__       =>    ______^_

                        
                        //println!("Case 2");
                        let diff = self.offset - (self.text.len() - self.length);
                        self.offset -= diff;
                        self.cursor_pos += diff;
                        //println!(">> cursor_pos={}, offset={}, text.len={}, length={}", self.cursor_pos, self.offset, self.text.len(), self.length);
                    //} else {
                    //    println!("Case 3");
                        // hello world!!!    =>  hello orld!!!
                        //  _____^___        =>   _____^___
                    }
                }
            },
            Input::KeyHome => {
                self.offset = 0;
                self.cursor_pos = 0;
            },
            Input::KeyEnd => {
                if self.text.len() >= self.length {
                    self.cursor_pos = self.length;
                    self.offset = self.text.len() - self.length;
                } else {
                    self.offset = 0;
                    self.cursor_pos = self.text.len();
                }
            }
            Input::Character('\u{08}') => { // Backspace
                if (self.cursor_pos + self.offset <= self.text.len() + 1) && (self.cursor_pos + self.offset > 0) {
                    self.text.remove(self.cursor_pos - 1 + self.offset);
                    self.retreat_cursor();
                    if self.length >= self.text.len() {
                        //println!("Case 1");
                        self.cursor_pos += self.offset;
                        self.offset = 0;
                    } else if self.offset + self.length > self.text.len() { 
                        // If the end of the text is before the end of the edit, adjust the offset so the ends line up

                        // hello world!     =>  hello orld!   
                        //     __^_______   =>   _____^____   

                        // hello world       =>  hello wold
                        //    _____^__       =>    ______^_

                        
                        //println!("Case 2");
                        let diff = self.offset - (self.text.len() - self.length);
                        self.offset -= diff;
                        self.cursor_pos += diff;
                    }
                }
            },
            Input::Character('\t') => {
                match &self.autocomplete_options {
                    Some(v) => {
                        self.autocomplete_index = (self.autocomplete_index + 1) % v.len();
                        let s = &v[self.autocomplete_index];
                        self.text = self.original_text[..self.autocomplete_range.0].to_vec();
                        self.text.extend(&s.chars().collect::<Vec<char>>());
                        self.text.extend(&self.original_text[self.autocomplete_range.1..]);
                        self.cursor_pos = self.autocomplete_range.0 + s.len() - self.offset; // TODO: Make this more safe

                    },
                    None => {
                        match get_word_range(&self.text, self.cursor_pos + self.offset) {
                            Some((a, b)) => {
                                self.autocomplete_options = Some(autocomplete_path(&self.text[a..b].iter().collect::<String>()));
                                if let Some(v) = &self.autocomplete_options {
                                    if v.len() == 0 {
                                        self.autocomplete_options = None;
                                    } else {
                                        self.original_text = self.text.to_vec();
                                        self.autocomplete_range = (a, b);
                                        self.autocomplete_index = 0;
                                        let s = &v[self.autocomplete_index];
                                        self.text = self.original_text[..self.autocomplete_range.0].to_vec();
                                        self.text.extend(&s.chars().collect::<Vec<char>>());
                                        self.text.extend(&self.original_text[self.autocomplete_range.1..]);
                                        self.cursor_pos = self.autocomplete_range.0 + s.len() - self.offset; // TODO: Make this more safe
                                    }
                                }

                            },
                            None => ()//println!("Nothing found")
                        }
                    }
                }

            },
            Input::Character(c) => {
                self.text.insert(self.cursor_pos + self.offset, c);
                self.advance_cursor();
            },
            input => ()//{ println!("Input: {:?}", input); },
        }
    }

    pub fn draw(&self, window: &mut Window) {


        start_color();
        init_pair(2, pancurses::COLOR_RED, pancurses::COLOR_BLACK);

        if self.alert_active {
            let mut bold_attr = Attributes::new();
            bold_attr.set_bold(true);
            bold_attr.set_color_pair(pancurses::ColorPair(2));
            let bold_attr = chtype::from(bold_attr);

            window.mvaddstr(self.pos_y as i32, self.pos_x as i32, &self.alert_text.iter().collect::<String>());
            window.mvchgat(self.pos_y as i32, self.pos_x as i32, self.alert_text.len() as i32, bold_attr, 0);
            window.mv(self.pos_y as i32, self.pos_x as i32);
        } else {
            let s: String;
            if self.text.len() < self.length {
                s = String::from_iter(&self.text) + &" ".repeat(self.length as usize - self.text.len());
            } else if self.text.len() >= self.offset + self.length {
                //println!("offset={}", self.offset);
                s = String::from_iter(&self.text[self.offset..(self.offset + self.length)]);
                //println!("string={}", s);
            } else {
                s = String::from_iter(&self.text[self.offset..(self.offset + self.length - 1)]) + &" ";
            }
            window.mvaddstr(self.pos_y as i32, self.pos_x as i32, &s);
            //window.mv(y, x);
            window.mv(self.pos_y as i32, (self.pos_x + self.cursor_pos) as i32);
        }
    }

}