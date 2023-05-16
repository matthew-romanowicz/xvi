use pancurses::{Input, Window, chtype, Attributes, start_color, init_pair};

pub struct LineEntry {
    length: usize,
    pos_x: usize,
    pos_y: usize,
    text: Vec::<char>,
    cursor_pos: usize,
    offset: usize,
    alert_text: Vec<char>,
    alert_active: bool
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
            alert_text: Vec::<char>::new(),
            alert_active: false
        }
    }

    pub fn get_text(&self) -> Vec::<char> {
        self.text.to_vec()
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
            Input::Character(c) => {
                self.text.insert(self.cursor_pos + self.offset, c);
                self.advance_cursor();
            },
            input => { println!("Input: {:?}", input); },
        }
    }

    pub fn draw(&self, window: &mut Window) {


        start_color();
        init_pair(2, pancurses::COLOR_RED, pancurses::COLOR_BLACK);

        if self.alert_active {
            let mut bold_attr = Attributes::new();
            //bold_attr.set_bold(true);
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