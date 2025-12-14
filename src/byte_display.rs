use pancurses::{Input, Window, chtype, Attributes, start_color, init_pair};

pub struct ByteDisplay {
    length: usize,
    pos_x: usize,
    pos_y: usize,
    byte: u8,
    labels: Vec<(u8, String)>
}

impl ByteDisplay {
    pub fn new(pos_x: usize, pos_y: usize) -> ByteDisplay {
        ByteDisplay {
            length: 0,
            pos_x, pos_y,
            byte: 0,
            labels: Vec::new()
        }
    }

    pub fn init(&mut self, length: usize) {
        self.length = length;
    }

    pub fn draw(&self, window: &mut Window) {

        //  0101 1100
        //  ││ │ │  └─ Field 1: 50
        //  ││ │ └──── Field 2: Hello
        //  ││ └────── Field 3: Spam and Eggs
        //  │└──────── Field 4: ???
        //  └───────── Field 5: Testing


        // start_color();
        // init_pair(2, pancurses::COLOR_RED, pancurses::COLOR_BLACK);

        let bin_str = format!("{:08b}", self.byte); //(0..8).rev().map (|n| (self.byte >> n) & 1).collect::<String>();
        window.mvaddstr(self.pos_y as i32, self.pos_x as i32, bin_str);

        // if self.alert_active {
        //     let mut bold_attr = Attributes::new();
        //     //bold_attr.set_bold(true);
        //     let color_pair = match self.alert_type {
        //         AlertType::Info => pancurses::ColorPair(globals::INFO_COLOR),
        //         AlertType::Warn => pancurses::ColorPair(globals::WARNING_COLOR),
        //         AlertType::Error => pancurses::ColorPair(globals::ERROR_COLOR),
        //         _ => todo!()
        //     };
        //     bold_attr.set_color_pair(color_pair);
        //     let bold_attr = chtype::from(bold_attr);

        //     window.mvaddstr(self.pos_y as i32, self.pos_x as i32, &self.alert_text.iter().collect::<String>());
        //     window.mvchgat(self.pos_y as i32, self.pos_x as i32, self.alert_text.len() as i32, bold_attr, 0);
        //     window.mv(self.pos_y as i32, self.pos_x as i32);
        // } else {
        //     let s: String;
        //     if self.text.len() < self.length {
        //         s = String::from_iter(&self.text) + &" ".repeat(self.length as usize - self.text.len());
        //     } else if self.text.len() >= self.offset + self.length {
        //         //println!("offset={}", self.offset);
        //         s = String::from_iter(&self.text[self.offset..(self.offset + self.length)]);
        //         //println!("string={}", s);
        //     } else {
        //         s = String::from_iter(&self.text[self.offset..(self.offset + self.length - 1)]) + &" ";
        //     }
        //     window.mvaddstr(self.pos_y as i32, self.pos_x as i32, &s);
        //     //window.mv(y, x);
        //     window.mv(self.pos_y as i32, (self.pos_x + self.cursor_pos) as i32);
        // }
    }
}