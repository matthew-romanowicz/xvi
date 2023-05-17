use pancurses::{Input, Window, chtype, Attributes, start_color, init_pair};

pub struct LargeTextView {
    width: usize,
    height: usize,
    pos_x: usize,
    pos_y: usize,
    lines: Vec<String>,
    offset: usize,
}

impl LargeTextView {

    pub fn new(pos_x: usize, pos_y: usize, width: usize, height: usize, text: String) -> LargeTextView {
        LargeTextView {
            width,
            height,
            pos_x,
            pos_y,
            lines: text.split("\n").map(|s| s.to_string()).collect(),
            offset: 0
        }
    }

    pub fn addch(&mut self, ch: Input) {
        match ch {
            Input::KeyUp => {
                if self.offset > 0 {
                    self.offset -= 1;
                }
            },
            Input::KeyDown => {
                if self.offset + self.height < self.lines.len() {
                    self.offset += 1
                }
            },
            _ => println!("Invalid input")
        }
    }

    pub fn draw(&self, window: &mut Window) {

        let mut bold_attr = Attributes::new();
        bold_attr.set_bold(true);
        let bold_attr = chtype::from(bold_attr);

        let mut bold: bool = false;
        let mut center: bool = false;

        for (y, line) in self.lines[self.offset..(self.offset + self.height)].iter().enumerate() {

            bold = false;
            center = false;

            let mut line_iter = line.chars().peekable();

            let mut escaped: bool = false;
            loop {
                match line_iter.peek() {
                    Some('\\') => {
                        escaped = !escaped;
                        line_iter.next();
                        continue;
                    },
                    Some('b') if escaped => {
                        bold = true;
                        line_iter.next();
                    },
                    Some('c') if escaped => {
                        center = true;
                        line_iter.next();
                    },
                    _ => break

                }
                escaped = false;
            }

            let s: String = line_iter.collect();

            if line.len() > self.width {
                window.mvaddstr((y + self.pos_y) as i32, self.pos_x as i32, &s[0..self.width]);
            } else if center {
                let pad = " ".repeat(self.width as usize - line.len());

                window.mvaddstr((y + self.pos_y) as i32, self.pos_x as i32, pad[0..(pad.len()/2)].to_owned() + &s + &pad[(pad.len()/2)..pad.len()]);
            } else {
                window.mvaddstr((y + self.pos_y) as i32, self.pos_x as i32, s.to_owned() + &" ".repeat(self.width as usize - line.len()));
            }

            if bold {
                window.mvchgat((y + self.pos_y) as i32, self.pos_x as i32, self.width as i32, bold_attr, 0);
            }

        }
    }
}