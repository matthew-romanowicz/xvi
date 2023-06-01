use pancurses::{Input, Window, chtype, Attributes, start_color, init_pair};

pub enum Align {
    Left(usize),
    Center,
    Right
}

pub struct Tag {
    open: String,
    close: String,
    attr: chtype
}

pub struct TextLine {
    text: String,
    bolds: Vec<(usize, usize)>,
    italics: Vec<(usize, usize)>,
    underlines: Vec<(usize, usize)>
}

pub struct Paragraph {
    text: String,
    align: Align,
}

impl Paragraph {
    pub fn new(text: String, align: Align) -> Paragraph {
        Paragraph {text, align}
    }

    pub fn get_lines(&self, width: usize) -> Vec<TextLine> {
        let mut res = Vec::<TextLine>::new();
        let mut line = String::new();
        let mut word = String::new();
        let mut current_tag = String::new();
        let mut active_tag: Option<String> = None;
        let mut in_tag = false;
        let mut bolds = Vec::<(usize, usize)>::new();
        let mut line_index = 0;
        for (i, c) in self.text.chars().enumerate() {
            if in_tag {
                match c {
                    '>' => {
                        in_tag = false;
                        match current_tag.as_str() {
                            "b" => bolds.push((line_index, line_index)),
                            "/b" => todo!(),
                            _ => todo!()
                        }
                    },
                    _ => current_tag.push(c)
                }
            } else {
                match c {
                    ' ' => {
                        if line.len() + word.len() + 1 <= width {
                            line.push(' ');
                            line += &word.to_string();
                        } else {
                            res.push(TextLine {
                                text: line,
                                bolds: vec![],
                                italics: vec![],
                                underlines: vec![]
                            });
                            line = match self.align {
                                Align::Left(n) => " ".repeat(n) + &word.to_string(),
                                _ => word.to_string()
                            };
                        }
                        word = String::new();
                    },
                    '<' => in_tag = true,
                    _ => {
                        line_index += 1;
                        word.push(c)
                    }
                        
                }
                line_index += 1;
            }
        }

        if line.len() + word.len() + 1 <= width {
            line.push(' ');
            line += &word.to_string();
        } else {
            res.push(TextLine {
                text: line,
                bolds: vec![],
                italics: vec![],
                underlines: vec![]
            });
            line = match self.align {
                Align::Left(n) => " ".repeat(n) + &word.to_string(),
                _ => word.to_string()
            };
        }
        res.push(TextLine {
            text: line,
            bolds: vec![],
            italics: vec![],
            underlines: vec![]
        });

        res
    }

}

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

    pub fn draw2(&self, window: &mut Window) {
        let mut line_num = 0;
        for line in &self.lines {
            let para = Paragraph::new(line.to_string(), Align::Left(30));
            for text_line in para.get_lines(self.width) {
                if line_num >= self.offset {
                    if line_num >= self.offset + self.height {
                        break;
                    }
                    let y = line_num - self.offset;
                    window.mvaddstr((y + self.pos_y) as i32, self.pos_x as i32, text_line.text.to_owned() + &" ".repeat(self.width as usize - text_line.text.len()));
                }
                line_num += 1;
            }
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