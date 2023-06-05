use pancurses::{Input, Window, chtype, Attributes, start_color, init_pair};

pub enum Align {
    Left(usize),
    Center,
    Right(usize)
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
    bolds: Vec<(usize, usize)>,
    underlines: Vec<(usize, usize)>
}

impl Paragraph {
    pub fn new(text: String, align: Align) -> Paragraph {
        let mut s = String::new();
        let mut bold_start: Option<usize> = None;
        let mut underline_start: Option<usize> = None;
        let mut bolds: Vec<(usize, usize)> = Vec::new();
        let mut underlines: Vec<(usize, usize)> = Vec::new();
        let mut in_tag = false;
        let mut current_tag = String::new();
        let mut index = 0;

        for (i, c) in text.chars().enumerate() {
            if in_tag {
                match c {
                    '>' => {
                        in_tag = false;
                        match current_tag.as_str() {
                            "b" => bold_start = Some(index),
                            "u" => underline_start = Some(index),
                            "/b" => match bold_start {
                                None => println!("Close tag '{}' does not have matching start tag", current_tag),
                                Some(n) => bolds.push((n, index))
                            },
                            "/u" => match underline_start {
                                None => println!("Close tag '{}' does not have matching start tag", current_tag),
                                Some(n) => underlines.push((n, index))
                            },
                            _ => println!("Unrecognized tag: {}", current_tag)
                        };
                        current_tag = String::new();
                    },
                    _ => current_tag.push(c)
                }
            } else {
                match c {
                    '<' => in_tag = true,
                    _ => {
                        s.push(c);
                        index += 1;
                    }
                        
                }
            }
        }
        Paragraph {text: s, align, bolds, underlines}
    }

    fn get_line_ranges(&self, width: usize) -> Vec<(usize, usize)> {
        let mut res: Vec::<(usize, usize)> = Vec::new(); // [start, end)
        let mut indent = 0; 
        let mut line_start: usize = 0;
        let mut word_start: usize = 0;
        for (i, c) in self.text.chars().enumerate() {
            if c == ' ' {
                if i - line_start + indent > width {
                    res.push((line_start, word_start));
                    line_start = word_start + 1; // To account for the space
                    word_start = line_start;
                    indent = match self.align {Align::Left(n) | Align::Right(n) => n, _ => 0}; // Doesn't apply to first line
                } else {
                    word_start = i;
                }
            } 
        }
        if self.text.chars().count() - line_start + indent > width {
            res.push((line_start, word_start));
            line_start = word_start + 1;
        }
        res.push((line_start, self.text.chars().count()));
        res
    }

    fn index_from_line(&self, line: usize, width: usize) -> usize {
        self.get_line_ranges(width)[line].0
    }

    fn line_from_index(&self, index: usize, width: usize) -> usize {
        for (i, (start, end)) in self.get_line_ranges(width).iter().enumerate() {
            if *start < index {
                return i
            }
        }
        return 0
    }

    pub fn get_lines(&self, width: usize) -> Vec<TextLine> {
        let mut res = Vec::<TextLine>::new();
        //println!("New paragraph {}", width);
        let mut indent = 0;
        for (start, end) in self.get_line_ranges(width) {
            //println!("{} {}", start, end);
            let offset = match self.align {
                Align::Left(_) => indent,
                Align::Right(_) => width - (end - start + indent),
                Align::Center => (width - (end - start + indent))/2
            };
            let mut bolds: Vec<(usize, usize)> = Vec::new();
            for (b_start, b_end) in &self.bolds {
                if *b_start > end {
                    break;
                } else if *b_end > start {
                    bolds.push((std::cmp::max(start, *b_start) - start + offset, std::cmp::min(end, *b_end) - start + offset));
                }
            }
            let mut underlines: Vec<(usize, usize)> = Vec::new();
            for (u_start, u_end) in &self.underlines {
                if *u_start > end {
                    break;
                } else if *u_end > start {
                    underlines.push((std::cmp::max(start, *u_start) - start + offset, std::cmp::min(end, *u_end) - start + offset));
                }
            }
            let pad = " ".repeat(width - (end - start + indent));
            let text = match self.align {
                Align::Left(_) => {
                    " ".repeat(indent).to_string() + &self.text.as_str()[start..end].to_string() + &pad
                },
                Align::Right(_) => {
                    pad + &self.text.as_str()[start..end].to_string() + &" ".repeat(indent).to_string()
                },
                Align::Center => { // Indent should always be zero for align::center
                    pad[0..(pad.len()/2)].to_owned() + &self.text.as_str()[start..end].to_string() + &pad[(pad.len()/2)..pad.len()]
                }
            };
            res.push(TextLine {
                text,
                bolds,
                italics: vec![],
                underlines
            });
            indent = match self.align {Align::Left(n) | Align::Right(n) => n, _ => 0}; // Doesn't apply to first line
        }
        res
    }

}

pub struct LargeTextView {
    width: usize,
    height: usize,
    pos_x: usize,
    pos_y: usize,
    paras: Vec<Paragraph>,
    lines: Vec<TextLine>,
    offset: usize,
}

impl LargeTextView {

    pub fn new(pos_x: usize, pos_y: usize, width: usize, height: usize, text: String) -> LargeTextView {
        let mut paras: Vec<Paragraph> = Vec::new();
        for line in text.split("\n").map(|s| s.to_string()) {
            let mut escaped: bool = false;
            let mut line_iter = line.chars().peekable();
            let mut align = Align::Left(30);
            loop {
                match line_iter.peek() {
                    Some('\\') => {
                        escaped = !escaped;
                        line_iter.next();
                        continue;
                    },
                    Some('c') if escaped => {
                        align = Align::Center;
                        line_iter.next();
                    },
                    _ => break

                }
                escaped = false;
            }
            paras.push(Paragraph::new(line_iter.collect::<String>(), align));

        }
        let mut res = LargeTextView {
            width,
            height,
            pos_x,
            pos_y,
            paras,
            lines: Vec::new(),
            offset: 0
        };
        res.calculate_lines();
        res
    }

    pub fn calculate_lines(&mut self) {
        self.lines = Vec::new();
        for para in &self.paras {
            self.lines.extend(para.get_lines(self.width))
        }
    }

    pub fn resize(&mut self, width: usize, height: usize) { // TODO: Make this more efficient
        let mut line = 0;
        let mut index = 0;
        let mut p_index = 0;
        for (i, p) in self.paras.iter().enumerate() {
            let num_lines = p.get_line_ranges(self.width).len();
            if line + num_lines > self.offset {
                index = p.index_from_line(self.offset - line, width);
                p_index = i;
                break
            }
            line += num_lines;
        }
        self.width = width;
        self.height = height;
        self.calculate_lines();

        let mut offset = 0;
        for (i, p) in self.paras.iter().enumerate() {
            if i == p_index {
                offset += p.line_from_index(index, self.width);
                break
            } else {
                offset += p.get_line_ranges(self.width).len();
            }
        }

        self.offset = std::cmp::min(offset, self.lines.len() - self.height);
    }

    pub fn addch(&mut self, ch: Input) { // TODO: Make this work if the height is larger than the number of lines
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
            Input::KeyHome => self.offset = 0,
            Input::KeyEnd => self.offset = self.lines.len() - self.height,
            Input::KeyPPage => {
                if self.offset > self.height {
                    self.offset -= self.height;
                } else {
                    self.offset = 0;
                }
            },
            Input::KeyNPage => self.offset = std::cmp::min(self.offset + self.height, self.lines.len() - self.height),
            _ => println!("Invalid input")
        }
    }

    pub fn draw(&self, window: &mut Window) { // TODO: Make this work if the height is larger than the number of lines
        let mut bold_attr = Attributes::new();
        bold_attr.set_bold(true);
        let bold_attr = chtype::from(bold_attr);
        let mut underline_attr = Attributes::new();
        underline_attr.set_underline(true);
        let underline_attr = chtype::from(underline_attr);

        for (y, line) in self.lines[self.offset..self.offset + self.height].iter().enumerate() {
            window.mvaddstr((y + self.pos_y) as i32, self.pos_x as i32, line.text.to_owned() + &" ".repeat(self.width as usize - line.text.len()));
            for (start, end) in &line.bolds {
                window.mvchgat((y + self.pos_y) as i32, *start as i32, (end - start) as i32, bold_attr, 0);
            }
            for (start, end) in &line.underlines {
                window.mvchgat((y + self.pos_y) as i32, *start as i32, (end - start) as i32, underline_attr, 0);
            }
        }
    }
}