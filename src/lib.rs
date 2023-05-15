use std::iter::FromIterator;

extern crate pancurses;
use pancurses::{initscr, endwin, Input, noecho, Window, resize_term};

mod line_entry;
use crate::line_entry::LineEntry;

mod hex_edit;
use crate::hex_edit::{FileManager, FillType, EditMode, HexEdit};
pub use crate::hex_edit::FileManagerType;

mod parsers;
use crate::parsers::{CommandToken, CommandKeyword, parse_command, parse_filltype, KeystrokeToken, parse_keystroke};

enum EditState {
    Escaped,
    Command,
    Edit
}

// Comands:
//
//      :set caps [on|off]  =>  Toggle the case of display hexadecimal values
//      :set hex [on|off]   =>  Toggle the hex display
//      :set ascii [on|off] =>  Toggle the ascii display
//      :set line #         =>  Set the number of bytes per line to #
//      :set fill r#        =>  Set the fill value to the contents of register #
//      :set fill x#        =>  Set the fill value to # (in hex)
//      :set icase [on|off] => Set ignore case for find function
//
//      :ins fmt #          =>  Insert # encoded as fmt at the cusor location
//      :ovr fmt #          =>  Overwrite # encoded as fmt at the cusor location
//
//      /expr               =>  Find expr in file


// Keystroke Commands:
//
//      ESC     =>  Clear the current keystroke buffer
//
//      i       =>  Change to hex insert mode
//      I       =>  Change to ASCII insert mode
//      o       =>  Change to hex overwrite mode
//      O       =>  Change to ASCII overwrite mode
//
//      g       =>  Move cursor to start of file
//      G       =>  Move cursor to end of file
//      #g      =>  Move cursor to #th byte from the start of the file
//      #G      =>  Move cursor to #th byte from the end of the file
//      +#g     =>  Move cursor # bytes forward
//      -#g     =>  Move cursor # bytes backward
//      n       =>  Seek to next find result
//      N       =>  Seek to previous find result
//
//      #f      =>  Insert next # bytes from the cursor location
//      #F      =>  Overwrite next # bytes from the cursor location
//      #d      =>  Delete next # bytes from the cursor location
//      #s      =>  Swap endinanness of next # bytes from the cursor location
//
//      #y      =>  Yank (copy) next # bytes from the cursor location to register 0
//      #r##y   =>  Yank (copy) next ## bytes from the cursor location to register #
//      p      =>  Insert contents of register # at cursor location
//      P      =>  Overwrite bytes with contents of register # at cursor location
//      #p      =>  Insert contents of register # at cursor location
//      #P      =>  Overwrite bytes with contents of register # at cursor location
//
//      u       =>  Undo last action
//      U       =>  Redo last action
//      #u      =>  Undo last # actions
//      #U      =>  Redo last # actions
//      #M      =>  Start recording #th macro
//      M       =>  Stop recording macro
//      #m      =>  Run #th macro



fn execute_command(window: &mut Window, hex_edit: &mut HexEdit, command: Vec<char>) -> Result<(), ()> {

    match command[0] {
        ':' => {
            let tokens = parse_command(&command[1..].to_vec());
            match tokens.len() {
                3 => {
                    match tokens[0] {
                        CommandToken::Keyword(CommandKeyword::Set) => { // :set [] []
                            match (&tokens[1], &tokens[2]) {
                                (CommandToken::Keyword(CommandKeyword::Caps), CommandToken::Keyword(CommandKeyword::On)) => {
                                    hex_edit.set_capitalize_hex(true);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Caps), CommandToken::Keyword(CommandKeyword::Off)) => {
                                    hex_edit.set_capitalize_hex(false);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Hex), CommandToken::Keyword(CommandKeyword::On)) => {
                                    hex_edit.set_show_hex(true);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Hex), CommandToken::Keyword(CommandKeyword::Off)) => {
                                    hex_edit.set_show_hex(false);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Ascii), CommandToken::Keyword(CommandKeyword::On)) => {
                                    hex_edit.set_show_ascii(true);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Ascii), CommandToken::Keyword(CommandKeyword::Off)) => {
                                    hex_edit.set_show_ascii(false);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Icase), CommandToken::Keyword(CommandKeyword::On)) => {
                                    hex_edit.set_ignore_case(true);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Icase), CommandToken::Keyword(CommandKeyword::Off)) => {
                                    hex_edit.set_ignore_case(false);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Line), CommandToken::Integer(_, n)) => {
                                    hex_edit.set_line_length(*n as u8);
                                    hex_edit.draw(window);
                                },
                                (CommandToken::Keyword(CommandKeyword::Fill), CommandToken::Word(word)) => {
                                    println!("Setting fill type");
                                    match parse_filltype(word.to_vec()) {
                                        Ok(fill) => hex_edit.set_fill(fill),
                                        Err(s) => println!("{}", s)
                                    }
                                },
                                _ => println!("Command not recognized 1")
                            }
                        },
                        _ => println!("Command not recognized 2")
                    }
                },
                _ => println!("Command not recognized 3")
            }
        },
        // FIND IN FILE
        '/' => {
            hex_edit.clear_find();
            hex_edit.find(&command[1..].to_vec());
            hex_edit.seek_next();
            hex_edit.draw(window);
        }
        _ => println!("Command not recognized 4")
    }

    Ok(())
}

fn execute_keystroke(window: &mut Window, hex_edit: &mut HexEdit, keystroke: Vec<char>) {
    let tokens = parse_keystroke(&keystroke);
    match tokens.len() {

        1 => {
            match tokens[0] {
                KeystrokeToken::Character('g') => {
                    hex_edit.set_cursor_pos(0);
                    hex_edit.draw(window);
                },
                KeystrokeToken::Character('G') => {
                    hex_edit.set_cursor_pos(hex_edit.len());
                    hex_edit.draw(window);
                },
                KeystrokeToken::Character('n') => {
                    hex_edit.seek_next();
                    hex_edit.draw(window);
                },
                KeystrokeToken::Character('N') => {
                    hex_edit.seek_prev();
                    hex_edit.draw(window);
                },
                KeystrokeToken::Character('u') => {
                    hex_edit.undo(1);
                    hex_edit.draw(window);
                },
                KeystrokeToken::Character('U') => {
                    hex_edit.redo(1);
                    hex_edit.draw(window);
                },
                KeystrokeToken::Character('p') => {
                    hex_edit.insert_register(0, hex_edit.get_cursor_pos());
                    hex_edit.draw(window);
                },
                KeystrokeToken::Character('P') => {
                    hex_edit.overwrite_register(0, hex_edit.get_cursor_pos());
                    hex_edit.draw(window);
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    println!("Command not recognized: {}", s)
                }
            }
        },

        2 => {
            match (tokens[0], tokens[1]) {
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    hex_edit.set_cursor_pos(n);
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('G')) => {
                    hex_edit.set_cursor_pos(hex_edit.len() - n);
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('f')) => {
                    hex_edit.insert_fill(hex_edit.get_cursor_pos(), n);
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('F')) => {
                    hex_edit.overwrite_fill(hex_edit.get_cursor_pos(), n);
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('d')) => {
                    hex_edit.delete_bytes(hex_edit.get_cursor_pos(), n);
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('s')) => {
                    hex_edit.swap_bytes(hex_edit.get_cursor_pos(), n);
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('y')) => {
                    hex_edit.yank(0, hex_edit.get_cursor_pos(), n);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('p')) => {
                    hex_edit.insert_register(n as u8, hex_edit.get_cursor_pos());
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('P')) => {
                    hex_edit.overwrite_register(n as u8, hex_edit.get_cursor_pos());
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('u')) => {
                    hex_edit.undo(n);
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Integer(n), KeystrokeToken::Character('U')) => {
                    hex_edit.redo(n);
                    hex_edit.draw(window);
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    println!("Command not recognized: {}", s)
                }
            }
        },

        3 => {
            match (tokens[0], tokens[1], tokens[2]) {
                (KeystrokeToken::Character('+'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    hex_edit.set_cursor_pos(hex_edit.get_cursor_pos() + n);
                    hex_edit.draw(window);
                },
                (KeystrokeToken::Character('-'), KeystrokeToken::Integer(n), KeystrokeToken::Character('g')) => {
                    hex_edit.set_cursor_pos(hex_edit.get_cursor_pos() - n);
                    hex_edit.draw(window);
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    println!("Command not recognized: {}", s)
                }
            }
        }

        4 => {
            match (tokens[0], tokens[1], tokens[2], tokens[3]) {
                (KeystrokeToken::Integer(n1), KeystrokeToken::Character('r'), KeystrokeToken::Integer(n2), KeystrokeToken::Character('y')) => {
                    hex_edit.yank(n1 as u8, hex_edit.get_cursor_pos(), n2);
                },
                _ => {
                    let s: String = keystroke.iter().collect();
                    println!("Command not recognized: {}", s)
                }
            }
        }
        _ => {
            let s: String = keystroke.iter().collect();
            println!("Command not recognized: {}", s)
        }
    }
}

pub fn run(filename: String, file_manager_type: FileManagerType) {
    let mut edit_state = EditState::Escaped;
    let mut window = initscr();
    window.refresh();
    window.keypad(true);
    noecho();

    let mut current_keystroke = Vec::<char>::new();

    let mut fm = FileManager::new(filename, file_manager_type);

    let mut hex_edit = HexEdit::new(fm, // File Manager
                                    0, 0, // x, y
                                    window.get_max_x() as usize - 1, window.get_max_y()as usize - 1, // width, height
                                    16, true, true, // Line Length, Show Hex, Show ASCII
                                    '.', "  ".to_string(), true); // Invalid ASCII, Separator, Capitalize Hex

    let mut cursor_index_len = format!("{:x}", hex_edit.len()).len();

    let mut line_entry = LineEntry::new(window.get_max_x() as usize - 2 - cursor_index_len, 0, window.get_max_y() as usize - 1);

    hex_edit.set_viewport_row(0);
    hex_edit.draw(&mut window);

    loop {
        let ch_input = window.getch();

        match edit_state {
            EditState::Escaped => {
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                        println!("{}", window.get_max_y());
                        hex_edit.resize(window.get_max_x() as usize - 1, window.get_max_y()as usize - 1);
                        line_entry.reset_geometry(window.get_max_x() as usize - 2 - cursor_index_len, 0, window.get_max_y() as usize - 1);
                        hex_edit.draw(&mut window);
                        line_entry.draw(&mut window);
                    },
                    Some(Input::Character('\u{1b}')) => {
                        current_keystroke = Vec::<char>::new();
                    },
                    Some(Input::Character('o')) => {
                        edit_state = EditState::Edit;
                        hex_edit.set_edit_mode(EditMode::HexOverwrite);
                        hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('O')) => {
                        edit_state = EditState::Edit;
                        hex_edit.set_edit_mode(EditMode::AsciiOverwrite);
                        hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('i')) => {
                        edit_state = EditState::Edit;
                        hex_edit.set_edit_mode(EditMode::HexInsert);
                        hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character('I')) => {
                        edit_state = EditState::Edit;
                        hex_edit.set_edit_mode(EditMode::AsciiInsert);
                        hex_edit.refresh_cursor(&mut window)
                    },
                    Some(Input::Character(':')) | Some(Input::Character('/')) => {
                        edit_state = EditState::Command;
                        line_entry.addch(ch_input.unwrap()); 
                        line_entry.draw(&mut window);
                    }, 
                    Some(Input::Character(c)) => {
                        current_keystroke.push(c);
                        if matches!(c, 'g' | 'G' | 'f' | 'F' | 'd' | 'y' | 'p' | 'P' | 'u' | 'U' | 'n' | 'N' | 's') {
                            execute_keystroke(&mut window, &mut hex_edit, current_keystroke);
                            current_keystroke = Vec::<char>::new();
                        }
                    },
                    Some(ch) if matches!(ch, Input::KeyRight | Input::KeyLeft | Input::KeyUp | Input::KeyDown | Input::KeyNPage | Input::KeyPPage | Input::KeyHome | Input::KeyEnd) => {
                        if hex_edit.addch(&mut window, ch) {
                            hex_edit.draw(&mut window);
                        } else {
                            hex_edit.refresh_cursor(&mut window);
                        }
                    },
                    _ => {
                        println!("Invalid keystroke");
                    }
                }
            },
            EditState::Command => {
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    },
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                    },
                    Some(Input::Character('\n')) => {
                        println!("Newline: {}", String::from_iter(line_entry.get_text()));
                        execute_command(&mut window, &mut hex_edit, line_entry.get_text());
                        line_entry.clear();
                        line_entry.draw(&mut window);
                        edit_state = EditState::Escaped;
                        hex_edit.refresh_cursor(&mut window);
                    },
                    Some(ch) => { 
                        line_entry.addch(ch); 
                        line_entry.draw(&mut window);
                    },
                    None => ()
                }
            },
            EditState::Edit => {
                match ch_input {
                    Some(Input::KeyResize) => {
                        resize_term(0, 0);
                    }
                    Some(Input::Character('\u{1b}')) => {
                        edit_state = EditState::Escaped;
                    },
                    Some(ch) => { 
                        if hex_edit.addch(&mut window, ch) {
                            hex_edit.draw(&mut window);
                        } else {
                            hex_edit.refresh_cursor(&mut window);
                        }
                        //line_entry.draw(&mut window);
                    },
                    None => ()
                }
            }
        }

        let (y, x) = window.get_cur_yx();

        // Draw cursor index
        window.mvaddstr((window.get_max_y() as usize - 1) as i32,
        (window.get_max_x() as usize - 1 - cursor_index_len) as i32, 
        format!("{:0width$x}", hex_edit.get_cursor_pos(), width=cursor_index_len));

        window.mv(y, x);

    }

}
