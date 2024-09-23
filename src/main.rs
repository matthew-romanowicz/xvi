use std::env;
use log::*;
use xvi::{run, FileManagerType};

fn main() {

    pretty_env_logger::init();

    const VERSION: &str = env!("CARGO_PKG_VERSION");

    let HELP_TEXT: String = format!("xvi v{} (2023-05-14)
author: Matthew Romanowicz
usage: xvi path [options]

options:
-h      Display help text (this message) and exit
-l      Open file in live edit mode (cannot be used with -r or -s)
-r      Open file in read-only mode (cannot be used with -l or -s)
-s      Open file in swap mode (cannot be used with -l or -r)
-x      Extract file as gzip for editing (cannot be used with -l)", VERSION);

    let mut extract: bool = false;
    let mut file_manager_type: FileManagerType = FileManagerType::RamOnly;
    let mut path = "".to_string();

    let mut args = env::args();
    args.next();

    for arg in args {
        let mut chars = arg.chars();
        if chars.nth(0) == Some('-') {
            for ch in chars {
                match ch {
                    'h' => {
                        println!("{}", HELP_TEXT);
                        std::process::exit(0);
                    }
                    'l' => {
                        file_manager_type = FileManagerType::LiveEdit;
                    },
                    's' => {
                        file_manager_type = FileManagerType::SwapFile;
                    },
                    'r' => {
                        file_manager_type = FileManagerType::ReadOnly;
                    },
                    'x' => {
                        extract = true;
                    },
                    _ => {
                        eprintln!("Option not recognized: {}. Run 'xvi -h' for help", ch);
                        std::process::exit(1);
                    }
                }
            }
        } else {
            path = arg;
        }
    }


    // let fname = r"C:\Users\Matthew\Documents\Rust\xvi\tests\ps2n0g08.png".to_string();
    // let fname = r"C:\Users\Matthew\Documents\Rust\xvi\tests\s03n3p01.png".to_string();
    // let fname = r"C:\Users\Matthew\Documents\Rust\xvi\tests\tbbn0g04.png".to_string();
    // let fname = r"C:\Users\Matthew\Documents\Rust\xvi\tests\ch1n3p04.png".to_string();
    // let fname = r"C:\Users\Matthew\Documents\Rust\xvi\tests\cm9n0g04.png".to_string();
    let fname = r"C:\Users\Matthew\Documents\Rust\xvi\tests\ccwn3p08.png".to_string();
    // let fname = r"C:\Users\Matthew\Documents\Rust\xvi\tests\exif2c08.png".to_string();
    match run(fname, file_manager_type, extract) {
        Ok(_) => {
            std::process::exit(0);
        },
        Err(msg) => {
            eprintln!("{}", msg.to_string());
            std::process::exit(1)
        }
    }
}