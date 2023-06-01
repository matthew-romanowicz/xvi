use std::env;
use xvi::{run, FileManagerType};

fn main() {

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


    match run("C:\\Users\\Matthew\\Documents\\Git\\xvi\\test_file4.bin.gz".to_string(), file_manager_type, extract) {
        Ok(_) => {
            std::process::exit(0);
        },
        Err(msg) => {
            eprintln!("{}", msg.to_string());
            std::process::exit(1)
        }
    }
}