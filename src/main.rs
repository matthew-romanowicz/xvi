use std::env;
use xvi::{run, FileManagerType};

fn main() {

    const HELP_TEXT: &str = "xvi 0.2 (2023-05-14)
author: Matthew Romanowicz
usage: xvi path [options]

options:
-h      Display help text (this message) and exit
-l      Open file in live edit mode (cannot be used with -s)
-s      Open file in swap mode (cannot be used with -l)
-x      Extract file as gzip for editing (cannot be used with -l)";

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
                        println!("Live edit");
                    },
                    's' => {
                        file_manager_type = FileManagerType::SwapFile;
                        println!("Swap file");
                    },
                    'x' => {
                        extract = true;
                    },
                    _ => {
                        println!("Option not recognized: {}. Run 'xvi -h' for help", ch);
                        std::process::exit(1);
                    }
                }
            }
        } else {
            path = arg;
        }
    }

    println!("Path: {}", path);
    println!("Extract: {}", extract);


    run("C:\\Users\\Matthew\\Documents\\Git\\xvi\\test_file2.bin".to_string(), FileManagerType::RamOnly);
}