use std::fs::File;
use std::io::{Read, Write, Seek, SeekFrom};

use regex::bytes::RegexBuilder;

use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;

pub struct FindResult {
    pub start: usize,
    pub span: usize
}

pub enum FileManagerType {
    RamOnly,
    SwapFile,
    LiveEdit,
    ReadOnly
}

pub struct FileManager<'a> {
    filename: String,
    file_manager_type: FileManagerType,
    extract: bool,
    handle: File,
    metadata: std::fs::Metadata,
    modified: bool,
    block_size: usize, // Not used for FileManagerType::RamOnly
    swap_handle: Option<File>, // Only used for FileManagerType::SwapFile
    swap_metadata: Option<&'a std::fs::Metadata>, // Only used for FileManagerType::SwapFile
    file_buffer: Vec<u8>, // Only used for FileManagerType::RamOnly
}

fn get_bytes_from_handle(handle: &mut File, index: usize, buffer: &mut Vec<u8>) -> std::io::Result<usize> {
    let file_length = handle.metadata()?.len() as usize;
    handle.seek(SeekFrom::Start(index as u64))?;
    if file_length >= index + buffer.len() {
        match handle.read_exact(buffer) {
            Ok(_) => Ok(buffer.len()),
            Err(msg) => Err(msg)
        }
    } else {
        let mut mini_buffer: Vec<u8> = vec![0; file_length - index];
        handle.read_exact(&mut mini_buffer)?;
        for (i, c) in mini_buffer.iter().enumerate() {
            buffer[i] = *c;
        }
        Ok(mini_buffer.len())
    }
}

fn offset_bytes_from_handle(handle: &mut File, index: usize, n_bytes: u64, block_size: usize) -> std::io::Result<()> {
    let file_length = handle.metadata()?.len() as usize;
    let mut buffer: Vec<u8> = vec![0; block_size];
    let mut i: u64 = index as u64;
    while i as usize + block_size < file_length {
        handle.seek(SeekFrom::Start(i))?;
        handle.read_exact(&mut buffer)?;
        handle.seek(SeekFrom::Start(i + n_bytes))?;
        handle.write(&buffer)?;
        i += block_size as u64;
    }
    unsafe {buffer.set_len(file_length - i as usize)}
    handle.seek(SeekFrom::Start(i))?;
    handle.read_exact(&mut buffer)?;
    handle.seek(SeekFrom::Start(i + n_bytes))?;
    handle.write(&buffer)?;       
    Ok(())       
}

fn delete_bytes_from_handle(handle: &mut File, index: usize, n_bytes: u64, block_size: usize) -> std::io::Result<()> {
    let file_length = handle.metadata()?.len() as usize;
    let mut buffer: Vec<u8> = vec![0; block_size];
    let mut i: u64 = index as u64;
    while (i + n_bytes) as usize + block_size < file_length {
        handle.seek(SeekFrom::Start(i + n_bytes))?;
        handle.read_exact(&mut buffer)?;
        handle.seek(SeekFrom::Start(i))?;
        handle.write(&buffer)?;
        i += block_size as u64;
    }
    unsafe {buffer.set_len(file_length - (i + n_bytes) as usize)}
    handle.seek(SeekFrom::Start(i + n_bytes))?;
    handle.read_exact(&mut buffer)?;
    handle.seek(SeekFrom::Start(i))?;
    handle.write(&buffer)?;       
    handle.set_len(file_length as u64 - n_bytes)?;
    Ok(())
}

fn insert_bytes_from_handle(handle: &mut File, index: usize, buffer: &Vec<u8>, block_size: usize) -> std::io::Result<()> {
    offset_bytes_from_handle(handle, index, buffer.len() as u64, block_size)?;
    handle.seek(SeekFrom::Start(index as u64))?;
    match handle.write(&buffer) {
        Ok(n) if n == buffer.len() => Ok(()),
        Ok(_) => Err(std::io::Error::new(std::io::ErrorKind::Other, "Not all bytes could be written".to_string())),
        Err(msg) => Err(msg)
    }
}

fn append_bytes_from_handle(handle: &mut File, buffer: &Vec<u8>) -> std::io::Result<()> {
    handle.seek(SeekFrom::End(0))?;
    match handle.write(buffer) {
        Ok(n) if n == buffer.len() => Ok(()),
        Ok(_) => Err(std::io::Error::new(std::io::ErrorKind::Other, "Not all bytes could be written".to_string())),
        Err(msg) => Err(msg)        
    }
}

fn overwrite_bytes_from_handle(handle: &mut File, index: usize, buffer: &Vec<u8>) -> std::io::Result<()> {
    handle.seek(SeekFrom::Start(index as u64))?;
    match handle.write(buffer) {
        Ok(n) if n == buffer.len() => Ok(()),
        Ok(_) => Err(std::io::Error::new(std::io::ErrorKind::Other, "Not all bytes could be written".to_string())),
        Err(msg) => Err(msg)        
    }
}


impl FileManager<'_> {
    pub fn new<'a>(filename: String, file_manager_type: FileManagerType, extract: bool) -> std::io::Result<FileManager<'a>>{

        let mut handle = File::options().read(true).write(true).open(&filename)?;
        let metadata = handle.metadata()?;

        let mut file_buffer = vec![];
        match file_manager_type {
            FileManagerType::RamOnly => {
                if extract {
                    //let mut temp_file_buffer = Vec::<u8>::new();
                    //handle.read_to_end(&mut temp_file_buffer);
                    let mut d = GzDecoder::new(&handle);
                    d.read_to_end(&mut file_buffer)?;
                } else {
                    handle.read_to_end(&mut file_buffer)?;
                }
            },
            FileManagerType::SwapFile => {
                todo!()
                
            },
            FileManagerType::LiveEdit | FileManagerType::ReadOnly => {
                // TODO: Is anything actually needed here?
            }
        }

        Ok(FileManager {
            filename,
            file_manager_type,
            extract,
            handle,
            metadata,
            modified: false,
            block_size: 65535,
            swap_handle: None,
            swap_metadata: None,
            file_buffer
        })
    }

    pub fn set_block_size(&mut self, block_size: usize) {
        self.block_size = block_size;
    }

    pub fn is_modified(&self) -> bool {
        match self.file_manager_type {
            FileManagerType::RamOnly | FileManagerType::SwapFile => {
                self.modified
            },
            FileManagerType::LiveEdit | FileManagerType::ReadOnly => {
                false
            }
        }
    }

    pub fn save(&mut self) -> std::io::Result<()>{
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.handle.seek(SeekFrom::Start(0))?;
                if self.extract {
                    let mut d = GzEncoder::new(Vec::new(), Compression::default());
                    d.write_all(&self.file_buffer)?;
                    self.handle.write_all(&d.finish()?)?;
                } else {
                    self.handle.write_all(&self.file_buffer)?;
                }
                let n = self.handle.seek(SeekFrom::Current(0))?;
                self.handle.set_len(n)?;
                self.modified = false;
                Ok(())
            },
            FileManagerType::SwapFile => {
                todo!()
            },
            FileManagerType::LiveEdit | FileManagerType::ReadOnly => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only and live modes".to_string()))
            }
        }
    }

    pub fn get_bytes(&mut self, index: usize, buffer: &mut Vec<u8>) -> std::io::Result<usize> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                let n = buffer.len();
                if self.file_buffer.len() >= index + n {
                    buffer.copy_from_slice(&self.file_buffer[index..(index + n)]);
                    Ok(n)
                } else if index < self.file_buffer.len() {
                    for (i, c) in self.file_buffer[index..self.file_buffer.len()].iter().enumerate() {
                        buffer[i] = *c;
                    }
                    Ok(self.file_buffer.len() - index)
                } else {
                    Ok(0)
                }
            },
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => get_bytes_from_handle(handle, index, buffer),
                    None => Err(std::io::Error::new(std::io::ErrorKind::Other, "No swap handle found".to_string()))
                }
            },
            FileManagerType::LiveEdit | FileManagerType::ReadOnly => {
                get_bytes_from_handle(&mut self.handle, index, buffer)
            }
        }
    }

    pub fn overwrite_byte(&mut self, index: usize, c: u8)  -> std::io::Result<()> {
        self.modified = true;
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer[index] = c;
                Ok(())
            },
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => overwrite_bytes_from_handle(handle, index, &vec![c]),
                    None => Err(std::io::Error::new(std::io::ErrorKind::Other, "No swap handle found".to_string()))
                }
            },
            FileManagerType::LiveEdit => {
                overwrite_bytes_from_handle(&mut self.handle, index, &vec![c])
            },
            FileManagerType::ReadOnly => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn insert_byte(&mut self, index: usize, c: u8)  -> std::io::Result<()> {
        self.modified = true;
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer.insert(index, c);
                Ok(())
            },
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => insert_bytes_from_handle(handle, index, &vec![c], self.block_size),
                    None => Err(std::io::Error::new(std::io::ErrorKind::Other, "No swap handle found".to_string()))
                }
            },
            FileManagerType::LiveEdit => {
                insert_bytes_from_handle(&mut self.handle, index, &vec![c], self.block_size)
            },
            FileManagerType::ReadOnly => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn delete_byte(&mut self, index: usize)  -> std::io::Result<()> {
        self.modified = true;
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer.remove(index);
                Ok(())
            },
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => delete_bytes_from_handle(handle, index, 1, self.block_size),
                    None => Err(std::io::Error::new(std::io::ErrorKind::Other, "No swap handle found".to_string()))
                }
            },
            FileManagerType::LiveEdit => {
                delete_bytes_from_handle(&mut self.handle, index, 1, self.block_size)
            },
            FileManagerType::ReadOnly => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn get_byte(&mut self, index: usize) -> Option<u8> {
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                if index < self.file_buffer.len() {
                    Some(self.file_buffer[index])
                } else {
                    None
                }
            },
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => {
                        let mut buffer: Vec<u8> = vec![0];
                        match get_bytes_from_handle(handle, index, &mut buffer) {
                            Ok(1) => Some(buffer[0]),
                            _ => None
                        }
                    },
                    None => panic!("Swap handle not found")
                }
            },
            FileManagerType::LiveEdit | FileManagerType::ReadOnly => {
                let mut buffer: Vec<u8> = vec![0];
                match get_bytes_from_handle(&mut self.handle, index, &mut buffer) {
                    Ok(1) => Some(buffer[0]),
                    _ => None
                }
            }
        }
    }

    pub fn insert_bytes(&mut self, index: usize, bytes: &Vec<u8>)  -> std::io::Result<()> {
        self.modified = true;
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                let second_part = self.file_buffer[index..self.file_buffer.len()].to_vec();
                unsafe {self.file_buffer.set_len(index)};
                self.file_buffer.extend(bytes);
                self.file_buffer.extend(second_part);
                Ok(())
            }
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => insert_bytes_from_handle(handle, index, bytes, self.block_size),
                    None => Err(std::io::Error::new(std::io::ErrorKind::Other, "No swap handle found".to_string()))
                }
            },
            FileManagerType::LiveEdit => {
                insert_bytes_from_handle(&mut self.handle, index, bytes, self.block_size)
            },
            FileManagerType::ReadOnly => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn overwrite_bytes(&mut self, index: usize, bytes: &Vec<u8>)  -> std::io::Result<()> {
        self.modified = true;
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                for (i, c) in bytes.iter().enumerate() {
                    self.file_buffer[i + index] = *c
                }
                Ok(())
            }
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => overwrite_bytes_from_handle(handle, index, bytes),
                    None => Err(std::io::Error::new(std::io::ErrorKind::Other, "No swap handle found".to_string()))
                }
            },
            FileManagerType::LiveEdit => {
                overwrite_bytes_from_handle(&mut self.handle, index, bytes)
            },
            FileManagerType::ReadOnly => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn append_bytes(&mut self, bytes: &Vec<u8>) -> std::io::Result<()> {
        self.modified = true;
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer.extend(bytes);
                Ok(())
            },
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => append_bytes_from_handle(handle, bytes),
                    None => Err(std::io::Error::new(std::io::ErrorKind::Other, "No swap handle found".to_string()))
                }
            },
            FileManagerType::LiveEdit => {
                append_bytes_from_handle(&mut self.handle, bytes)
            },
            FileManagerType::ReadOnly => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn delete_bytes(&mut self, index: usize, n_bytes: usize) -> std::io::Result<()> {
        self.modified = true;
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                let second_part = self.file_buffer[(index + n_bytes)..self.file_buffer.len()].to_vec();
                unsafe {self.file_buffer.set_len(index)};
                self.file_buffer.extend(second_part);
                Ok(())
            }
            FileManagerType::SwapFile => {
                match &mut self.swap_handle {
                    Some(ref mut handle) => delete_bytes_from_handle(handle, index, n_bytes as u64, self.block_size),
                    None => Err(std::io::Error::new(std::io::ErrorKind::Other, "No swap handle found".to_string()))
                }
            },
            FileManagerType::LiveEdit => {
                delete_bytes_from_handle(&mut self.handle, index, n_bytes as u64, self.block_size)
            },
            FileManagerType::ReadOnly => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn len(&self) -> usize{
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                self.file_buffer.len()
            }
            FileManagerType::SwapFile => {
                match &self.swap_handle {
                    Some(handle) => handle.metadata().expect("Error reading metadata").len() as usize, // TODO make this more safe
                    None => panic!("Swap handle not found")
                }

            },
            FileManagerType::LiveEdit | FileManagerType::ReadOnly => {
                self.handle.metadata().expect("Error reading metadata").len() as usize // TODO make this more safe
            }
        }        
    }

    pub fn find(&self, expr: &Vec<u8>, ignore_case: bool, use_regex: bool) -> Vec<FindResult>{
        let mut result = Vec::<FindResult>::new();
        match self.file_manager_type {
            FileManagerType::RamOnly => {
                if use_regex {
                    let re = RegexBuilder::new(std::str::from_utf8(expr).unwrap()).case_insensitive(ignore_case).build().unwrap();
                    for cap in re.captures_iter(&self.file_buffer) {
                        if let Some(m) = cap.get(0) {
                            result.push(FindResult {start: m.start(), span: m.len()});
                        }
                    }

                } else if ignore_case {
                    let slice = expr.as_slice().to_ascii_lowercase();
                    for (i, w) in self.file_buffer.windows(expr.len()).enumerate() {
                        if w.to_ascii_lowercase() == slice {
                            result.push(FindResult {start: i, span: expr.len()});
                        }
                    }
                } else {
                    let slice = expr.as_slice();
                    for (i, w) in self.file_buffer.windows(expr.len()).enumerate() {
                        if w == slice {
                            result.push(FindResult {start: i, span: expr.len()});
                        }
                    }
                }
            }
            FileManagerType::SwapFile => {
                todo!()
            },
            FileManagerType::LiveEdit | FileManagerType::ReadOnly => {
                todo!()
            }
        } 
        
        result
    }
}