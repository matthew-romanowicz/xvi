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
    NoFile,
    RamOnly,
    SwapFile,
    LiveEdit,
    ReadOnly
}

pub enum FileManagerData {
    NoFile{file_buffer: Vec<u8>},
    RamOnly{handle: File, metadata: std::fs::Metadata, file_buffer: Vec<u8>},
    SwapFile{handle: File, metadata: std::fs::Metadata, swap_handle: File},
    LiveEdit{handle: File, metadata: std::fs::Metadata},
    ReadOnly{handle: File, metadata: std::fs::Metadata}
}

pub struct FileManager {
    pub filename: String,
    file_manager_type: FileManagerType,
    extract: bool,
    // handle: File,
    // metadata: std::fs::Metadata,
    modified: bool,
    block_size: usize, // Not used for FileManagerType::RamOnly
    max_match_length: usize, // Not used for FileManagerType::RamOnly
    data: FileManagerData
    // swap_handle: Option<File>, // Only used for FileManagerType::SwapFile
    // swap_metadata: Option<&'a std::fs::Metadata>, // Only used for FileManagerType::SwapFile
    // file_buffer: Vec<u8>, // Only used for FileManagerType::RamOnly
}

fn get_bytes_from_handle(handle: &mut File, index: usize, buffer: &mut Vec<u8>) -> std::io::Result<usize> {
    let file_length = handle.metadata()?.len() as usize;
    handle.seek(SeekFrom::Start(index as u64))?;
    if file_length >= index + buffer.len() {
        match handle.read_exact(buffer) {
            Ok(_) => Ok(buffer.len()),
            Err(msg) => Err(msg)
        }
    } else if file_length <= index {
        Ok(0)
    } else {
        let mut mini_buffer: Vec<u8> = vec![0; file_length - index];
        handle.read_exact(&mut mini_buffer)?;
        for (i, c) in mini_buffer.iter().enumerate() {
            buffer[i] = *c;
        }
        Ok(mini_buffer.len())
    }
}

fn truncate_from_handle(handle: &mut File, size: usize) -> std::io::Result<()> {
    handle.set_len(size as u64)
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


impl FileManager {
    pub fn new(filename: String, file_manager_type: FileManagerType, extract: bool) -> std::io::Result<FileManager>{

        let mut handle = File::options().read(true).write(!matches!(file_manager_type, FileManagerType::ReadOnly)).open(&filename)?;
        let metadata = handle.metadata()?;

        // let mut swap_handle = None;

        let mut file_buffer = vec![];
        let data = match file_manager_type {
            FileManagerType::RamOnly => {
                if extract {
                    //let mut temp_file_buffer = Vec::<u8>::new();
                    //handle.read_to_end(&mut temp_file_buffer);
                    let mut d = GzDecoder::new(&handle);
                    d.read_to_end(&mut file_buffer)?;
                } else {
                    handle.read_to_end(&mut file_buffer)?;
                }
                FileManagerData::RamOnly{handle, metadata, file_buffer}
            },
            FileManagerType::SwapFile => {
                let path = std::path::Path::new(&filename);

                let name = format!(".{}.swp", path.file_name().unwrap().to_str().unwrap().trim_matches('"'));
                let swap_path = path.with_file_name(name).to_str().unwrap().to_string();
                //println!("{}", swap_path);
                let mut swap = File::options().read(true).write(true).create(true).open(swap_path)?;
                let mut buffer = vec![0;65535];
                if extract {
                    let mut d = GzDecoder::new(&handle);
                    let mut num_read = d.read(&mut buffer)?;
                    while num_read == buffer.len() {
                        swap.write(&buffer)?;
                        num_read = d.read(&mut buffer)?;
                    }
                    unsafe {buffer.set_len(num_read);}
                    swap.write(&buffer)?;
                } else {
                    let mut num_read = handle.read(&mut buffer)?;
                    while num_read == buffer.len() {
                        swap.write(&buffer)?;
                        num_read = handle.read(&mut buffer)?;
                    }
                    unsafe {buffer.set_len(num_read);}
                    swap.write(&buffer)?;
                }
                // let swap_metadata = swap.metadata()?;
                // swap_handle = Some(swap);

                FileManagerData::SwapFile{handle, metadata, swap_handle: swap}
                
            },
            FileManagerType::LiveEdit => {
                FileManagerData::LiveEdit{handle, metadata}
            },
            FileManagerType::ReadOnly => {
                FileManagerData::ReadOnly{handle, metadata}
            },
            FileManagerType::NoFile => {
                todo!()
            }
        };

        Ok(FileManager {
            filename,
            file_manager_type,
            extract,
            // handle,
            // metadata,
            modified: false,
            block_size: 65535,
            max_match_length: 65535,
            // swap_handle: swap_handle,
            // swap_metadata: None,
            // file_buffer
            data
        })
    }

    pub fn from_buffer(buffer: Vec<u8>) -> FileManager {
        FileManager {
            filename: "".to_string(),
            file_manager_type: FileManagerType::NoFile,
            extract: false,
            modified: false,
            block_size: 65535,
            max_match_length: 65535,
            data: FileManagerData::NoFile{file_buffer: buffer}
        }
    }

    pub fn set_block_size(&mut self, block_size: usize) {
        self.block_size = block_size;
    }

    pub fn is_modified(&self) -> bool {
        match self.file_manager_type {
            FileManagerType::RamOnly | FileManagerType::SwapFile => {
                self.modified
            },
            FileManagerType::LiveEdit | FileManagerType::ReadOnly | FileManagerType::NoFile => {
                false
            }
        }
    }

    pub fn save(&mut self) -> std::io::Result<()>{
        match &mut self.data {
            FileManagerData::RamOnly{ref mut handle, file_buffer, ..} => {
                handle.seek(SeekFrom::Start(0))?;
                if self.extract {
                    let mut d = GzEncoder::new(Vec::new(), Compression::default());
                    d.write_all(&file_buffer)?;
                    handle.write_all(&d.finish()?)?;
                } else {
                    handle.write_all(&file_buffer)?;
                }
                let n = handle.seek(SeekFrom::Current(0))?;
                handle.set_len(n)?;
                self.modified = false;
                Ok(())
            },
            FileManagerData::SwapFile{ref mut handle, ref mut swap_handle, ..} => { 
                handle.seek(SeekFrom::Start(0))?;
                swap_handle.seek(SeekFrom::Start(0))?;
                let mut buffer = vec![0;self.block_size];
                if self.extract {// TODO: Make this work
                    //let b = std::io::BufReader::new(swap_handle);
                    let mut d = GzEncoder::new(swap_handle, Compression::default());
                    let mut num_read = d.read(&mut buffer)?;
                    while num_read == buffer.len() {
                        handle.write(&buffer)?;
                        d.read(&mut buffer)?;
                    }
                    unsafe {buffer.set_len(num_read);}
                    handle.write(&buffer)?;
                    // ("Done writing!");
                } else {
                    let mut num_read = swap_handle.read(&mut buffer)?;
                    while num_read == buffer.len() {
                        handle.write(&buffer)?;
                        swap_handle.read(&mut buffer)?;
                    }
                    unsafe {buffer.set_len(num_read);}
                    handle.write(&buffer)?;
                }
                let n = handle.seek(SeekFrom::Current(0))?;
                handle.set_len(n)?;
                Ok(())
            },
            FileManagerData::LiveEdit{..} | FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only and live modes".to_string()))
            },
            FileManagerData::NoFile{..} => todo!()
        }
    }

    pub fn get_bytes(&mut self, index: usize, buffer: &mut Vec<u8>) -> std::io::Result<usize> {
        match &mut self.data {
            FileManagerData::RamOnly{file_buffer, ..} | FileManagerData::NoFile{file_buffer} => {
                let n = buffer.len();
                if file_buffer.len() >= index + n {
                    buffer.copy_from_slice(&file_buffer[index..(index + n)]);
                    Ok(n)
                } else if index < file_buffer.len() {
                    for (i, c) in file_buffer[index..file_buffer.len()].iter().enumerate() {
                        buffer[i] = *c;
                    }
                    Ok(file_buffer.len() - index)
                } else {
                    Ok(0)
                }
            },
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                get_bytes_from_handle(swap_handle, index, buffer)
            },
            FileManagerData::LiveEdit{ref mut handle, ..} | FileManagerData::ReadOnly{ref mut handle, ..} => {
                get_bytes_from_handle(handle, index, buffer)
            }
        }
    }

    pub fn truncate(&mut self, size: usize) -> std::io::Result<()> {
        if size > self.len() {
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "Cannot truncate to size greater than file length".to_string()));
        }
        self.modified = true;
        match &mut self.data {
            FileManagerData::RamOnly{ref mut file_buffer, ..} | FileManagerData::NoFile{ref mut file_buffer} => {
                unsafe {file_buffer.set_len(size);}
                Ok(())
            },
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                truncate_from_handle(swap_handle, size)
            },
            FileManagerData::LiveEdit{ref mut handle, ..} => {
                truncate_from_handle(handle, size)
            },
            FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn overwrite_byte(&mut self, index: usize, c: u8)  -> std::io::Result<()> {
        self.modified = true;
        match &mut self.data {
            FileManagerData::RamOnly{ref mut file_buffer, ..} | FileManagerData::NoFile{ref mut file_buffer} => {
                file_buffer[index] = c;
                Ok(())
            },
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                overwrite_bytes_from_handle(swap_handle, index, &vec![c])
            },
            FileManagerData::LiveEdit{ref mut handle, ..} => {
                overwrite_bytes_from_handle(handle, index, &vec![c])
            },
            FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn insert_byte(&mut self, index: usize, c: u8)  -> std::io::Result<()> {
        self.modified = true;
        match &mut self.data {
            FileManagerData::RamOnly{ref mut file_buffer, ..} | FileManagerData::NoFile{ref mut file_buffer} => {
                file_buffer.insert(index, c);
                Ok(())
            },
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                insert_bytes_from_handle(swap_handle, index, &vec![c], self.block_size)
            },
            FileManagerData::LiveEdit{ref mut handle, ..} => {
                insert_bytes_from_handle(handle, index, &vec![c], self.block_size)
            },
            FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn delete_byte(&mut self, index: usize)  -> std::io::Result<()> {
        self.modified = true;
        match &mut self.data {
            FileManagerData::RamOnly{ref mut file_buffer, ..}  | FileManagerData::NoFile{ref mut file_buffer}=> {
                file_buffer.remove(index);
                Ok(())
            },
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                delete_bytes_from_handle(swap_handle, index, 1, self.block_size)
            },
            FileManagerData::LiveEdit{ref mut handle, ..} => {
                delete_bytes_from_handle(handle, index, 1, self.block_size)
            },
            FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn get_byte(&mut self, index: usize) -> Option<u8> {
        match &mut self.data {
            FileManagerData::RamOnly{file_buffer, ..} | FileManagerData::NoFile{file_buffer} => {
                if index < file_buffer.len() {
                    Some(file_buffer[index])
                } else {
                    None
                }
            },
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                let mut buffer: Vec<u8> = vec![0];
                match get_bytes_from_handle(swap_handle, index, &mut buffer) {
                    Ok(1) => Some(buffer[0]),
                    _ => None
                }
            },
            FileManagerData::LiveEdit{ref mut handle, ..} | FileManagerData::ReadOnly{ref mut handle, ..} => {
                let mut buffer: Vec<u8> = vec![0];
                match get_bytes_from_handle(handle, index, &mut buffer) {
                    Ok(1) => Some(buffer[0]),
                    _ => None
                }
            }
        }
    }

    pub fn insert_bytes(&mut self, index: usize, bytes: &Vec<u8>)  -> std::io::Result<()> {
        self.modified = true;
        match &mut self.data {
            FileManagerData::RamOnly{ref mut file_buffer, ..} | FileManagerData::NoFile{ref mut file_buffer} => {
                let second_part = file_buffer[index..file_buffer.len()].to_vec();
                unsafe {file_buffer.set_len(index)};
                file_buffer.extend(bytes);
                file_buffer.extend(second_part);
                Ok(())
            }
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                insert_bytes_from_handle(swap_handle, index, bytes, self.block_size)
            },
            FileManagerData::LiveEdit{ref mut handle, ..} => {
                insert_bytes_from_handle(handle, index, bytes, self.block_size)
            },
            FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn overwrite_bytes(&mut self, index: usize, bytes: &Vec<u8>)  -> std::io::Result<()> {
        self.modified = true;
        match &mut self.data {
            FileManagerData::RamOnly{ref mut file_buffer, ..} | FileManagerData::NoFile{ref mut file_buffer}=> {
                for (i, c) in bytes.iter().enumerate() {
                    file_buffer[i + index] = *c
                }
                Ok(())
            }
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                overwrite_bytes_from_handle(swap_handle, index, bytes)
            },
            FileManagerData::LiveEdit{ref mut handle, ..} => {
                overwrite_bytes_from_handle(handle, index, bytes)
            },
            FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn append_bytes(&mut self, bytes: &Vec<u8>) -> std::io::Result<()> {
        self.modified = true;
        match &mut self.data {
            FileManagerData::RamOnly{ref mut file_buffer, ..} | FileManagerData::NoFile{ref mut file_buffer} => {
                file_buffer.extend(bytes);
                Ok(())
            },
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                append_bytes_from_handle(swap_handle, bytes)
            },
            FileManagerData::LiveEdit{ref mut handle, ..} => {
                append_bytes_from_handle(handle, bytes)
            },
            FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn delete_bytes(&mut self, index: usize, n_bytes: usize) -> std::io::Result<()> {
        self.modified = true;
        match &mut self.data {
            FileManagerData::RamOnly{ref mut file_buffer, ..} | FileManagerData::NoFile{ref mut file_buffer} => {
                let second_part = file_buffer[(index + n_bytes)..file_buffer.len()].to_vec();
                unsafe {file_buffer.set_len(index)};
                file_buffer.extend(second_part);
                Ok(())
            }
            FileManagerData::SwapFile{ref mut swap_handle, ..} => {
                delete_bytes_from_handle(swap_handle, index, n_bytes as u64, self.block_size)
            },
            FileManagerData::LiveEdit{ref mut handle, ..} => {
                delete_bytes_from_handle(handle, index, n_bytes as u64, self.block_size)
            },
            FileManagerData::ReadOnly{..} => {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "Operation not valid in read-only mode".to_string()))
            }
        }
    }

    pub fn len(&self) -> usize{
        match &self.data {
            FileManagerData::RamOnly{file_buffer, ..} | FileManagerData::NoFile{file_buffer} => {
                file_buffer.len()
            }
            FileManagerData::SwapFile{swap_handle, ..} => {
                swap_handle.metadata().expect("Error reading metadata").len() as usize // TODO make this more safe

            },
            FileManagerData::LiveEdit{handle, ..} | FileManagerData::ReadOnly{handle, ..} => {
                handle.metadata().expect("Error reading metadata").len() as usize // TODO make this more safe
            }
        }        
    }

    pub fn find(&mut self, expr: &Vec<u8>, ignore_case: bool, use_regex: bool) -> Vec<FindResult> {
        let mut result = Vec::<FindResult>::new();
        match &mut self.data {
            FileManagerData::RamOnly{file_buffer, ..} | FileManagerData::NoFile{file_buffer} => {
                if use_regex {
                    let re = RegexBuilder::new(std::str::from_utf8(expr).unwrap()).case_insensitive(ignore_case).build().unwrap();
                    for cap in re.captures_iter(&file_buffer) {
                        if let Some(m) = cap.get(0) {
                            result.push(FindResult {start: m.start(), span: m.len()});
                        }
                    }

                } else if ignore_case {
                    let slice = expr.as_slice().to_ascii_lowercase();
                    for (i, w) in file_buffer.windows(expr.len()).enumerate() {
                        if w.to_ascii_lowercase() == slice {
                            result.push(FindResult {start: i, span: expr.len()});
                        }
                    }
                } else {
                    let slice = expr.as_slice();
                    for (i, w) in file_buffer.windows(expr.len()).enumerate() {
                        if w == slice {
                            result.push(FindResult {start: i, span: expr.len()});
                        }
                    }
                }
            },
            FileManagerData::SwapFile{..} | FileManagerData::LiveEdit{..} | FileManagerData::ReadOnly{..} => {
                // This works by iterating through blocks twize the size of the maximum match size with an interval
                // of the maximum match size. This means that each byte appears in two blocks and that every group of
                // n contiguous bytes appears contiguously in at least one block, where n is the maximum match size.
                // If the last match in one block extends into the start of the would-be next block, the next block will
                // start at the end of that match instead to save some efficiency and ensure matches aren't duplicated.
                //
                // For non-regex matches, the max match size used is just the length of the expression, since every match
                // will be that length.
                if use_regex {
                    let re = RegexBuilder::new(std::str::from_utf8(expr).unwrap()).case_insensitive(ignore_case).build().unwrap();
                    let mut block_buffer = vec![0; self.max_match_length * 2];
                    let mut index = 0;
                    let mut last_block = false;
                    while !last_block {
                        
                        let n = self.get_bytes(index, &mut block_buffer).unwrap();
                        if n < block_buffer.len() {
                            block_buffer.truncate(n);
                            last_block = true;
                        } 

                        let mut next_index = index + self.max_match_length;

                        for cap in re.captures_iter(&block_buffer) {
                            if let Some(m) = cap.get(0) {
                                result.push(FindResult {start: index + m.start(), span: m.len()});
                                if index + m.end() > next_index {
                                    next_index = index + m.end();
                                }
                            }
                        }

                        index = next_index;
                        
                    }
                } else {
                    let slice = match ignore_case {
                        true => expr.as_slice().to_ascii_lowercase(),
                        false => expr.to_vec()
                    };

                    let mut block_buffer = vec![0; slice.len() * 2];
                    let mut index = 0;
                    let mut last_block = false;
                    while !last_block {
                        
                        let n = self.get_bytes(index, &mut block_buffer).unwrap();
                        if n < block_buffer.len() {
                            block_buffer.truncate(n);
                            last_block = true;
                        } 

                        let mut next_index = index + self.max_match_length;

                        if ignore_case {
                            block_buffer = block_buffer.to_ascii_lowercase();
                        }

                        for (i, w) in block_buffer.windows(expr.len()).enumerate() {
                            if w == slice {
                                result.push(FindResult {start: index + i, span: expr.len()});
                                if index + i + expr.len() > next_index {
                                    next_index = index + i + expr.len();
                                }
                            }
                        }

                        index = next_index;
                        
                    }
                }
            }
        } 
        
        result
    }

    pub fn find_bytes(&self, bytes: &Vec<u8>) -> Vec<FindResult> {
        let mut result = Vec::<FindResult>::new();
        match &self.data {
            FileManagerData::RamOnly{file_buffer, ..} | FileManagerData::NoFile{file_buffer} => {
                let slice = bytes.as_slice();
                for (i, w) in file_buffer.windows(bytes.len()).enumerate() {
                    if w == slice {
                        result.push(FindResult {start: i, span: bytes.len()});
                    }
                }

            }
            FileManagerData::SwapFile{..} => {
                todo!()
            },
            FileManagerData::LiveEdit{..} | FileManagerData::ReadOnly{..} => {
                todo!()
            }
        } 
        
        result
    }
}