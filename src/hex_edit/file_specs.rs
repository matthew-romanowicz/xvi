

pub struct BinaryField {
    pub name: String,
    pub value: Option<String>,
    pub start: usize,
    pub span: usize
}

pub trait FileSpec {
    fn new(fm: &mut crate::hex_edit::FileManager) -> Self where Self: Sized;

    fn field_at(self: &mut Self, index: usize, fm: &mut crate::hex_edit::FileManager) -> Option<BinaryField>;
}

pub struct PngFileSpec {
    chunks: Vec<usize>,
    chunk_types: Vec<[u8; 4]>
}

impl FileSpec for PngFileSpec {
    fn new(fm: &mut crate::hex_edit::FileManager) -> Self {
        let file_length = fm.len();
        let mut buffer_32 = vec![0; 4];
        let mut chunks = Vec::new();
        let mut chunk_types = Vec::new();
        let mut index = 8;
        while index + 4 < file_length {
            chunks.push(index);
            fm.get_bytes(index, &mut buffer_32);
            let chunk_length = u32::from_be_bytes([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]) + 12;
            fm.get_bytes(index + 4, &mut buffer_32);
            chunk_types.push([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]);
            index += chunk_length as usize;
        }
        
        PngFileSpec {
            chunks,
            chunk_types
        }
    }

    fn field_at(self: &mut Self, index: usize, fm: &mut crate::hex_edit::FileManager) -> Option<BinaryField> {
        if index < 8 {
            Some(
                BinaryField {
                    name: "PNG File Signature".to_string(),
                    value: None,
                    start: 0,
                    span: 8
                }
            )
        } else {
            let chunk = bisection::bisect_right(&self.chunks, &index) - 1;
            let chunk_index = self.chunks[chunk];
            let chunk_diff = self.chunks[chunk + 1] - chunk_index;
            if index >= chunk_index {
                match index - chunk_index {
                    0..=3 => {
                        let mut buffer_32 = vec![0; 4];
                        fm.get_bytes(chunk_index, &mut buffer_32);
                        let chunk_length = u32::from_be_bytes([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]);
                        Some(
                            BinaryField {
                                name: format!("Chunk #{} Length", chunk).to_string(),
                                value: Some(format!("{} Bytes", chunk_length).to_string()),
                                start: chunk_index,
                                span: 4
                            }
                        )
                    },
                    4..=7 => {
                        let mut buffer_32 = vec![0; 4];
                        fm.get_bytes(chunk_index + 4, &mut buffer_32);
                        let value = match String::from_utf8(buffer_32) {
                            Ok(s) => Some(s),
                            Err(_) => None
                        };
                        Some(
                            BinaryField {
                                name: format!("Chunk #{} Type", chunk).to_string(),
                                value,
                                start: chunk_index + 4,
                                span: 4
                            }
                        )
                    },
                    i if i + 4 >= chunk_diff => {
                        Some(
                            BinaryField {
                                name: format!("Chunk #{} CRC", chunk).to_string(),
                                value: None,
                                start: chunk_index + chunk_diff - 4,
                                span: 4
                            }
                        )
                    },
                    i => {
                        let data_start = chunk_index + 8;
                        let i = i - 8;
                        match self.chunk_types[chunk] {
                            [b'I', b'H', b'D', b'R'] => {
                                match i {
                                    0..=3 => {
                                        let mut buffer_32 = vec![0; 4];
                                        fm.get_bytes(data_start, &mut buffer_32);
                                        let value = u32::from_be_bytes([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]);
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Width").to_string(),
                                                value: Some(format!("{}", value).to_string()),
                                                start: data_start,
                                                span: 4
                                            }
                                        )
                                    },
                                    4..=7 => {
                                        let mut buffer_32 = vec![0; 4];
                                        fm.get_bytes(data_start + 4, &mut buffer_32);
                                        let value = u32::from_be_bytes([buffer_32[0], buffer_32[1], buffer_32[2], buffer_32[3]]);
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Height").to_string(),
                                                value: Some(format!("{}", value).to_string()),
                                                start: data_start + 4,
                                                span: 4
                                            }
                                        )
                                    },
                                    8 => {
                                        let value = match fm.get_byte(data_start + 8) {
                                            Some(v) => Some(format!("{}", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Bit Depth").to_string(),
                                                value,
                                                start: data_start + 8,
                                                span: 1
                                            }
                                        )
                                    },
                                    9 => {
                                        let value = match fm.get_byte(data_start + 9) {
                                            Some(0) => Some(format!("{} (Grayscale)", 0).to_string()),
                                            Some(2) => Some(format!("{} (Truecolor)", 2).to_string()),
                                            Some(3) => Some(format!("{} (Indexed)", 3).to_string()),
                                            Some(4) => Some(format!("{} (Grayscale and Alpha)", 4).to_string()),
                                            Some(6) => Some(format!("{} (Truecolor and Alpha)", 6).to_string()),
                                            Some(v) => Some(format!("{} (Invalid)", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Color Type").to_string(),
                                                value,
                                                start: data_start + 9,
                                                span: 1
                                            }
                                        )
                                    },
                                    10 => {
                                        let value = match fm.get_byte(data_start + 10) {
                                            Some(0) => Some(format!("{}", 0).to_string()),
                                            Some(v) => Some(format!("{} (Invalid)", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Compression Method").to_string(),
                                                value,
                                                start: data_start + 10,
                                                span: 1
                                            }
                                        )
                                    },
                                    11 => {
                                        let value = match fm.get_byte(data_start + 11) {
                                            Some(0) => Some(format!("{}", 0).to_string()),
                                            Some(v) => Some(format!("{} (Invalid)", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Filter Method").to_string(),
                                                value,
                                                start: data_start + 11,
                                                span: 1
                                            }
                                        )
                                    },
                                    12 => {
                                        let value = match fm.get_byte(data_start + 12) {
                                            Some(0) => Some(format!("{} (No Interlace)", 0).to_string()),
                                            Some(1) => Some(format!("{} (Adam7 Interlace)", 1).to_string()),
                                            Some(v) => Some(format!("{} (Invalid)", v).to_string()),
                                            None => None
                                        };
                                        Some(
                                            BinaryField {
                                                name: format!("IHDR Interlace Method").to_string(),
                                                value,
                                                start: data_start + 12,
                                                span: 1
                                            }
                                        )
                                    },
                                    _ => None
                                }
                            },
                            _ => None
                        }
                    }
                }
            } else {
                panic!("Unexpected condition: {}, {}, {}", index, chunk_index, chunk)
            }
        }
    }
}