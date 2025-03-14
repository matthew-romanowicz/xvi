use std::rc::Rc;

use bitutils2::{BitIndex, BitField, BitIndexable};

use crate::globals::{MACRO_ARRAY_LENGTH, MARKS_ARRAY_LENGTH, REGISTER_ARRAY_LENGTH};
use crate::utils::{BoundedVec, BoundedIndex};
use crate::hex_edit::CompoundAction;



// const MARKS_ARRAY_LENGTH_X2: usize = MARKS_ARRAY_LENGTH * 2;

pub type MacroArray = BoundedVec<MACRO_ARRAY_LENGTH, Option<Rc<CompoundAction>>>;
pub type MacroId = BoundedIndex<MACRO_ARRAY_LENGTH>;

pub type MarkArray = BoundedVec<MARKS_ARRAY_LENGTH, BitIndex>;
pub type MarkId = BoundedIndex<MARKS_ARRAY_LENGTH>;

pub type RegisterArray = BoundedVec<REGISTER_ARRAY_LENGTH, BitField>;
pub type RegisterId = BoundedIndex<REGISTER_ARRAY_LENGTH>;

#[derive(Clone, Copy)]
pub enum FullMarkId {
    Lower(MarkId),
    Upper(MarkId)
    
}

impl FullMarkId {
    pub fn new(index: usize) -> Result<FullMarkId, usize> {
        if let Ok(mark_id) = MarkId::new(index) {
            Ok(FullMarkId::Lower(mark_id))
        } else if let Ok(mark_id) = MarkId::new(index - MarkId::BOUND) {
            Ok(FullMarkId::Upper(mark_id))
        } else {
            Err(MarkId::BOUND * 2)
        }
    }

    pub fn zero() -> FullMarkId {
        FullMarkId::Lower(MarkId::zero())
    }
}

#[derive(Clone, Copy)]
pub enum FullRegisterId {
    Lower(RegisterId),
    Upper(RegisterId)
    
}

impl FullRegisterId {
    pub fn new(index: usize) -> Result<FullRegisterId, usize> {
        if let Ok(reg_id) = RegisterId::new(index) {
            Ok(FullRegisterId::Lower(reg_id))
        } else if let Ok(reg_id) = RegisterId::new(index - RegisterId::BOUND) {
            Ok(FullRegisterId::Upper(reg_id))
        } else {
            Err(RegisterId::BOUND * 2)
        }
    }

    pub fn zero() -> FullRegisterId {
        FullRegisterId::Lower(RegisterId::zero())
    }
}

pub enum FillType {
    Bytes(BitField),
    Register(RegisterId)
}

pub enum FullFillType {
    Bytes(BitField),
    Register(FullRegisterId)
}

impl FullFillType {
    pub fn convert(self, registers: &RegisterArray) -> FillType {
        match self {
            FullFillType::Bytes(b) => FillType::Bytes(b),
            FullFillType::Register(FullRegisterId::Upper(reg_id)) => {
                FillType::Bytes(registers[reg_id].clone())
            },
            FullFillType::Register(FullRegisterId::Lower(reg_id)) => FillType::Register(reg_id)
        }
    }
}

pub enum DataSource {
    Bytes(Vec<u8>),
    Fill(usize),
    Register(RegisterId)
}

pub enum FullDataSource {
    Bytes(Vec<u8>),
    Fill(usize),
    Register(FullRegisterId)
}

impl FullDataSource {
    pub fn convert(self, registers: &RegisterArray) -> DataSource {
        match self {
            FullDataSource::Bytes(b) => DataSource::Bytes(b),
            FullDataSource::Fill(n) => DataSource::Fill(n),
            FullDataSource::Register(FullRegisterId::Upper(reg_id)) => {
                let v = registers[reg_id].clone().into_boxed_slice().unwrap().to_vec(); // TODO: Make this work for non-byte aligned
                DataSource::Bytes(v)
            },
            FullDataSource::Register(FullRegisterId::Lower(reg_id)) => DataSource::Register(reg_id)
        }
    }
}

#[derive(Clone)]
pub enum RangeSize {
    Bytes(usize),
    UntilAddr(BitIndex),
    UntilMark(MarkId)
}

#[derive(Clone)]
pub enum FullRangeSize {
    Bytes(usize),
    UntilAddr(BitIndex),
    UntilMark(FullMarkId)
}

impl FullRangeSize {
    pub fn convert(self, marks: &MarkArray) -> RangeSize {
        match self {
            FullRangeSize::Bytes(b) => RangeSize::Bytes(b),
            FullRangeSize::UntilAddr(bi) => RangeSize::UntilAddr(bi),
            FullRangeSize::UntilMark(FullMarkId::Upper(mark_id)) => {
                RangeSize::UntilAddr(marks[mark_id])
            },
            FullRangeSize::UntilMark(FullMarkId::Lower(mark_id)) => RangeSize::UntilMark(mark_id)
        }
    }
}

#[derive(Clone, Copy)]
pub enum Seek {
    FromStart(u64),
    FromCurrent(i64),
    FromEnd(i64),
    Mark(MarkId)
}

#[derive(Clone, Copy)]
pub enum FullSeek {
    FromStart(u64),
    FromCurrent(i64),
    FromEnd(i64),
    Mark(FullMarkId)
}

impl FullSeek {
    pub fn convert(self, marks: &MarkArray) -> Seek {
        match self {
            FullSeek::FromStart(i) => Seek::FromStart(i),
            FullSeek::FromCurrent(i) => Seek::FromCurrent(i),
            FullSeek::FromEnd(i) => Seek::FromEnd(i),
            FullSeek::Mark(FullMarkId::Lower(mark_id)) => Seek::Mark(mark_id),
            FullSeek::Mark(FullMarkId::Upper(mark_id)) => {
                // TODO: Make this work with BitIndex
                Seek::FromStart(marks[mark_id].byte() as u64)
            }
        }
    }
}