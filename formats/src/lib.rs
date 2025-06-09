use std::rc::Rc;
use std::sync::Arc;
use rlua::prelude::*;
use serde::{Deserialize, Serialize};
use imstr::ImString;

use anyhow::anyhow;
use bitflags::bitflags;

pub mod parser {
    pub mod slk;
    pub mod crlf;
    pub mod profile;
    pub mod w3obj;
}

pub mod error;
pub mod metadata;
pub mod object;
pub mod objectstore;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
/// A WC3 object id, which is conceptually a simple 32-bit integer,
/// but often represented as a 4-char ASCII string.
///
/// Provides conversion to/from byte arrays for this reason.
pub struct ObjectId {
    id: Vec<u8>,
}

impl ObjectId {
    pub fn new(id: u32) -> ObjectId {
        ObjectId { id: id.to_be_bytes().iter().copied().collect() }
    }

    pub fn is_zero(&self) -> bool {
        for i in &self.id {
            if *i != 0 {
                return false;
            }
        }
        true
    }

    pub fn from_str(str: &str) -> Self {
        Self::from_bytes(str.as_bytes())
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        ObjectId { id: bytes.to_vec() }
    }

    pub fn to_u32(&self) -> Option<u32> {
        if self.id.len() > 4 {
            None
        } else {
            let mut value = 0;
            for i in &self.id {
                value <<= 8;
                value += u32::from(*i);
            }
            for i in self.id.len()..4 {
                value <<= 8;
            }
            Some(value)
        }
    }

    pub fn to_string(&self) -> Option<String> {
        String::from_utf8(self.id.clone()).ok()
    }
}

impl std::fmt::Debug for ObjectId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.is_zero() {
            write!(f, "ObjectID(NULL)")
        } else {
            let pretty = std::str::from_utf8(&self.id).ok();

            if let Some(pretty) = pretty {
                write!(f, "ObjectID({})", pretty)
            } else {
                write!(f, "ObjectID({})", self.to_u32().unwrap())
            }
        }
    }
}

impl std::fmt::Display for ObjectId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.is_zero() {
            write!(f, "NULL")
        } else {
            let pretty = String::from_utf8_lossy(&self.id);

            write!(f, "{}", pretty)
        }
    }
}

impl From<u32> for ObjectId {
    fn from(other: u32) -> Self {
        ObjectId::new(other)
    }
}

impl<'lua> FromLua<'lua> for ObjectId {
    fn from_lua(value: LuaValue<'lua>, _ctx: LuaContext<'lua>) -> Result<Self, LuaError> {
        match value {
            LuaValue::String(value) => Ok(ObjectId::from_bytes(value.as_bytes())),
            LuaValue::Integer(value) => Ok(ObjectId::new(value as u32)),
            _ => Err(LuaError::external(anyhow!(
                "only strings and integers can be converted to object ids"
            ))),
        }
    }
}

impl<'lua> ToLua<'lua> for ObjectId {
    fn to_lua(self, ctx: LuaContext<'lua>) -> Result<LuaValue<'lua>, LuaError> {
        if let Some(value) = self.to_string() {
            Ok(LuaValue::String(ctx.create_string(&value)?))
        } else if let Some(value) = self.to_u32() {
            Ok(LuaValue::Integer(value as i64))
        } else {
            Err(LuaError::external(anyhow!("an unrepresentable object id")))
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, Hash)]
/// Represents a WC3 primitive data type.
///
/// WC3 field metadata specifies many more types than these,
/// but most of them collapse to strings.
pub enum ValueType {
    Int,
    Real,
    Unreal,
    String,
}

impl ValueType {
    /// Collapse a WC3 data type into a primitive value type.
    ///
    /// Mostly supposed to be used with data types specified in SLKs.
    pub fn new(input: &str) -> ValueType {
        match input {
            "real" => ValueType::Real,
            "unreal" => ValueType::Unreal,
            "int" | "bool" | "attackBits" | "deathType" | "defenseTypeInt" | "detectionType"
            | "teamColor" | "morphFlags" | "silenceFlags" | "stackFlags" | "interactionFlags"
            | "pickFlags" | "versionFlags" | "fullFlags" | "channelType" | "channelFlags"
            | "spellDetail" | "techAvail" => ValueType::Int,
            _ => ValueType::String,
        }
    }
}

bitflags! {
    #[derive(Serialize, Deserialize)]
    /// Represents a WC3 object type.
    pub struct ObjectKind: u32 {
        const ABILITY = 0b1;
        const BUFF = 0b10;
        const DESTRUCTABLE = 0b100;
        const MISC = 0b1000;
        const UNIT = 0b10000;
        const UPGRADE = 0b100000;
        const ITEM = 0b1000000;
        const DOODAD = 0b10000000;
        const LIGHTNING = 0b100000000; // fake
        const SOUND = 0b1000000000; // fake
    }
}

impl ObjectKind {
    /// Converts an extension of a WC3 object data file
    /// to its corresponding object type.
    pub fn from_ext(ext: &str) -> ObjectKind {
        match ext {
            "w3u" => ObjectKind::UNIT,
            "w3a" => ObjectKind::ABILITY,
            "w3t" => ObjectKind::ITEM,
            "w3b" => ObjectKind::DESTRUCTABLE,
            "w3d" => ObjectKind::DOODAD,
            "w3h" => ObjectKind::BUFF,
            "w3q" => ObjectKind::UPGRADE,
            "lightning" => ObjectKind::LIGHTNING,
            "sound" => ObjectKind::LIGHTNING,
            _ => ObjectKind::empty(),
        }
    }

    pub fn to_ext(self) -> &'static str {
        match self {
            ObjectKind::UNIT => "w3u",
            ObjectKind::ABILITY => "w3a",
            ObjectKind::ITEM => "w3t",
            ObjectKind::DESTRUCTABLE => "w3b",
            ObjectKind::DOODAD => "w3d",
            ObjectKind::BUFF => "w3h",
            ObjectKind::UPGRADE => "w3q",
            ObjectKind::LIGHTNING => "lightning",
            ObjectKind::SOUND => "sound",
            _ => "none",
        }
    }

    /// Returns true if the object type is capable
    /// of using data/leveled fields instead of just regular fields.
    ///
    /// This affects the layout of WC3 object data files.
    pub fn is_data_type(self) -> bool {
        match self {
            ObjectKind::DOODAD | ObjectKind::ABILITY | ObjectKind::UPGRADE => true,
            _ => false,
        }
    }

    pub fn to_typestr(self) -> &'static str {
        match self {
            ObjectKind::UNIT => "unit",
            ObjectKind::ABILITY => "ability",
            ObjectKind::ITEM => "item",
            ObjectKind::DESTRUCTABLE => "destructable",
            ObjectKind::DOODAD => "doodad",
            ObjectKind::BUFF => "buff",
            ObjectKind::UPGRADE => "upgrade",
            ObjectKind::LIGHTNING => "lightning",
            ObjectKind::SOUND => "sound",
            _ => "none",
        }
    }
}
