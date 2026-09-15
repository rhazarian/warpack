use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::metadata::FieldVariant;
use crate::metadata::MetadataStore;
use crate::ObjectId;
use crate::ObjectKind;
use crate::parser::slk;
use crate::ValueType;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    String(String),
    Int(i32),
    Real(f32),
    Unreal(f32),
}

impl Value {
    pub fn default(type_id: u32) -> Option<Self> {
        match type_id {
            0 => Some(Value::Unreal(0.0)),
            1 => Some(Value::Real(0.0)),
            2 => Some(Value::Int(0)),
            3 => Some(Value::String("".into())),
            _ => None
        }
    }

    pub fn from_str_and_ty(value: &str, ty: ValueType) -> Option<Self> {
        Some(match ty {
            ValueType::Unreal => Value::Unreal(value.parse().ok()?),
            ValueType::Real => Value::Real(value.parse().ok()?),
            ValueType::Int => Value::Int(value.parse().ok()?),
            ValueType::String => Value::String(value.into()),
        })
    }

    pub fn type_id(&self) -> u32 {
        match self {
            Value::Int(..) => 0,
            Value::Real(..) => 1,
            Value::Unreal(..) => 2,
            Value::String(..) => 3,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum ValueKind {
    Common,
    SD,
    HD,
    DE,
}

impl ValueKind {
    /// Graphics-mode suffix used in profile (`war3mapSkin.txt`) keys
    /// and in Lua field names, e.g. `Art:hd`.
    pub fn suffix(self) -> &'static str {
        match self {
            ValueKind::Common => "",
            ValueKind::SD => ":sd",
            ValueKind::HD => ":hd",
            ValueKind::DE => ":de",
        }
    }

    /// All graphics-mode-specific kinds, i.e. everything except `Common`.
    pub const GRAPHICS_MODES: [ValueKind; 3] = [ValueKind::SD, ValueKind::HD, ValueKind::DE];

    /// Parses a graphics-mode name such as `"sd"`, `"hd"` or `"de"` (case-insensitive).
    pub fn from_mode_name(name: &str) -> Option<ValueKind> {
        match name.to_ascii_lowercase().as_str() {
            "sd" => Some(ValueKind::SD),
            "hd" => Some(ValueKind::HD),
            "de" => Some(ValueKind::DE),
            _ => None,
        }
    }

    /// Splits a field name with an optional graphics-mode suffix
    /// (`Art:hd` -> (`Art`, `HD`)) into the bare name and the value kind.
    pub fn split_field_name(key: &str) -> (&str, ValueKind) {
        for kind in Self::GRAPHICS_MODES {
            if let Some(name) = key.strip_suffix(kind.suffix()) {
                return (name, kind);
            }
        }
        (key, ValueKind::Common)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LeveledValue {
    pub level: u32,
    pub value: Option<Value>,
    pub value_sd: Option<Value>,
    pub value_hd: Option<Value>,
    pub value_de: Option<Value>,
}

impl LeveledValue {
    /// Returns the value stored exactly for the given kind, without
    /// falling back to the common value.
    pub fn value_of(&self, value_kind: ValueKind) -> Option<&Value> {
        match value_kind {
            ValueKind::Common => self.value.as_ref(),
            ValueKind::SD => self.value_sd.as_ref(),
            ValueKind::HD => self.value_hd.as_ref(),
            ValueKind::DE => self.value_de.as_ref(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FieldKind {
    Simple { value: Option<Value>, value_sd: Option<Value>, value_hd: Option<Value>, value_de: Option<Value> },
    Leveled { values: Vec<LeveledValue> },
}

impl FieldKind {
    /// For a simple field, returns the value stored exactly for the given kind,
    /// without falling back to the common value. Returns `None` for leveled fields.
    pub fn simple_value_of(&self, value_kind: ValueKind) -> Option<&Value> {
        match self {
            FieldKind::Simple { value, value_sd, value_hd, value_de } => match value_kind {
                ValueKind::Common => value.as_ref(),
                ValueKind::SD => value_sd.as_ref(),
                ValueKind::HD => value_hd.as_ref(),
                ValueKind::DE => value_de.as_ref(),
            },
            FieldKind::Leveled { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Field {
    pub id: ObjectId,
    pub kind: FieldKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Object {
    kind: ObjectKind,
    id: ObjectId,
    aliased_id: Option<ObjectId>,
    parent_id: Option<ObjectId>,
    fields: BTreeMap<ObjectId, Field>,
    dirty: bool,
}

impl Object {
    pub fn new(id: ObjectId, kind: ObjectKind) -> Object {
        Object {
            id,
            kind,
            aliased_id: None,
            parent_id: None,
            fields: Default::default(),
            dirty: true,
        }
    }

    pub fn with_parent(id: ObjectId, parent_id: ObjectId, kind: ObjectKind) -> Object {
        Object {
            id,
            kind,
            aliased_id: None,
            parent_id: Some(parent_id),
            fields: Default::default(),
            dirty: true,
        }
    }

    pub fn id(&self) -> ObjectId {
        self.id.clone()
    }

    pub fn set_id(&mut self, id: ObjectId) {
        self.id = id
    }

    pub fn aliased_id(&self) -> Option<ObjectId> {
        self.aliased_id.clone()
    }

    pub fn set_aliased_id(&mut self, aliased_id: Option<ObjectId>) {
        self.aliased_id = aliased_id
    }

    pub fn parent_id(&self) -> Option<ObjectId> {
        self.parent_id.clone()
    }

    pub fn set_parent_id(&mut self, parent_id: Option<ObjectId>) {
        self.parent_id = parent_id
    }

    pub fn kind(&self) -> ObjectKind {
        self.kind
    }

    pub fn is_dirty(&self) -> bool {
        self.dirty
    }

    pub fn set_dirty(&mut self, dirty: bool) {
        self.dirty = dirty
    }

    pub fn fields(&self) -> impl Iterator<Item=(&ObjectId, &Field)> {
        self.fields.iter()
    }

    pub fn field(&self, id: ObjectId) -> Option<&Field> {
        self.fields.get(&id)
    }

    pub fn simple_field(&self, id: &ObjectId, value_kind: ValueKind) -> Option<&Value> {
        self.fields.get(id).and_then(|field| match &field.kind {
            FieldKind::Simple { value, value_sd, value_hd, value_de } =>
                match value_kind {
                    ValueKind::Common => value.as_ref(),
                    ValueKind::SD => value_sd.as_ref().or(value.as_ref()),
                    ValueKind::HD => value_hd.as_ref().or(value.as_ref()),
                    ValueKind::DE => value_de.as_ref().or(value.as_ref()),
                }
            _ => None,
        })
    }

    pub fn leveled_field(&self, id: &ObjectId, level: u32, value_kind: ValueKind) -> Option<&Value> {
        self.fields.get(id).and_then(|field| match &field.kind {
            FieldKind::Leveled { values } => values
                .iter()
                .find(|value| value.level == level)
                .and_then(|value| match value_kind {
                    ValueKind::Common => value.value.as_ref(),
                    ValueKind::SD => value.value_sd.as_ref().or(value.value.as_ref()),
                    ValueKind::HD => value.value_hd.as_ref().or(value.value.as_ref()),
                    ValueKind::DE => value.value_de.as_ref().or(value.value.as_ref()),
                }),
            _ => None,
        })
    }

    pub fn unset_simple_field(&mut self, id: &ObjectId) {
        self.dirty = true;

        self.fields.remove(id);
    }

    pub fn unset_leveled_field(&mut self, id: &ObjectId, level: u32) {
        self.dirty = true;

        if let Some(field) = self.fields.get_mut(id) {
            if let FieldKind::Leveled { values } = &mut field.kind {
                values.retain(|dv| dv.level != level)
            }
        }
    }

    pub fn set_simple_field(&mut self, id: &ObjectId, value: Value, value_kind: ValueKind) {
        self.dirty = true;

        let field = self.fields.entry(id.clone()).or_insert_with(|| Field {
            id: id.clone(),
            kind: FieldKind::Simple {
                value: None,
                value_sd: None,
                value_hd: None,
                value_de: None,
            },
        });

        match &mut field.kind {
            FieldKind::Simple { value: value_ref, value_sd: value_sd_ref, value_hd: value_hd_ref, value_de: value_de_ref } => {
                match value_kind {
                    ValueKind::Common => value_ref,
                    ValueKind::SD => value_sd_ref,
                    ValueKind::HD => value_hd_ref,
                    ValueKind::DE => value_de_ref,
                }.replace(value);
            },
            FieldKind::Leveled { .. } => eprintln!(
                "tried to insert simple field {} for object {}, but a data field {} already exists",
                    id, self.id, field.id
            ),
        }
    }

    pub fn set_leveled_field(&mut self, id: &ObjectId, level: u32, value: Value, value_kind: ValueKind) {
        self.dirty = true;

        let field = self.fields.entry(id.clone()).or_insert_with(|| Field {
            id: id.clone(),
            kind: FieldKind::Leveled {
                values: Default::default(),
            },
        });

        match &mut field.kind {
            FieldKind::Simple { .. } => eprintln!(
                "tried to insert data field {} for object {}, but a simple field {} already exists",
                id, self.id, field.id
            ),
            FieldKind::Leveled { values } => {
                if let Some(leveled_value) = values.iter_mut().find(|dv| dv.level == level) {
                    match value_kind {
                        ValueKind::Common => leveled_value.value = Some(value),
                        ValueKind::SD => leveled_value.value_sd = Some(value),
                        ValueKind::HD => leveled_value.value_hd = Some(value),
                        ValueKind::DE => leveled_value.value_de = Some(value),
                    }
                } else {
                    values.push(
                        LeveledValue {
                            level,
                            value: if value_kind == ValueKind::Common {
                                Some(value.clone())
                            } else {
                                None
                            },
                            value_sd: if value_kind == ValueKind::SD {
                                Some(value.clone())
                            } else {
                                None
                            },
                            value_hd: if value_kind == ValueKind::HD {
                                Some(value.clone())
                            } else {
                                None
                            },
                            value_de: if value_kind == ValueKind::DE {
                                Some(value.clone())
                            } else {
                                None
                            },
                        }
                    );
                }
            }
        }
    }

    /// Merges this object's data with another object's data
    /// Doesn't do field-level merging because it's not needed
    /// in our use case. Just override the fields in this object
    /// from the fields in the other.
    pub fn add_from(&mut self, other: &Object) {
        self.dirty = true;

        for (id, field) in &other.fields {
            self.fields.insert(id.clone(), field.clone());
        }
    }

    pub(crate) fn process_slk_field(
        &mut self,
        value: &slk::Value,
        name: &str,
        metadata: &MetadataStore,
    ) -> Option<()> {
        let (field_meta, level) = metadata.query_slk_field(name, &self)?;

        let value = Value::from_str_and_ty(value.as_inner()?, field_meta.value_ty)?;

        match field_meta.variant {
            FieldVariant::Normal { .. } => self.set_simple_field(&field_meta.id, value, ValueKind::Common),
            FieldVariant::Leveled { .. } | FieldVariant::Data { .. } => self.set_leveled_field(
                &field_meta.id,
                level.expect("field must have level specified"),
                value,
                ValueKind::Common,
            ),
        }

        Some(())
    }

    pub(crate) fn process_func_field(
        &mut self,
        key: &str,
        value: &str,
        index: i8,
        metadata: &MetadataStore,
    ) -> Option<()> {
        let (field_name, value_kind) = ValueKind::split_field_name(key);
        let (field_meta, level) = metadata.query_profile_field(field_name, &self, index)?;
        let value = Value::from_str_and_ty(value, field_meta.value_ty)?;

        if let Some(level) = level {
            self.set_leveled_field(&field_meta.id, level, value, value_kind)
        } else {
            self.set_simple_field(&field_meta.id, value, value_kind)
        }

        Some(())
    }
}
