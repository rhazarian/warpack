pub mod read {
    use byteorder::{BE, LE, ReadBytesExt};

    use crate::{ObjectId, ObjectKind};
    use crate::error::*;
    use crate::object::{Object, Value, ValueKind};
    use crate::objectstore::ObjectStore;

    fn read_str<'src>(source: &mut &'src [u8]) -> Result<&'src [u8], ObjParseError> {
        let end = source
            .iter()
            .enumerate()
            .find(|(_, c)| **c == 0)
            .map(|(i, _)| i)
            .ok_or_else(ObjParseError::unterminated_string)?;
        let result = &source[..end];
        *source = &source[end + 1..];

        Ok(result)
    }

    fn read_value(source: &mut &[u8], field_type: u32) -> Result<Value, ObjParseError> {
        Ok(match field_type {
            0 => Value::Int(source.read_i32::<LE>()?),
            1 => Value::Real(source.read_f32::<LE>()?),
            2 => Value::Unreal(source.read_f32::<LE>()?),
            3 => Value::String(read_str(source).map(|s| String::from_utf8_lossy(s).into_owned())?),
            _ => panic!("malformed"),
        })
    }

    fn read_field(
        source: &mut &[u8],
        object: &mut Object,
        uses_extra_ints: bool,
    ) -> Result<(), ObjParseError> {
        let field_id = source.read_u32::<BE>().map(ObjectId::new)?;
        let field_type = source.read_u32::<LE>()?;

        if !uses_extra_ints {
            let value = read_value(source, field_type)?;

            object.set_simple_field(field_id, value, ValueKind::Common);
        } else {
            let level = source.read_u32::<LE>()?;
            source.read_u32::<LE>()?;
            let value = read_value(source, field_type)?;

            if level == 0 {
                object.set_simple_field(field_id, value, ValueKind::Common);
            } else {
                object.set_leveled_field(field_id, level, value, ValueKind::Common);
            }
        }

        // read trailing int
        source.read_u32::<LE>()?;

        Ok(())
    }

    pub fn read_object_table(
        source: &mut &[u8],
        version: u32,
        data: &mut ObjectStore,
        kind: ObjectKind,
    ) -> Result<(), ObjParseError> {
        let obj_amount = source.read_u32::<LE>()?;

        for _ in 0..obj_amount {
            let original_id = source.read_u32::<BE>().map(ObjectId::new)?;
            let new_id = source.read_u32::<BE>().map(ObjectId::new)?;

            let mut object = if new_id.to_u32() != 0 {
                Object::with_parent(new_id, original_id, kind)
            } else {
                Object::new(original_id, kind)
            };

            let aliased_id = data.object(original_id)
                .and_then(|object| object.read().unwrap().aliased_id());
            object.set_aliased_id(aliased_id);

            if version >= 3 {
                let bytes = source.read_u32::<LE>()?;
                for _ in 0..bytes {
                    source.read_u32::<LE>()?;
                }
            }

            let mod_amount = source.read_u32::<LE>()?;
            for _ in 0..mod_amount {
                read_field(source, &mut object, kind.is_data_type())?;
            }

            data.insert_object(object);
        }

        Ok(())
    }

    /// Reads the given object file, and produces
    /// an `ObjectStore` object containing all read
    /// objects.
    pub fn read_object_file(
        mut source: &[u8],
        data: &mut ObjectStore,
        kind: ObjectKind,
    ) -> Result<(), ObjParseError> {
        let version = source.read_u32::<LE>()?;

        read_object_table(&mut source, version, data, kind)?;
        read_object_table(&mut source, version, data, kind)?;

        Ok(())
    }
}

pub mod write {
    use std::io::{Error as IoError, Write};
    use std::sync::RwLockReadGuard;

    use byteorder::{BE, LE, WriteBytesExt};

    use crate::{ObjectId, ObjectKind};
    use crate::metadata::MetadataStore;
    use crate::object::{FieldKind, Object, Value, ValueKind};
    use crate::objectstore::{ObjectStore, ObjectStoreStock};

    const W3OBJ_FORMAT_VERSION: u32 = 1;

    type FlatFieldItem<'a> = (ObjectId, u8, u32, &'a Value);

    fn object_flat_fields_data<'a>(
        object: &'a Object,
        metadata: &'a MetadataStore,
        value_kind: ValueKind,
    ) -> impl Iterator<Item = FlatFieldItem<'a>> {
        object.fields().flat_map(move |(id, field)| {
            let iter: Box<dyn Iterator<Item = FlatFieldItem>> = match &field.kind {
                FieldKind::Simple { value, value_sd, value_hd } =>
                    Box::new(
                        match value_kind {
                            ValueKind::Common => value.iter(),
                            ValueKind::SD => value_sd.iter(),
                            ValueKind::HD => value_hd.iter(),
                        }.map(move |value| (*id, 0, 0, value))
                    ),
                FieldKind::Leveled { values } => {
                    if let Some(field_desc) = metadata.field_by_id((*id).clone()) {
                        Box::new(values.iter().flat_map(move |leveled_value| {
                            match value_kind {
                                ValueKind::Common => leveled_value.value.iter(),
                                ValueKind::SD => leveled_value.value_sd.iter(),
                                ValueKind::HD => leveled_value.value_hd.iter(),
                            }.map(move |value|
                                (
                                    *id,
                                    field_desc.variant.data_id().unwrap_or(0),
                                    leveled_value.level,
                                    value,
                                )
                            )
                        }))
                    } else {
                        Box::new(std::iter::empty())
                    }
                }
            };

            iter
        })
    }

    fn object_flat_fields_simple(object: &Object, value_kind: ValueKind) -> impl Iterator<Item = (ObjectId, &Value)> {
        object
            .fields()
            .filter_map(move |(id, field)| match &field.kind {
                FieldKind::Simple { value, value_sd, value_hd } => match value_kind {
                    ValueKind::Common => value.as_ref(),
                    ValueKind::SD => value_sd.as_ref(),
                    ValueKind::HD => value_hd.as_ref(),
                }.map(|value| (*id, value)),
                FieldKind::Leveled { .. } => {
                    eprintln!(
                        "unexpected data field in object {} for field {}",
                        object.id(),
                        field.id
                    );
                    None
                }
            })
    }

    fn is_obj_kind_pred(kind: ObjectKind) -> impl Fn(&RwLockReadGuard<Object>) -> bool {
        move |o| o.kind().contains(kind)
    }

    fn is_obj_stock_pred() -> impl Fn(&RwLockReadGuard<Object>) -> bool {
        |o| o.parent_id().is_none()
    }

    fn is_obj_custom_pred() -> impl Fn(&RwLockReadGuard<Object>) -> bool {
        |o| o.parent_id().is_some()
    }

    fn write_string<W: Write>(mut writer: W, string: &str) -> Result<(), IoError> {
        for c in string.as_bytes() {
            if *c == 0 {
                break;
            }

            writer.write_u8(*c)?;
        }
        writer.write_u8(0)?;

        Ok(())
    }

    fn write_value<W: Write>(mut writer: W, value: &Value) -> Result<(), IoError> {
        match value {
            Value::Int(num) => writer.write_i32::<LE>(*num)?,
            Value::Real(num) => writer.write_f32::<LE>(*num)?,
            Value::Unreal(num) => writer.write_f32::<LE>(*num)?,
            Value::String(val) => write_string(&mut writer, val)?,
        }

        Ok(())
    }

    fn write_simple_fields<W: Write>(mut writer: W, object: &Object, value_kind: ValueKind) -> Result<(), IoError> {
        let fields: Vec<_> = object_flat_fields_simple(object, value_kind).collect();

        writer.write_u32::<LE>(fields.len() as u32)?;
        for (id, value) in fields {
            writer.write_u32::<BE>(id.to_u32())?;
            writer.write_u32::<LE>(value.type_id())?;

            write_value(&mut writer, value)?;
            writer.write_u32::<BE>(object.id().to_u32())?;
        }

        Ok(())
    }

    fn write_data_fields<W: Write>(
        mut writer: W,
        object: &Object,
        metadata: &MetadataStore,
        value_kind: ValueKind,
    ) -> Result<(), IoError> {
        let fields: Vec<_> = object_flat_fields_data(object, metadata, value_kind).collect();

        writer.write_u32::<LE>(fields.len() as u32)?;
        for (id, data_id, level, value) in fields {
            writer.write_u32::<BE>(id.to_u32())?;
            writer.write_u32::<LE>(value.type_id())?;
            writer.write_u32::<LE>(level)?;
            writer.write_u32::<LE>(data_id as u32)?;

            write_value(&mut writer, value)?;
            writer.write_u32::<BE>(object.id().to_u32())?;
        }

        Ok(())
    }

    pub fn write_object_file<W: Write>(
        mut writer: W,
        metadata: &MetadataStore,
        stock_data: &ObjectStoreStock,
        data: &ObjectStore,
        kind: ObjectKind,
        value_kind: ValueKind,
    ) -> Result<(), IoError> {
        if kind == ObjectKind::LIGHTNING {
            let fields: Vec<_> = metadata.fields()
                .filter(|f| f.kind == kind)
                .filter(|f| !f.is_profile)
                .collect();

            let objects: Vec<Object> = data
                .objects()
                .map(|o| o.read().unwrap())
                .filter(is_obj_kind_pred(kind))
                .map(|o| o.clone())
                .chain(stock_data
                    .objects()
                    .filter(|o| o.kind().contains(kind))
                    .filter(|o| data.object(o.id()).is_none())
                    .map(|o| o.clone()))
                .collect();

            writer.write_all("ID;PWXL;N;E\r\n".as_bytes())?;
            writer.write_all(format!("B;X{};Y{};D0\r\n", fields.len() + 1, objects.len() + 2).as_bytes())?;
            for (index, field) in fields.iter().enumerate() {
                writer.write_all(format!("C;X{};Y1;K\"{}\"\r\n", index + 1, field.variant.name()).as_bytes())?;
            }

            for (row_index, object) in objects.iter().enumerate() {
                for (index, field) in fields.iter().enumerate() {
                    let value = if index == 0 {
                        Some(Value::String(object.id().to_string().unwrap()))
                    } else {
                        object.simple_field(field.id, value_kind).or_else(
                            || stock_data.object_prototype(&object)
                                .and_then(|proto| proto.simple_field(field.id, value_kind)))
                            .map(|v| v.clone())
                    };
                    let str_value = match value {
                        Some(actual_value) => match actual_value {
                            Value::Int(num) => num.to_string(),
                            Value::Real(num) => num.to_string(),
                            Value::Unreal(num) => num.to_string(),
                            Value::String(val) => format!("\"{}\"", val),
                        },
                        None => "".to_string()
                    };
                    if !str_value.is_empty() {
                        writer.write_all(format!("C;X{};Y{};K{}\r\n", index + 1, row_index + 2, str_value).as_bytes())?;
                    }
                }
            }
            writer.write_all("E\r\n\r\n".as_bytes())?;

            return Ok(())
        }

        writer.write_u32::<LE>(W3OBJ_FORMAT_VERSION)?;

        let stock_objs: Vec<_> = data
            .objects()
            .map(|o| o.read().unwrap())
            .filter(is_obj_kind_pred(kind))
            .filter(is_obj_stock_pred())
            .collect();

        let custom_objs: Vec<_> = data
            .objects()
            .map(|o| o.read().unwrap())
            .filter(is_obj_kind_pred(kind))
            .filter(is_obj_custom_pred())
            .collect();

        // write stock objects
        writer.write_u32::<LE>(stock_objs.len() as u32)?;
        for object in stock_objs {
            writer.write_u32::<BE>(object.id().to_u32())?;
            writer.write_u32::<BE>(0)?;

            if kind.is_data_type() {
                write_data_fields(&mut writer, &object, metadata, value_kind)?;
            } else {
                write_simple_fields(&mut writer, &object, value_kind)?;
            }
        }

        // write custom objects
        writer.write_u32::<LE>(custom_objs.len() as u32)?;
        for object in custom_objs {
            writer.write_u32::<BE>(object.parent_id().unwrap().to_u32())?;
            writer.write_u32::<BE>(object.id().to_u32())?;

            if kind.is_data_type() {
                write_data_fields(&mut writer, &object, metadata, value_kind)?;
            } else {
                write_simple_fields(&mut writer, &object, value_kind)?;
            }
        }

        Ok(())
    }
}
