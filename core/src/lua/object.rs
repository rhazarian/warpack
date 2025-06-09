use std::cell::RefCell;
use std::collections::HashSet;
use std::sync::{Arc, RwLock};

use anyhow::anyhow;
use atoi::atoi;
use rlua::prelude::*;

use warpack_formats::{ObjectId, ObjectKind};
use warpack_formats::metadata::FieldDesc;
use warpack_formats::object::{Object, Value, ValueKind};
use warpack_formats::objectstore::ObjectStore;
use warpack_formats::parser::w3obj;

use crate::error::StringError;
use crate::lua::util::*;

fn get_field_for<C>(object: &Object, field_getter: C) -> Option<&Value>
    where
        C: Fn(&Object) -> Option<&Value>,
{
    field_getter(object).or_else(|| {
        w3data::data()
            .object_prototype(&object)
            .and_then(|proto| field_getter(proto))
    })
}

struct StaticMethodKeys {
    obj_getfield: LuaRegistryKey,
    obj_setfield: LuaRegistryKey,
    obj_clone: LuaRegistryKey,
    objstore_read: LuaRegistryKey,
    objstore_write: LuaRegistryKey,
    objstore_write_skin: LuaRegistryKey,
    objstore_setobject: LuaRegistryKey,
    objstore_getobject: LuaRegistryKey,
    objstore_newobject: LuaRegistryKey,
}

thread_local! {
    static OBJECT_METHODS: RefCell<Option<StaticMethodKeys >> = RefCell::new(None);
}

struct StaticMethods<'lua> {
    obj_getfield: LuaFunction<'lua>,
    obj_setfield: LuaFunction<'lua>,
    obj_clone: LuaFunction<'lua>,
    objstore_read: LuaFunction<'lua>,
    objstore_write: LuaFunction<'lua>,
    objstore_write_skin: LuaFunction<'lua>,
    objstore_setobject: LuaFunction<'lua>,
    objstore_getobject: LuaFunction<'lua>,
    objstore_newobject: LuaFunction<'lua>,
}

impl<'lua> StaticMethods<'lua> {
    fn new(ctx: LuaContext<'lua>, keys: &StaticMethodKeys) -> StaticMethods<'lua> {
        StaticMethods {
            obj_getfield: ctx.registry_value(&keys.obj_getfield).unwrap(),
            obj_setfield: ctx.registry_value(&keys.obj_setfield).unwrap(),
            obj_clone: ctx.registry_value(&keys.obj_clone).unwrap(),
            objstore_read: ctx.registry_value(&keys.objstore_read).unwrap(),
            objstore_write: ctx.registry_value(&keys.objstore_write).unwrap(),
            objstore_write_skin: ctx.registry_value(&keys.objstore_write_skin).unwrap(),
            objstore_getobject: ctx.registry_value(&keys.objstore_getobject).unwrap(),
            objstore_setobject: ctx.registry_value(&keys.objstore_setobject).unwrap(),
            objstore_newobject: ctx.registry_value(&keys.objstore_newobject).unwrap(),
        }
    }

    fn with<'a, C, R>(ctx: LuaContext<'a>, callback: C) -> R
        where
            C: FnOnce(LuaContext<'a>, StaticMethods<'a>) -> R,
    {
        OBJECT_METHODS.with(|keys| {
            if keys.borrow().is_none() {
                let objstore_read = ctx
                    .create_registry_value(
                        ctx.create_function(LuaObjectStoreWrapper::read).unwrap(),
                    )
                    .unwrap();

                let objstore_write = ctx
                    .create_registry_value(
                        ctx.create_function(LuaObjectStoreWrapper::write).unwrap(),
                    )
                    .unwrap();

                let objstore_write_skin = ctx
                    .create_registry_value(
                        ctx.create_function(LuaObjectStoreWrapper::write_skin).unwrap(),
                    )
                    .unwrap();

                let objstore_getobject = ctx
                    .create_registry_value(
                        ctx.create_function(|ctx, (object, key): (LuaAnyUserData, LuaValue)| {
                            let mut object = object.borrow_mut::<LuaObjectStoreWrapper>()?;
                            LuaObjectStoreWrapper::get_object(ctx, (&mut object, key))
                        })
                            .unwrap(),
                    )
                    .unwrap();

                let objstore_setobject = ctx
                    .create_registry_value(
                        ctx.create_function(
                            |ctx, (object, key, value): (LuaAnyUserData, LuaValue, LuaValue)| {
                                let mut object = object.borrow_mut::<LuaObjectStoreWrapper>()?;
                                LuaObjectStoreWrapper::set_object(ctx, (&mut object, key, value))
                            },
                        )
                            .unwrap(),
                    )
                    .unwrap();

                let objstore_newobject = ctx
                    .create_registry_value(
                        ctx.create_function(
                            |ctx, (object, id, parent_id): (LuaAnyUserData, LuaValue, LuaValue)| {
                                let mut object = object.borrow_mut::<LuaObjectStoreWrapper>()?;
                                LuaObjectStoreWrapper::new_object(ctx, (&mut object, id, parent_id))
                            },
                        )
                            .unwrap(),
                    )
                    .unwrap();

                let obj_clone = ctx
                    .create_registry_value(ctx.create_function(LuaObjectWrapper::clone).unwrap())
                    .unwrap();

                let obj_setfield = ctx
                    .create_registry_value(
                        ctx.create_function(
                            |ctx, (object, key, value): (LuaAnyUserData, LuaValue, LuaValue)| {
                                let object = object.borrow::<LuaObjectWrapper>()?;
                                LuaObjectWrapper::set_field(ctx, (&object, key, value))
                            },
                        )
                            .unwrap(),
                    )
                    .unwrap();

                let obj_getfield = ctx
                    .create_registry_value(
                        ctx.create_function(|ctx, (object, key): (LuaAnyUserData, LuaValue)| {
                            let object = object.borrow::<LuaObjectWrapper>()?;
                            LuaObjectWrapper::get_field(ctx, (&object, key))
                        })
                            .unwrap(),
                    )
                    .unwrap();

                *keys.borrow_mut() = Some(StaticMethodKeys {
                    obj_setfield,
                    obj_getfield,
                    obj_clone,
                    objstore_read,
                    objstore_write,
                    objstore_write_skin,
                    objstore_getobject,
                    objstore_setobject,
                    objstore_newobject,
                })
            }

            let keys_ref = keys.borrow();
            let keys = keys_ref.as_ref().unwrap();
            let methods = StaticMethods::new(ctx, keys);

            callback(ctx, methods)
        })
    }

    fn obj_getfield_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.obj_getfield)
        })
    }

    fn obj_setfield_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.obj_setfield)
        })
    }

    fn obj_clone_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| LuaValue::Function(methods.obj_clone))
    }

    fn objstore_read_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_read)
        })
    }

    fn objstore_write_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_write)
        })
    }

    fn objstore_write_skin_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_write_skin)
        })
    }

    fn objstore_getobject_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_getobject)
        })
    }

    fn objstore_setobject_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_setobject)
        })
    }

    fn objstore_newobject_fn(ctx: LuaContext) -> LuaValue {
        Self::with(ctx, |_ctx, methods| {
            LuaValue::Function(methods.objstore_newobject)
        })
    }
}

struct LuaObjectWrapper {
    inner: Arc<RwLock<Object>>,
}

impl LuaObjectWrapper {
    fn clone<'lua>(
        _ctx: LuaContext<'lua>,
        object: LuaAnyUserData<'lua>,
    ) -> Result<impl ToLuaMulti<'lua>, LuaError> {
        let object = object.borrow::<LuaObjectWrapper>()?;

        let object = object.inner.read().unwrap();
        let mut new_object = Object::new(object.id(), object.kind());

        new_object.set_parent_id(object.parent_id());
        new_object.set_aliased_id(object.aliased_id());
        new_object.add_from(&object);

        Ok(LuaObjectWrapper {
            inner: Arc::new(RwLock::new(new_object)),
        })
    }

    fn fields<'lua>(ctx: LuaContext<'lua>, object: &Object) -> Result<LuaValue<'lua>, LuaError> {
        let fields: Vec<_> = w3data::metadata()
            .query_all_object_fields(&object)
            .map(|field_desc| field_desc.id.clone())
            .collect();

        Ok(fields.to_lua(ctx)?)
    }

    fn translate_field_name<'lua>(
        ctx: LuaContext<'lua>,
        key: LuaValue<'lua>,
        object: &Object,
    ) -> Result<Option<(&'static FieldDesc, Option<u32>, ValueKind)>, LuaError> {
        if let Ok(id) = LuaInteger::from_lua(key.clone(), ctx) {
            let field_desc =
                w3data::metadata().query_object_field(ObjectId::new(id as u32), object);

            return Ok(field_desc.map(|d| (d, None, ValueKind::Common)));
        }

        let lua_key = LuaString::from_lua(key, ctx)?;
        let key = lua_key.to_str()?;
        let sd_field_name = key.strip_suffix(":sd");
        let hd_field_name = key.strip_suffix(":hd");
        let field_name = hd_field_name.or(sd_field_name).unwrap_or(key);
        let field_bytes = field_name.as_bytes();
        let value_kind = if hd_field_name.is_some() {
            ValueKind::HD
        } else if sd_field_name.is_some() {
            ValueKind::SD
        } else {
            ValueKind::Common
        };

        // check if the field is in the form of 'XXXX' or 'XXXX+Y'
        if (field_bytes.len() == 4) || (field_bytes.len() > 5 && field_bytes[4] == b'+') {
            let object_id = ObjectId::from_bytes(&field_bytes[0..4]).unwrap();

            if let Some(field_desc) = w3data::metadata().query_object_field(object_id, object) {
                let level = if field_bytes.len() > 5 {
                    atoi::<u32>(&field_bytes[5..])
                } else {
                    None
                };

                if (level.is_some() && field_desc.variant.is_leveled())
                    || (level.is_none() && !field_desc.variant.is_leveled())
                {
                    return Ok(Some((field_desc, level, value_kind)));
                }
            }
        }

        let result = w3data::metadata().query_lua_field(object, field_name);

        Ok(result.map(|(field_desc, level)| (field_desc, level, value_kind)))
    }

    fn get_field<'lua>(
        ctx: LuaContext<'lua>,
        (object, key): (&LuaObjectWrapper, LuaValue<'lua>),
    ) -> Result<impl ToLua<'lua>, LuaError> {
        let object = object.inner.read().unwrap();

        if let Some((field_desc, level, hd)) = Self::translate_field_name(ctx, key, &object)? {
            let field = if let Some(level) = level {
                get_field_for(&object, |o| o.leveled_field(&field_desc.id, level, hd))
            } else {
                get_field_for(&object, |o| o.simple_field(&field_desc.id, hd))
            };

            if let Some(field) = field {
                return Ok(value_to_lvalue(ctx, field));
            }
        }

        Ok(LuaValue::Nil)
    }

    fn set_field<'lua>(
        ctx: LuaContext<'lua>,
        (object, key, value): (&LuaObjectWrapper, LuaValue<'lua>, LuaValue<'lua>),
    ) -> Result<impl ToLuaMulti<'lua>, LuaError> {
        let object = &mut object.inner.write().unwrap();

        if let Some((field_desc, level, hd)) = Self::translate_field_name(ctx, key, object)? {
            if let LuaValue::Nil = value {
                if let Some(level) = level {
                    object.unset_leveled_field(&field_desc.id, level)
                } else {
                    object.unset_simple_field(&field_desc.id)
                }
            } else {
                let value = lvalue_to_value(ctx, value, field_desc)?;
                if let Some(level) = level {
                    object.set_leveled_field(&field_desc.id, level, value, hd)
                } else {
                    object.set_simple_field(&field_desc.id, value, hd)
                }
            }

            return Ok(LuaValue::Nil);
        }

        Err(LuaError::external(anyhow!(
            "cannot set unknown field on object"
        )))
    }

    fn index<'lua>(
        ctx: LuaContext<'lua>,
        object: &mut LuaObjectWrapper,
        key: LuaValue<'lua>,
    ) -> Result<impl ToLua<'lua>, LuaError> {
        let object_inner = &object.inner.read().unwrap();

        if let Ok(key) = LuaString::from_lua(key.clone(), ctx) {
            let key = key.as_bytes();

            match key {
                b"all" => return Ok(Self::fields(ctx, &object_inner)?),
                b"clone" => return Ok(StaticMethods::obj_clone_fn(ctx)),
                b"setField" => return Ok(StaticMethods::obj_setfield_fn(ctx)),
                b"getField" => return Ok(StaticMethods::obj_getfield_fn(ctx)),
                b"id" => return Ok(object_inner.id().to_lua(ctx)?),
                b"parentId" => return Ok(object_inner.parent_id().to_lua(ctx)?),
                b"type" => return Ok(object_inner.kind().to_typestr().to_lua(ctx)?),
                _ => {}
            }
        }

        Ok(Self::get_field(ctx, (object, key))?.to_lua(ctx)?)
    }

    fn newindex<'lua>(
        ctx: LuaContext<'lua>,
        object: &mut LuaObjectWrapper,
        (key, value): (LuaValue<'lua>, LuaValue<'lua>),
    ) -> Result<impl ToLuaMulti<'lua>, LuaError> {
        Self::set_field(ctx, (object, key, value))
    }
}

struct LuaObjectStoreWrapper {
    inner: ObjectStore,
    kind: ObjectKind,
}

impl LuaObjectStoreWrapper {
    fn read<'lua>(
        ctx: LuaContext<'lua>,
        (data, value, value_kind_lua): (LuaAnyUserData<'lua>, LuaValue<'lua>, LuaValue<'lua>),
    ) -> Result<LuaValue<'lua>, LuaError> {
        let mut data = data.borrow_mut::<LuaObjectStoreWrapper>()?;
        let kind = data.kind;
        let data = &mut data.inner;
        let value = LuaString::from_lua(value, ctx)?;
        let value_kind = if let LuaValue::Nil = value_kind_lua {
            ValueKind::Common
        } else {
            let value_kind = LuaString::from_lua(value_kind_lua, ctx)?.to_str()?.to_string();
            match value_kind.to_lowercase().as_str() {
                "sd" => ValueKind::SD,
                "hd" => ValueKind::HD,
                _ => Err(LuaError::external(anyhow!("unknown value kind '{}'", value_kind)))?,
            }
        };

        w3obj::read::read_object_file(value.as_bytes(), data, kind, value_kind).map_err(LuaError::external)?;
        data.reset_dirty();

        Ok(LuaValue::Nil)
    }

    fn write<'lua>(
        ctx: LuaContext<'lua>,
        (data, value_kind_lua): (LuaAnyUserData<'lua>, LuaValue<'lua>),
    ) -> Result<LuaValue<'lua>, LuaError> {
        let data = data.borrow::<LuaObjectStoreWrapper>()?;
        let kind = data.kind;
        let data = &data.inner;
        let value_kind = if let LuaValue::Nil = value_kind_lua {
            ValueKind::Common
        } else {
            let value_kind = LuaString::from_lua(value_kind_lua, ctx)?.to_str()?.to_string();
            match value_kind.to_lowercase().as_str() {
                "sd" => ValueKind::SD,
                "hd" => ValueKind::HD,
                _ => Err(LuaError::external(anyhow!("unknown value kind '{}'", value_kind)))?,
            }
        };

        let mut buf = Vec::new();
        w3obj::write::write_object_file(&mut buf, w3data::metadata(), w3data::data(), &data, kind, value_kind)
            .map_err(LuaError::external)?;

        Ok(LuaValue::String(ctx.create_string(&buf)?))
    }

    fn write_skin<'lua>(
        ctx: LuaContext<'lua>,
        data: LuaAnyUserData<'lua>,
    ) -> Result<LuaValue<'lua>, LuaError> {
        let data = data.borrow::<LuaObjectStoreWrapper>()?;
        let kind = data.kind;
        let data = &data.inner;

        let mut buf = Vec::new();
        w3obj::write::write_skin_file(&mut buf, w3data::metadata(), &data, kind)
            .map_err(LuaError::external)?;

        Ok(LuaValue::String(ctx.create_string(&buf)?))
    }

    fn object_or_new(
        data: &mut ObjectStore,
        kind: ObjectKind,
        id: &ObjectId,
    ) -> Option<Arc<RwLock<Object>>> {
        data.object(id.clone()).map(|object| Arc::clone(object)).or_else(|| {
            w3data::data()
                .object(id.clone())
                .filter(|object| kind.contains(object.kind()))
                .map(|object| {
                    let mut object = Object::new(object.id(), object.kind());
                    object.set_dirty(false);
                    data.insert_object(object);

                    Arc::clone(data.object(id.clone()).unwrap())
                })
        })
    }

    fn objects<'lua>(
        ctx: LuaContext<'lua>,
        data: &mut ObjectStore,
        kind: ObjectKind,
    ) -> Result<LuaValue<'lua>, LuaError> {
        let table = ctx.create_table()?;
        let mut set: HashSet<ObjectId> = HashSet::new();

        set.extend(
            w3data::data()
                .objects()
                .filter(|object| kind.contains(object.kind()))
                .map(|o| o.id()),
        );

        set.extend(data.objects().map(|o| o.read().unwrap().id()));

        for id in &set {
            let object = Self::object_or_new(data, kind, id).unwrap();
            table.set(id.clone(), LuaObjectWrapper { inner: object })?;
        }

        Ok(LuaValue::Table(table))
    }

    fn get_object<'lua>(
        ctx: LuaContext<'lua>,
        (data, key): (&mut LuaObjectStoreWrapper, LuaValue<'lua>),
    ) -> Result<LuaValue<'lua>, LuaError> {
        let kind = data.kind;
        let data = &mut data.inner;

        if let Ok(id) = ObjectId::from_lua(key, ctx) {
            return Self::object_or_new(data, kind, &id)
                .map(|object| LuaObjectWrapper { inner: object })
                .to_lua(ctx);
        }

        Ok(LuaValue::Nil)
    }

    fn set_object<'lua>(
        ctx: LuaContext<'lua>,
        (data, key, value): (&mut LuaObjectStoreWrapper, LuaValue<'lua>, LuaValue<'lua>),
    ) -> Result<LuaValue<'lua>, LuaError> {
        let data = &mut data.inner;

        if let Ok(id) = ObjectId::from_lua(key.clone(), ctx) {
            if let LuaValue::Nil = value {
                data.remove_object(id)
            } else {
                let value = LuaAnyUserData::from_lua(value, ctx)?;
                let object = value.borrow::<LuaObjectWrapper>()?;
                let object = object.inner.read().unwrap();

                if w3data::data().object(id.clone()).is_some() {
                    if id == object.id() {
                        let object_clone = object.clone();
                        data.insert_object(object_clone);
                    } else {
                        return Err(LuaError::external(anyhow!(
                            "stock objects can only be assigned to the same stock object"
                        )));
                    }
                } else {
                    let mut object_clone = object.clone();

                    if object_clone.parent_id().is_none() {
                        object_clone.set_parent_id(Some(object_clone.id()));
                    }

                    object_clone.set_id(id);
                    data.insert_object(object_clone);
                }
            }

            Ok(LuaValue::Nil)
        } else {
            Err(LuaError::external(anyhow!(
                "cannot assign invalid field {:?} on object store",
                key
            )))
        }
    }

    fn new_object<'lua>(
        ctx: LuaContext<'lua>,
        (data, id, parent_id): (&mut LuaObjectStoreWrapper, LuaValue<'lua>, LuaValue<'lua>),
    ) -> Result<LuaValue<'lua>, LuaError> {
        let kind = data.kind;
        let data = &mut data.inner;

        if let Ok(id) = ObjectId::from_lua(id.clone(), ctx) {
            if let Ok(parent_id) = ObjectId::from_lua(parent_id.clone(), ctx) {
                if let Some(parent) = w3data::data().object(parent_id.clone()).filter(|object| kind.contains(object.kind())) {
                    data.insert_object(Object::with_parent(id.clone(), parent_id.clone(), kind));

                    if let Some(new_object_ref) = data.object(id) {
                        new_object_ref.write().unwrap().set_aliased_id(parent.aliased_id());
                        LuaObjectWrapper {
                            inner: new_object_ref.clone()
                        }.to_lua(ctx)
                    } else {
                        Err(LuaError::external(anyhow!(
                            "internal error"
                        )))
                    }
                } else {
                    Err(LuaError::external(anyhow!(
                        "stock {:?} with id '{:?}' does not exist",
                        kind.to_typestr(), parent_id
                    )))
                }
            } else {
                Err(LuaError::external(anyhow!(
                    "invalid object id '{:?}'",
                    parent_id
                )))
            }
        } else {
            Err(LuaError::external(anyhow!(
                "invalid object id '{:?}'",
                id
            )))
        }
    }

    fn index<'lua>(
        ctx: LuaContext<'lua>,
        data: &mut LuaObjectStoreWrapper,
        key: LuaValue<'lua>,
    ) -> Result<LuaValue<'lua>, LuaError> {
        let kind = data.kind;
        let data_inner = &mut data.inner;

        if let Ok(key) = LuaString::from_lua(key.clone(), ctx) {
            let key = key.as_bytes();

            match key {
                b"all" => return Self::objects(ctx, data_inner, kind),
                b"readFromString" => return Ok(StaticMethods::objstore_read_fn(ctx)),
                b"writeToString" => return Ok(StaticMethods::objstore_write_fn(ctx)),
                b"writeSkinToString" => return Ok(StaticMethods::objstore_write_skin_fn(ctx)),
                b"getObject" => return Ok(StaticMethods::objstore_getobject_fn(ctx)),
                b"setObject" => return Ok(StaticMethods::objstore_setobject_fn(ctx)),
                b"newObject" => return Ok(StaticMethods::objstore_newobject_fn(ctx)),
                b"ext" => return Ok(kind.to_ext().to_lua(ctx)?),
                b"typestr" => return Ok(kind.to_typestr().to_lua(ctx)?),
                b"isDirty" => return Ok(data_inner.is_dirty().to_lua(ctx)?),
                _ => {}
            }
        }

        Self::get_object(ctx, (data, key))
    }

    fn newindex<'lua>(
        ctx: LuaContext<'lua>,
        data: &mut LuaObjectStoreWrapper,
        (key, value): (LuaValue<'lua>, LuaValue<'lua>),
    ) -> Result<LuaValue<'lua>, LuaError> {
        Self::set_object(ctx, (data, key, value))
    }
}

impl LuaUserData for LuaObjectWrapper {
    fn add_methods<'lua, T>(methods: &mut T)
        where
            T: LuaUserDataMethods<'lua, Self>,
    {
        methods.add_meta_method_mut(LuaMetaMethod::Index, LuaObjectWrapper::index);
        methods.add_meta_method_mut(LuaMetaMethod::NewIndex, LuaObjectWrapper::newindex);
    }
}

impl LuaUserData for LuaObjectStoreWrapper {
    fn add_methods<'lua, T>(methods: &mut T)
        where
            T: LuaUserDataMethods<'lua, Self>,
    {
        methods.add_meta_method_mut(LuaMetaMethod::Index, LuaObjectStoreWrapper::index);
        methods.add_meta_method_mut(LuaMetaMethod::NewIndex, LuaObjectStoreWrapper::newindex);
    }
}

// standalone functions

fn open_store_from_str(
    source: &[u8],
    kind: ObjectKind,
) -> Result<LuaObjectStoreWrapper, anyhow::Error> {
    let mut data = ObjectStore::default();
    w3obj::read::read_object_file(source, &mut data, kind, ValueKind::Common)?;
    data.reset_dirty();

    Ok(LuaObjectStoreWrapper { inner: data, kind })
}

fn get_open_store_from_str_luafn(ctx: LuaContext) -> LuaFunction {
    ctx.create_function(|_ctx: LuaContext, (data, ext): (LuaString, LuaString)| {
        let data = data.as_bytes();
        let kind = ObjectKind::from_ext(ext.to_str()?);

        if kind == ObjectKind::empty() {
            return Err(StringError::new(format!(
                "{} is not a valid format",
                ext.to_str().unwrap()
            ))
                .into());
        }

        let result = open_store_from_str(data, kind).map_err(LuaError::external)?;

        Ok(result)
    })
        .unwrap()
}

fn get_open_store_blank_luafn(ctx: LuaContext) -> LuaFunction {
    ctx.create_function(|_ctx: LuaContext, ext: LuaString| {
        let kind = ObjectKind::from_ext(ext.to_str()?);

        if kind == ObjectKind::empty() {
            return Err(StringError::new(format!(
                "{} is not a valid format",
                ext.to_str().unwrap()
            ))
                .into());
        }

        Ok(LuaObjectStoreWrapper {
            inner: ObjectStore::default(),
            kind,
        })
    })
        .unwrap()
}

pub fn get_object_module(ctx: LuaContext) -> LuaTable {
    let table = ctx.create_table().unwrap();

    table
        .set("openStore", get_open_store_from_str_luafn(ctx))
        .unwrap();

    table
        .set("newStore", get_open_store_blank_luafn(ctx))
        .unwrap();

    table
}
