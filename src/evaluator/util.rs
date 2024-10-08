use super::Object;

pub fn is_error(obj: &Object) -> bool {
    obj.object_type() == "ERROR"
}

pub fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(value) => *value,
        _ => true,
    }
}
