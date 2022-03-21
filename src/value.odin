package nordlox
import "core:fmt"

Value :: union {
    f64,
    bool,
    ^Obj,
}

is_number :: proc(val: Value) -> bool {
    _, ok := val.(f64)
    return ok
}

is_nil :: proc(val: Value) -> bool {
    return val == nil
}

is_bool :: proc(val: Value) -> bool {
    _, ok := val.(bool)
    return ok
}

is_obj :: proc(val: Value) -> bool {
    _, ok := val.(^Obj)
    return ok
}

is_string :: proc(val: Value) -> bool {
    return is_obj(val) && is_obj_type(val.(^Obj), ^String)
}

as_string :: proc(val: Value) -> string {
    return string(val.(^Obj).variant.(^String).data)
}

as_bytes :: proc(val: Value) -> []u8 {
    return val.(^Obj).variant.(^String).data
}

is_falsy :: proc(val: Value) -> bool {
    return val == nil || is_bool(val) && !val.(bool)
}

value_eq :: proc(a: Value, b: Value) -> bool {
    if a == nil && b == nil do return true
    return a == b
}

print_value :: proc(val: Value) {
    switch v in val {
    case bool: fmt.print("true" if v else "false")
    case f64 : fmt.printf("%f", v)
    case ^Obj: print_obj(v)
    case     : fmt.print("nil")
    }
}

