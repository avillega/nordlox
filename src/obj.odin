package nordlox

import "core:fmt"
import "core:bytes"

Obj :: struct {
    next: ^Obj,
    variant: union {
        ^String,
    },
}

new_obj :: proc($T: typeid) -> ^T {
    e := new(T)
    e.variant = e
    e.next = vm.objects
    vm.objects = &e.obj
    return e
}

print_obj :: proc(obj: ^Obj) {
    switch v in obj.variant {
    case ^String: fmt.printf("%s", v.data)
    }
}

is_obj_type :: proc(obj: ^Obj, $T: typeid) -> bool {
    _, ok := obj.variant.(T)
    return ok
}

free_obj :: proc(obj: ^Obj) {
    switch o in obj.variant {
    case ^String: free_str(o)
    }
}

String :: struct {
    using obj: Obj,
    data: []u8,
}

alloc_str :: proc(str: []u8) -> ^String {
    new_String := new_obj(String)
    new_String.data = str
    vm.strings[string(str)] = new_String
    return new_String
}

free_str :: proc(s: ^String) {
    delete(s.data)
    free(s)
}

copy_str :: proc (str: []u8) -> ^String {
    interned, ok := vm.strings[string(str)]
    if ok do return interned

    data := bytes.clone(str)
    return alloc_str(data)
}

take_str :: proc(str: []u8) -> ^String {
    interned, ok := vm.strings[string(str)]
    if ok {
        delete(str)
        return interned
    }
    return alloc_str(str)
}

concat :: proc(a: []u8, b: []u8) -> ^Obj {
    new_data := make([]u8, len(a) + len(b))
    copy(new_data, a)
    copy(new_data[len(a):], b)
    res := take_str(new_data)
    return &res.obj
}