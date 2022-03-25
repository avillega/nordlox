package nordlox

import "core:fmt"
import "core:bytes"

// Obj

Obj :: struct {
	next: ^Obj,
	variant: union {
		^String,
		^Function,
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
	case ^Function: print_fn(v)
	}
}

is_obj_type :: proc(obj: ^Obj, $T: typeid) -> bool {
	_, ok := obj.variant.(T)
	return ok
}

free_obj :: proc(obj: ^Obj) {
	switch v in obj.variant {
	case ^String: free_str(v)
	case ^Function: free_fn(v)
	}
}

// String

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

// Functions

Function :: struct {
	using obj: Obj,
	arity: int,
	chunk: Chunk,
	name: string,
}

new_fn :: proc() -> ^Function {
	function := new_obj(Function)
	function.arity = 0
	function.name = ""
	function.chunk = Chunk{}
	return function
}

free_fn :: proc(f: ^Function) {
	free_chunk(&f.chunk)
	free(f)
}

print_fn :: proc(f: ^Function) {
	if f.name == "" {
		fmt.print("<script>")
		return
	}
	fmt.printf("<fn %s>", f.name)
}