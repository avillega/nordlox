package nordlox

import "core:fmt"
import "core:bytes"

// Obj

Obj :: struct {
	next: ^Obj,
	variant: union {
		^String,
		^Function,
		^Native,
		^Closure,
		^Obj_Upvalue,
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
	case ^String     : fmt.printf("%s", v.data)
	case ^Function   : print_fn(v)
	case ^Native     : print_native(v)
	case ^Closure    : print_fn(v.fn)
	case ^Obj_Upvalue: fmt.print("Upvalue")
	}
}

is_obj_type :: proc(obj: ^Obj, $T: typeid) -> bool {
	_, ok := obj.variant.(T)
	return ok
}

free_obj :: proc(obj: ^Obj) {
	switch v in obj.variant {
	case ^String     : free_str(v)
	case ^Function   : free_fn(v)
	case ^Native     : free_native(v)
	case ^Closure    : free_closure(v)
	case ^Obj_Upvalue: free_upvalue(v)
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

// Obj_Upvalue
Obj_Upvalue ::  struct {
	using obj: Obj,
	location: ^Value,
	closed: Value,
	next_upvalue: ^Obj_Upvalue,
}

new_upvalue :: proc(location: ^Value) -> ^Obj_Upvalue {
	upvalue := new_obj(Obj_Upvalue)
	upvalue.location = location
	upvalue.closed = nil
	return upvalue
}

free_upvalue :: proc(upvalue: ^Obj_Upvalue) {
	free(upvalue)
}

// Functions

Function :: struct {
	using obj: Obj,
	arity: int,
	upvalue_count: int,
	chunk: Chunk,
	name: string,
}

new_fn :: proc() -> ^Function {
	function := new_obj(Function)
	function.arity = 0
	function.name = ""
	function.upvalue_count = 0
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

// Native Functions

Native_Fn :: proc(arg_count: int, args: []Value) -> Value

Native :: struct {
	using obj: Obj,
	fn: Native_Fn,
}

new_native :: proc(fn: Native_Fn) -> ^Native {
	native := new_obj(Native)
	native.fn = fn
	return native
}

print_native :: proc(native: ^Native) {
	fmt.print("<native fn>")
}

free_native :: proc(native: ^Native) {
	free(native)
}

// Closures

Closure :: struct {
	using obj: Obj,
	fn: ^Function,
	upvalues: []^Obj_Upvalue,
}

new_closure :: proc(fn: ^Function) -> ^Closure {
	closure := new_obj(Closure)
	closure.fn = fn
	closure.upvalues = make([]^Obj_Upvalue, fn.upvalue_count)
	return closure
}

free_closure :: proc(closure: ^Closure) {
	delete(closure.upvalues)
	free(closure)
}