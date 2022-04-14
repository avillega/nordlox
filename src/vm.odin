package nordlox

import "core:fmt"
import "core:mem"
import "core:time"

DEBUG_TRACE_EXECUTIION :: true
FRAMES_MAX :: 64
STACK_MAX :: (FRAMES_MAX * U8_COUNT)

Interpret_Result :: enum {
	Ok,
	Halt,
	Compile_Error,
	Runtime_Error,
}

VM :: struct {
	frames: [FRAMES_MAX]Call_Frame,
	frame_count: int,

	stack: [STACK_MAX]Value,
	stack_top: int,
	strings: map[string]^String,
	globals: map[string]Value,
	objects: ^Obj,
	open_upvalues: ^Obj_Upvalue,
}

Call_Frame :: struct {
	closure: ^Closure,
	ip: int,
	slot_start: int,
	slots: []Value,
}

Op_Ctx :: struct {
	frame: ^Call_Frame,
	vm: ^VM,
}

vm : VM = ---

current_frame :: proc() -> ^Call_Frame {
	return &vm.frames[vm.frame_count - 1];
}

init_vm :: proc() {
	vm = VM{
		strings = make(map[string]^String),
		globals = make(map[string]Value),
		frame_count = 0,
	}

	define_native("clock", clock_native)
}

interpret :: proc(src: []u8) -> Interpret_Result {
	fn, ok := compile(src)
	if !ok do return Interpret_Result.Compile_Error

	push(&fn.obj)
	closure := new_closure(fn)
	pop()
	push(&closure.obj)
	_call(closure, 0)
	return run()
}

@(private = "file")
run :: proc() -> Interpret_Result {
	for {
		when (DEBUG_TRACE_EXECUTIION) {
			frame := current_frame()
			using vm
			fmt.print("          ")
			for val, idx in stack {
				if idx >= stack_top do break
				fmt.print("[")
				print_value(val)
				fmt.print("]")
			}
			fmt.println()
			disassemble_instruction(&frame.closure.fn.chunk, frame.ip)
		}

		instruction := Op_Code(read_byte())
		interpret_result: Interpret_Result = operations[instruction]()
		if interpret_result != Interpret_Result.Ok {
			when DEBUG_TRACE_EXECUTIION {
				fmt.print("          ")
				for val, idx in stack {
					if idx >= stack_top do break
					fmt.print("[")
					print_value(val)
					fmt.print("]")
				}
				fmt.println()
			}
			return interpret_result
		}
	}
	return Interpret_Result.Ok
}

read_byte :: #force_inline proc() -> u8 {
	using frame := current_frame()
	res := closure.fn.chunk.code[ip]
	ip += 1
	return res
}

read_u16 :: #force_inline proc() -> u16 {
	b0 := read_byte()
	b1 := read_byte()
	return (u16(b0) << 8) | u16(b1)
}

read_constant :: #force_inline proc() -> Value {
	frame := current_frame()
	idx := read_byte()
	return frame.closure.fn.chunk.consts[idx]
}

read_string :: #force_inline proc() -> string {
	return as_string(read_constant())
}

stack_peek :: proc(distance: int) -> Value {
	using vm
	return stack[stack_top - 1 - distance]
}

push :: proc(val: Value) {
	using vm
	stack[stack_top] = val
	stack_top += 1
}

pop :: proc() -> Value {
	using vm
	stack_top -= 1
	return stack[stack_top]
}

runtime_error :: proc(format: string, args: ..any) -> Interpret_Result {
	frame := current_frame()
	fmt.eprintf(format, ..args)
	fmt.eprint("\n")

	for i := vm.frame_count - 1; i >= 0; i -= 1 {
		frame := vm.frames[i]
		fn := frame.closure.fn

		fn_name := fn.name if fn.name != "" else "script"
		format := "[line %d] in %s()\n" if fn.name != "" else "[line %d] in %s\n"
		fmt.eprintf(format, fn.chunk.lines[frame.ip], fn_name)
	}

	return .Runtime_Error
} 

free_vm :: proc() {
	free_objs()
	delete(vm.strings)
	delete(vm.globals)
}

reset_stack :: proc() {
	vm.stack_top = 0
}


Op_Fn :: proc() -> Interpret_Result

_op_const :: #force_inline proc() -> Interpret_Result {
	constant := read_constant()
	push(constant)
	return Interpret_Result.Ok
}

_op_neg :: #force_inline proc() -> Interpret_Result {
	if !is_number(stack_peek(0)) {
		runtime_error("Operand must be number")
		return Interpret_Result.Runtime_Error
	}

	push(-pop().(f64))
	return Interpret_Result.Ok
}

_op_add :: #force_inline proc() -> Interpret_Result {
	if is_string(stack_peek(0)) && is_string(stack_peek(1)) {
		bstr := as_bytes(pop())
		astr := as_bytes(pop())
		push(concat(astr, bstr))
		return Interpret_Result.Ok
	}
	if is_number(stack_peek(0)) && is_number(stack_peek(1)) {
		return _op_binary(.Add)()
	}
	runtime_error("Operands must be two numbers or two strings.")
	return Interpret_Result.Runtime_Error
}


Binary_Op :: enum {
	Add, Sub, Mul, Div, Gt, Lt,
}

_op_binary :: #force_inline proc($op: Binary_Op) -> Op_Fn {
	return #force_inline proc() -> Interpret_Result {
		if !is_number(stack_peek(0)) || !is_number(stack_peek(1)) {
			runtime_error("Operands must be numbers.")
			return Interpret_Result.Runtime_Error
		}

		b := pop().(f64)
		a := pop().(f64)
		switch op {
		case .Add: push(a + b)
		case .Sub: push(a - b)
		case .Mul: push(a * b)
		case .Div: push(a / b)
		case .Gt : push(a > b)
		case .Lt : push(a < b)
		}

		return Interpret_Result.Ok
	}
}

_op_ret :: #force_inline proc() -> Interpret_Result {
	result := pop()
	frame := current_frame()

	_close_upvalues(&frame.slots[0])

	vm.frame_count -= 1
	if vm.frame_count == 0 {
		pop()
		return Interpret_Result.Halt
	}
	vm.stack_top = frame.slot_start
	push(result)
	return Interpret_Result.Ok
}

_op_false :: #force_inline proc() -> Interpret_Result {
	push(false)
	return Interpret_Result.Ok
}

_op_true :: #force_inline proc() -> Interpret_Result {
	push(true)
	return Interpret_Result.Ok
}

_op_nil :: #force_inline proc() -> Interpret_Result {
	push({})
	return Interpret_Result.Ok
}

_op_not :: #force_inline proc() -> Interpret_Result {
	push(is_falsy(pop()))
	return Interpret_Result.Ok
}

_op_eql :: #force_inline proc() -> Interpret_Result {
	b := pop()
	a := pop()
	push(value_eq(a, b))
	return Interpret_Result.Ok
}

_op_print :: #force_inline proc() -> Interpret_Result {
	print_value(pop())
	fmt.println()
	return Interpret_Result.Ok
}

_op_pop :: #force_inline proc() -> Interpret_Result {
	pop()
	return Interpret_Result.Ok
}

_op_popn :: #force_inline proc() -> Interpret_Result {
	n := read_byte()
	vm.stack_top -= int(n)
	return Interpret_Result.Ok
}

_op_def_global :: #force_inline proc() -> Interpret_Result {
	name := read_string()
	vm.globals[name] = stack_peek(0)
	pop()
	return Interpret_Result.Ok
}

_op_get_global :: #force_inline proc() -> Interpret_Result {
	name := read_string()
	val, ok := vm.globals[name]
	if !ok {
		runtime_error("Undefined variable '%s'", name)
		return Interpret_Result.Runtime_Error
	}
	push(val)
	return Interpret_Result.Ok
}

_op_set_global :: #force_inline proc() -> Interpret_Result {
	name := read_string()
	_, ok := vm.globals[name]
	if !ok {
		runtime_error("Undefined variable '%s'", name)
		return Interpret_Result.Runtime_Error
	}
	vm.globals[name] = stack_peek(0)
	return Interpret_Result.Ok
}

_op_get_local :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	slot := read_byte()
	push(frame.slots[slot])
	return Interpret_Result.Ok
}

_op_set_local :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	slot := read_byte()
	frame.slots[slot] = stack_peek(0)
	return Interpret_Result.Ok
}

_op_jmp_false :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	offset := read_u16()
	if (is_falsy(stack_peek(0))) do frame.ip += int(offset)

	return Interpret_Result.Ok
}

_op_jmp_true :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	offset := read_u16()
	if (!is_falsy(stack_peek(0))) do frame.ip += int(offset)

	return Interpret_Result.Ok
}

_op_jmp :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	offset := read_u16()
	frame.ip += int(offset)
	return Interpret_Result.Ok
}

_op_loop :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	offset := read_u16()
	frame.ip -= int(offset)
	return Interpret_Result.Ok
}

_op_closure :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	fn := as_fn(read_constant())
	closure := new_closure(fn)
	push(&closure.obj)
	for upvalue in &closure.upvalues {
		is_local := bool(read_byte())
		index := read_byte()
		if is_local {
			upvalue = _capture_upvalue(&frame.slots[index])
		} else {
			upvalue = frame.closure.upvalues[index]
		}
	}
	return Interpret_Result.Ok
}

_capture_upvalue :: proc(local: ^Value) -> ^Obj_Upvalue {
	prev_upvalue: ^Obj_Upvalue = nil
	upvalue := vm.open_upvalues
	for upvalue != nil && uintptr(upvalue.location) > uintptr(local) {
		prev_upvalue = upvalue
		upvalue = upvalue.next_upvalue
	}

	if upvalue != nil && upvalue.location == local {
		return upvalue
	}

	created_upvalue := new_upvalue(local)
	created_upvalue.next_upvalue = upvalue

	if prev_upvalue == nil {
		vm.open_upvalues = created_upvalue
	} else {
		vm.open_upvalues.next_upvalue = created_upvalue
	}
	return created_upvalue
}

_op_call :: #force_inline proc() -> Interpret_Result {
	arg_count : int = int(read_byte())
	return _call_value(stack_peek(arg_count), arg_count)
}

_call_value ::proc(callee: Value, arg_count: int) -> Interpret_Result {
	if is_obj(callee) {
		#partial switch f in callee.(^Obj).variant {
		case ^Closure:
			return _call(f, arg_count)
		case ^Native:
			native := f.fn
			result := native(arg_count, vm.stack[vm.stack_top - arg_count:])
			vm.stack_top -= arg_count + 1
			push(result)
			return Interpret_Result.Ok
		}
	}
	return runtime_error("Can only call functions and classes.")
}

_call :: proc(using closure: ^Closure, arg_count: int) -> Interpret_Result {
	if fn.arity != arg_count do return runtime_error("Expected %d arguments but got %d.", fn.arity, arg_count)
	using vm

	if frame_count == FRAMES_MAX do return runtime_error("Stack overflow.")

	slot_start := stack_top - arg_count - 1
	frames[frame_count] = Call_Frame{
		closure = closure,
		ip = 0,
		slots = stack[slot_start:],
		slot_start = slot_start,
	}
	frame_count += 1
	return Interpret_Result.Ok
}

_op_get_upvalue :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	slot := read_byte()
	push(frame.closure.upvalues[slot].location^)
	return Interpret_Result.Ok
}

_op_set_upvalue :: #force_inline proc() -> Interpret_Result {
	frame := current_frame()
	slot := read_byte()
	frame.closure.upvalues[slot].location^ = stack_peek(0)
	return Interpret_Result.Ok
}

_op_close_upvalue :: proc() -> Interpret_Result {
	_close_upvalues(&vm.stack[vm.stack_top - 1])
	pop()
	return Interpret_Result.Ok
}

_close_upvalues :: proc(last: ^Value) {
	for vm.open_upvalues != nil && uintptr(vm.open_upvalues.location) >= uintptr(last) {	
		upvalue := vm.open_upvalues
		upvalue.closed = upvalue.location^
		upvalue.location = &upvalue.closed
		vm.open_upvalues = upvalue.next_upvalue
	}
}

Op_Code :: enum u8 {
	Op_Const,
	Op_Neg,
	Op_Add,
	Op_Sub,
	Op_Mul,
	Op_Div,
	Op_Nil,
	Op_False,
	Op_True,
	Op_Not,
	Op_Eql,
	Op_Lt,
	Op_Gt,
	Op_Print,
	Op_Pop,
	Op_PopN,
	Op_Def_Global,
	Op_Get_Global,
	Op_Set_Global,
	Op_Get_Local,
	Op_Set_Local,
	Op_Get_Upvalue,
	Op_Set_Upvalue,
	Op_Jmp_False,
	Op_Jmp_True,
	Op_Jmp,
	Op_Loop,
	Op_Call,
	Op_Closure,
	Op_Close_Upvalue,
	Op_Ret,
}

operations := [Op_Code]Op_Fn {
	.Op_Const         = _op_const,
	.Op_Neg           = _op_neg,
	.Op_Add           = _op_add,
	.Op_Sub           = _op_binary(.Sub),
	.Op_Mul           = _op_binary(.Mul),
	.Op_Div           = _op_binary(.Div),
	.Op_Lt            = _op_binary(.Lt),
	.Op_Gt            = _op_binary(.Gt),
	.Op_False         = _op_false,
	.Op_True          = _op_true,
	.Op_Nil           = _op_nil,
	.Op_Not           = _op_not,
	.Op_Eql           = _op_eql,
	.Op_Print         = _op_print,
	.Op_Pop           = _op_pop,
	.Op_PopN          = _op_popn,
	.Op_Def_Global    = _op_def_global,
	.Op_Get_Global    = _op_get_global,
	.Op_Set_Global    = _op_set_global,
	.Op_Get_Local     = _op_get_local,
	.Op_Set_Local     = _op_set_local,
	.Op_Get_Upvalue   = _op_get_upvalue,
	.Op_Set_Upvalue   = _op_set_upvalue,
	.Op_Jmp_False     = _op_jmp_false,
	.Op_Jmp_True      = _op_jmp_true,
	.Op_Jmp           = _op_jmp,
	.Op_Loop          = _op_loop,
	.Op_Call          = _op_call,
	.Op_Closure       = _op_closure,
	.Op_Close_Upvalue = _op_close_upvalue,
	.Op_Ret           = _op_ret,
}

define_native :: proc(name: string, fn: Native_Fn) {
	push(&new_native(fn).obj)
	vm.globals[name] = vm.stack[0]
	pop()
}
