package nordlox

import "core:fmt"

DEBUG_TRACE_EXECUTIION :: true
STACK_MAX :: 256

Interpret_Result :: enum {
    Ok,
    Halt,
    Compile_Error,
    Runtime_Error,
}

VM :: struct {
    chunk: ^Chunk,
    ip: int,
    stack: [STACK_MAX]Value,
    stack_top: int,
    strings: map[string]^String,
    globals: map[string]Value,
    objects: ^Obj,
}

vm : VM = ---

init_vm :: proc() {
    vm = VM{
        strings = make(map[string]^String),
        globals = make(map[string]Value),
    }
}

interpret :: proc(src: []u8) -> Interpret_Result {
    chunk := Chunk{}
    defer free_chunk(&chunk)

    ok := compile(src, &chunk)
    if !ok do return Interpret_Result.Compile_Error

    vm.chunk = &chunk
    vm.ip = 0

    return run()
}

@(private = "file")
run :: proc() -> Interpret_Result {
    for {
        when (DEBUG_TRACE_EXECUTIION) {
            using vm
            fmt.print("          ")
            for val, idx in stack {
                if idx >= stack_top do break
                fmt.print("[")
                print_value(val)
                fmt.print("]")
            }
            fmt.println()
            disassemble_instruction(vm.chunk, vm.ip)
        }

        instruction := Op_Code(read_byte())
        interpret_result: Interpret_Result = operations[instruction]()
        if interpret_result != Interpret_Result.Ok do return interpret_result
    }
    return Interpret_Result.Ok
}

read_byte :: #force_inline proc() -> u8 {
    defer vm.ip += 1
    return vm.chunk.code[vm.ip]
}

read_u16 :: #force_inline proc() -> u16 {
    b0 := read_byte()
    b1 := read_byte()
    return (u16(b0) << 8) | u16(b1)
}

read_constant :: #force_inline proc() -> Value {
    idx := read_byte()
    return vm.chunk.consts[idx]
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

runtime_error :: proc(format: string, args: ..any) {
    fmt.eprintf(format, ..args)
    fmt.eprint("\n")
    fmt.eprintf("[line %d] in script\n", vm.chunk.lines[vm.ip])
    reset_stack()
} 

free_vm :: proc() {
    free_objs()
    delete(vm.strings)
    delete(vm.globals)
}

reset_stack :: proc() {
    vm.ip = 0
    vm.stack_top = 0
}


Op_Fn :: proc() -> Interpret_Result

_op_const :: proc() -> Interpret_Result {
    constant := read_constant()
    push(constant)
    return Interpret_Result.Ok
}

_op_neg :: proc() -> Interpret_Result {
    if !is_number(stack_peek(0)) {
        runtime_error("Operand must be number")
        return Interpret_Result.Runtime_Error
    }

    push(-pop().(f64))
    return Interpret_Result.Ok
}

_op_add :: proc() -> Interpret_Result {
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

_op_binary :: proc($op: Binary_Op) -> Op_Fn {
    return proc() -> Interpret_Result {
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

_op_ret :: proc() -> Interpret_Result {
    return Interpret_Result.Halt
}

_op_false :: proc() -> Interpret_Result {
    push(false)
    return Interpret_Result.Ok
}

_op_true :: proc() -> Interpret_Result {
    push(true)
    return Interpret_Result.Ok
}

_op_nil :: proc() -> Interpret_Result {
    push({})
    return Interpret_Result.Ok
}

_op_not :: proc() -> Interpret_Result {
    push(is_falsy(pop()))
    return Interpret_Result.Ok
}

_op_eql :: proc() -> Interpret_Result {
    b := pop()
    a := pop()
    push(value_eq(a, b))
    return Interpret_Result.Ok
}

_op_print :: proc() -> Interpret_Result {
    print_value(pop())
    fmt.println()
    return Interpret_Result.Ok
}

_op_pop :: proc() -> Interpret_Result {
    pop()
    return Interpret_Result.Ok
}

_op_popn :: proc() -> Interpret_Result {
    n := read_byte()
    vm.stack_top -= int(n)
    return Interpret_Result.Ok
}

_op_def_global :: proc() -> Interpret_Result {
    name := read_string()
    vm.globals[name] = stack_peek(0)
    pop()
    return Interpret_Result.Ok
}

_op_get_global :: proc() -> Interpret_Result {
    name := read_string()
    val, ok := vm.globals[name]
    if !ok {
        runtime_error("Undefined variable '%s'", name)
        return Interpret_Result.Runtime_Error
    }
    push(val)
    return Interpret_Result.Ok
}

_op_set_global :: proc() -> Interpret_Result {
    name := read_string()
    _, ok := vm.globals[name]
    if !ok {
        runtime_error("Undefined variable '%s'", name)
        return Interpret_Result.Runtime_Error
    }
    vm.globals[name] = stack_peek(0)
    return Interpret_Result.Ok
}

_op_get_local :: proc() -> Interpret_Result {
    slot := read_byte()
    push(vm.stack[slot])
    return Interpret_Result.Ok
}

_op_set_local :: proc() -> Interpret_Result {
    slot := read_byte()
    vm.stack[slot] = stack_peek(0)
    return Interpret_Result.Ok
}

_op_jmp_false :: proc() -> Interpret_Result {
    offset := read_u16()
    if (is_falsy(stack_peek(0))) do vm.ip += int(offset)

    return Interpret_Result.Ok
}

_op_jmp_true :: proc() -> Interpret_Result {
    offset := read_u16()
    if (!is_falsy(stack_peek(0))) do vm.ip += int(offset)

    return Interpret_Result.Ok
}

_op_jmp :: proc() -> Interpret_Result {
    offset := read_u16()
    vm.ip += int(offset)
    return Interpret_Result.Ok
}

_op_loop :: proc() -> Interpret_Result {
    offset := read_u16()
    vm.ip -= int(offset)
    return Interpret_Result.Ok
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
    Op_Jmp_False,
    Op_Jmp_True,
    Op_Jmp,
    Op_Loop,
    Op_Ret,
}

operations := [Op_Code]Op_Fn {
    .Op_Const      = _op_const,
    .Op_Neg        = _op_neg,
    .Op_Add        = _op_add,
    .Op_Sub        = _op_binary(.Sub),
    .Op_Mul        = _op_binary(.Mul),
    .Op_Div        = _op_binary(.Div),
    .Op_Lt         = _op_binary(.Lt),
    .Op_Gt         = _op_binary(.Gt),
    .Op_False      = _op_false,
    .Op_True       = _op_true,
    .Op_Nil        = _op_nil,
    .Op_Not        = _op_not,
    .Op_Eql        = _op_eql,
    .Op_Print      = _op_print,
    .Op_Pop        = _op_pop,
    .Op_PopN       = _op_popn,
    .Op_Def_Global = _op_def_global,
    .Op_Get_Global = _op_get_global,
    .Op_Set_Global = _op_set_global,
    .Op_Get_Local  = _op_get_local,
    .Op_Set_Local  = _op_set_local,
    .Op_Jmp_False  = _op_jmp_false,
    .Op_Jmp_True   = _op_jmp_true,
    .Op_Jmp        = _op_jmp,
    .Op_Loop       = _op_loop,
    .Op_Ret        = _op_ret,
}
