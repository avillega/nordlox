package nordlox

import "core:fmt"
import "core:strconv"

MAX_U8 :: 255
MAX_U16 :: 65535
U8_COUNT :: MAX_U8 + 1
DEBUG_PRINT_CODE :: true

current : Compiler = ---

compile :: proc(src: []u8, chunk: ^Chunk) -> bool {
    init_scanner(src)
    current = {}
    parser.had_error = false
    parser.panic_mode = false
    compiling_chunk = chunk

    advance_parser()

    for !match_parser(.Eof) {
        declaration()
    }
    end_compiler()
    return !parser.had_error
}

Parser :: struct {
    previous: Token,
    current: Token,
    had_error: bool,
    panic_mode: bool,
}

ParseFn :: proc(ctx: Compile_Ctx)

Parse_Rule :: struct {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precendece,
}

Precendece :: enum {
    Prec_None,
    Prec_Assignment,  // =
    Prec_Or,          // Or
    Prec_And,         // And
    Prec_Equality,    // == !=
    Prec_Comparison,  // < > <= >=
    Prec_Term,        // + -
    Prec_Factor,      // * /
    Prec_Unary,       // ! -
    Prec_Call,        // . ()
    Prec_Primary,
}

Compile_Ctx :: struct {
    can_assign: bool,
}

Compiler :: struct {
    locals: [U8_COUNT]Local,
    local_count: int,
    scope_depth: int,
}


Local :: struct {
    name: Token,
    depth: int,
}

parser := Parser{}
compiling_chunk : ^Chunk = ---

expression :: proc() {
    parse_precedence(.Prec_Assignment)
}

declaration :: proc() {
    if match_parser(.Var) {
        var_decl()
    } else {
        statement()
    }

    if parser.panic_mode do synchronize()
}

var_decl :: proc() {
    global := parse_var("Expect variable name.")
    if match_parser(.Equal) {
        expression()
    } else {
        emit_op(.Op_Nil)
    }
    consume(.Semicolon, "Expect ';' after variable declaration")
    define_var(global)
}

parse_var :: proc(error_msg: string) -> u8 {
    consume(.Identifier, error_msg)

    declare_var()
    if current.scope_depth > 0 do return 0

    return identifier_constant(parser.previous)
}

declare_var :: proc() {
    using current
    if scope_depth == 0 do return // if is a global variable don't need to be declared
    name := parser.previous

    for i := local_count - 1; i >= 0; i -= 1 {
        local := locals[i]
        if local.depth != -1 && local.depth < scope_depth do break

        if string(name.lexeme) == string(local.name.lexeme) {
            error("Already a variable with this name in scope")
        }
    } 
    add_local(name)
}

add_local :: proc(name: Token) {
    using current
    if local_count >= U8_COUNT {
        error("Too many local variables in function.")
        return
    }

    locals[local_count] = Local{
        name = name,
        depth = -1,
    }
    local_count += 1
}

identifier_constant :: proc(name: Token) -> u8 {
    return make_const(&copy_str(name.lexeme).obj)
}

define_var :: proc(global_pos: u8) {
    if current.scope_depth > 0 {
        mark_initialized();
        return
    }

    emit_op(.Op_Def_Global)
    emit_byte(global_pos)
}

mark_initialized :: proc() {
    using current
    locals[local_count - 1].depth = scope_depth
}

statement :: proc() {
    switch {
    case match_parser(.Print):
        print_stmt()
    case match_parser(.Left_Brace):
        begin_scope()
        block()
        end_scope()
    case match_parser(.If):
        if_stmt()
    case match_parser(.While):
        while_stmt()
    case :
        expression_stmt()
    }
}

print_stmt :: proc() {
    expression()
    consume(.Semicolon, "Expect ';' after value.")
    emit_op(.Op_Print)
}

begin_scope :: proc() {
    current.scope_depth += 1
}

block :: proc() {
    for !check(.Right_Brace) && !check(.Eof) {
        declaration()
    }

    consume(.Right_Brace, "Expect '}' after block.")
}

end_scope :: proc() {
    using current
    scope_depth -= 1
    n: u8 = 0
    for local_count > 0 && locals[local_count - 1].depth > scope_depth {
        n += 1
        local_count -= 1
    }
    if n > 0 {
        emit_op(.Op_PopN)
        emit_byte(n)
    }
}

expression_stmt :: proc() {
    expression()
    consume(.Semicolon, "Expect ';' after expression.")
    emit_op(.Op_Pop)
}

if_stmt :: proc() {
    consume(.Left_Paren, "Expect '(' after if.")
    expression()
    consume(.Right_Paren, "Expect ')' after condition.")

    then_jmp := emit_jmp(.Op_Jmp_False)
    emit_op(.Op_Pop)
    statement()
    else_jmp := emit_jmp(.Op_Jmp)

    patch_jmp(then_jmp)

    emit_op(.Op_Pop)
    if match_parser(.Else) do statement()
    patch_jmp(else_jmp)
    
}

while_stmt :: proc() {
    loop_start := len(current_chunk().code)
    consume(.Left_Paren, "Expect '(' after while.")
    expression()
    consume(.Right_Paren, "Expect ')' after condition.")

    exit_jmp := emit_jmp(.Op_Jmp_False)
    emit_op(.Op_Pop)
    statement()
    emit_loop(loop_start)

    patch_jmp(exit_jmp)
    emit_op(.Op_Pop)
}

patch_jmp :: proc(offset: int) {
    jmp := len(current_chunk().code) - offset - 2
    if jmp > MAX_U16 {
        error("Too much code to jump over")
    }
    current_chunk().code[offset]   = u8(jmp >> 8) & 0xff
    current_chunk().code[offset+1] = u8(jmp) & 0xff
}

number :: proc(ctx: Compile_Ctx) {
    val := strconv.atof(string(parser.previous.lexeme))
    emit_const(val)
}

parse_string :: proc(ctx: Compile_Ctx) {
    lexeme_len := len(parser.previous.lexeme)
    emit_const(&copy_str(parser.previous.lexeme[1:lexeme_len-1]).obj)
}

variable :: proc(ctx: Compile_Ctx) {
    named_variable(parser.previous, ctx)
}

named_variable :: proc(name: Token, ctx: Compile_Ctx) {
    get_op, set_op : Op_Code = ---, ---
    arg, ok := resolve_local(&current, name)
    if ok {
        get_op = .Op_Get_Local
        set_op = .Op_Set_Local
    } else {
        arg = identifier_constant(name)
        get_op = .Op_Get_Global
        set_op = .Op_Set_Global    
    }

    if ctx.can_assign && match_parser(.Equal) {
        expression()
        emit_op(set_op)
        emit_byte(arg)
    } else {
        emit_op(get_op)
        emit_byte(arg)
    }
}

resolve_local :: proc(compiler: ^Compiler, name: Token) -> (u8, bool) {
    for i := compiler.local_count - 1; i >=0; i -=1 {
        local := compiler.locals[i]
        if string(local.name.lexeme) == string(name.lexeme) {
            if local.depth == -1 do error("Can't read varaible in its own initializer.")
            return u8(i), true
        }
    }
    return 0, false
}

grouping :: proc(ctx: Compile_Ctx) {
    expression()
    consume(.Right_Paren, "Expect ')' after expression.")
}

unary :: proc(ctx: Compile_Ctx) {
    op_type := parser.previous.type
    parse_precedence(.Prec_Unary)

    #partial switch op_type {
    case .Minus: emit_op(.Op_Neg)
    case .Bang : emit_op(.Op_Not)
    case       : return
    }
}

binary :: proc(ctx: Compile_Ctx) {
    op_type := parser.previous.type
    rule := parse_rules[op_type]
    parse_precedence(Precendece(int(rule.precedence) + 1))

    #partial switch op_type {
    case .Plus       : emit_op(.Op_Add)
    case .Minus      : emit_op(.Op_Sub)
    case .Star       : emit_op(.Op_Mul)
    case .Slash      : emit_op(.Op_Div)
    case .Equal_Equal: emit_op(.Op_Eql)
    case .Greater    : emit_op(.Op_Gt)
    case .Less       : emit_op(.Op_Lt)
    case .Bang_Equal:
        emit_op(.Op_Eql)
        emit_op(.Op_Not)
    case .Greater_Equal:
        emit_op(.Op_Lt)
        emit_op(.Op_Not)
    case .Less_Equal:
        emit_op(.Op_Gt)
        emit_op(.Op_Not)
    case: unreachable()
    }
}

and :: proc(ctx: Compile_Ctx) {
    end_jmp := emit_jmp(.Op_Jmp_False)
    emit_op(.Op_Pop)

    parse_precedence(.Prec_And)
    patch_jmp(end_jmp)
}

or :: proc(ctx: Compile_Ctx) {
    end_jmp := emit_jmp(.Op_Jmp_True)
    emit_op(.Op_Pop)

    parse_precedence(.Prec_Or)
    patch_jmp(end_jmp)
}

literal :: proc(ctx: Compile_Ctx) {
    #partial switch parser.previous.type {
    case .False: emit_op(.Op_False)
    case .True : emit_op(.Op_True)
    case .Nil  : emit_op(.Op_Nil)
    case       : return
    }
}

parse_precedence :: proc(precedence: Precendece) {
    advance_parser()
    prefix_rule := parse_rules[parser.previous.type].prefix
    if prefix_rule == nil {
        error("Expect expression.")
        return
    }

    can_assign := precedence <= Precendece.Prec_Assignment
    prefix_rule({can_assign})

    for precedence <= parse_rules[parser.current.type].precedence {
        advance_parser()
        infix_rule := parse_rules[parser.previous.type].infix
        infix_rule({can_assign})
    }

    if can_assign && match_parser(.Equal) do error("Invalid assigment target.")
}

current_chunk :: proc() -> ^Chunk {
    return compiling_chunk
}

advance_parser :: proc() {
    parser.previous = parser.current
    for {
        parser.current = next_token()
        if parser.current.type != .Error do break

        error_at_current(string(parser.current.lexeme))
    }
}

consume :: proc(type: Token_Type, msg: string) {
    if parser.current.type == type {
        advance_parser()
        return
    }
    error_at_current(msg)
}

match_parser :: proc(type: Token_Type) -> bool {
    if !check(type) do return false
    advance_parser()
    return true
}

check :: proc(type: Token_Type) -> bool {
    return parser.current.type == type
}

synchronize :: proc() {
    parser.panic_mode = false
    sync_types : bit_set[Token_Type] = {.Class, .Fun, .Var, .For, .If, .While, .Print, .Return}
    for parser.current.type != .Eof {
        if parser.previous.type == .Semicolon || parser.current.type in sync_types {
            return
        }
        advance_parser()
    }
}

error_at_current :: proc(msg: string) {
    error_at(parser.current, msg)
}

error :: proc(msg: string) {
    error_at(parser.previous, msg)
}

error_at :: proc(token: Token, msg: string) {
    if parser.panic_mode do return

    parser.panic_mode = true
    fmt.eprintf("[line %d] Error", token.line)

    if token.type == .Eof {
        fmt.eprint(" at end.")
    } else if token.type == .Error {

    } else {
        fmt.eprintf(" at '%s'", token.lexeme)
    }

    fmt.eprintf(": %s\n", msg)
    parser.had_error = true
}

// Backend

emit_const :: proc(val: Value) {
    emit_op(.Op_Const)
    emit_byte(make_const(val))
}

make_const :: proc(val: Value) -> u8 {
    constant := add_const(current_chunk(), val)
    if constant >= MAX_U8 {
        error("Too many constants")
        return 0
    }

    return u8(constant)
}

emit_jmp :: proc(op_code: Op_Code) -> int  {
    emit_op(op_code)
    emit_byte(0xff)
    emit_byte(0xff)
    return len(current_chunk().code) - 2
}

emit_loop :: proc(loop_start: int) {
    emit_op(.Op_Loop)
    offset := len(current_chunk().code) - loop_start + 2 // +2 to count for the two sized operand of op_loop
    if offset > MAX_U16 do error("Loop body too large.")
    emit_byte(u8(offset >> 8) & 0xff)
    emit_byte(u8(offset) & 0xff)
}

emit_op :: proc(op_code: Op_Code) {
    emit_byte(u8(op_code))
}

emit_bytes :: proc(b1, b2: u8) {
    emit_byte(b1)
    emit_byte(b2)
}

emit_byte :: proc(b: u8) {
    write_byte(current_chunk(), b, parser.previous.line)
}

emit_return :: proc() {
    emit_op(.Op_Ret)
}

end_compiler :: proc() {
    emit_return()
    when DEBUG_PRINT_CODE {
        if !parser.had_error do disassemble_chunk(current_chunk(), "code")
    }
}

parse_rules := [Token_Type]Parse_Rule {
    .Left_Paren    = {grouping,     nil,    .Prec_None},
    .Right_Paren   = {nil,          nil,    .Prec_None},
    .Left_Brace    = {nil,          nil,    .Prec_None},
    .Right_Brace   = {nil,          nil,    .Prec_None},
    .Comma         = {nil,          nil,    .Prec_None},
    .Dot           = {nil,          nil,    .Prec_None},
    .Minus         = {unary,        binary, .Prec_Term},
    .Plus          = {nil,          binary, .Prec_Term},
    .Semicolon     = {nil,          nil,    .Prec_None},
    .Slash         = {nil,          binary, .Prec_Factor},
    .Star          = {nil,          binary, .Prec_Factor},
    .Bang          = {unary,        nil,    .Prec_None},
    .Bang_Equal    = {nil,          binary, .Prec_Equality},
    .Equal         = {nil,          nil,    .Prec_None},
    .Equal_Equal   = {nil,          binary, .Prec_Equality},
    .Greater       = {nil,          binary, .Prec_Comparison},
    .Greater_Equal = {nil,          binary, .Prec_Comparison},
    .Less          = {nil,          binary, .Prec_Comparison},
    .Less_Equal    = {nil,          binary, .Prec_Comparison},
    .Identifier    = {variable,          nil,    .Prec_None},
    .String        = {parse_string, nil,    .Prec_None},
    .Number        = {number,       nil,    .Prec_None},
    .And           = {nil,          and,    .Prec_And},
    .Class         = {nil,          nil,    .Prec_None},
    .Else          = {nil,          nil,    .Prec_None},
    .False         = {literal,      nil,    .Prec_None},
    .For           = {nil,          nil,    .Prec_None},
    .Fun           = {nil,          nil,    .Prec_None},
    .If            = {nil,          nil,    .Prec_None},
    .Nil           = {literal,      nil,    .Prec_None},
    .Or            = {nil,          or,     .Prec_Or},
    .Print         = {nil,          nil,    .Prec_None},
    .Return        = {nil,          nil,    .Prec_None},
    .Super         = {nil,          nil,    .Prec_None},
    .This          = {nil,          nil,    .Prec_None},
    .True          = {literal,      nil,    .Prec_None},
    .Var           = {nil,          nil,    .Prec_None},
    .While         = {nil,          nil,    .Prec_None},
    .Error         = {nil,          nil,    .Prec_None},
    .Eof           = {nil,          nil,    .Prec_None},
}

