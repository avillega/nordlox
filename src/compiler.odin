package nordlox

import "core:fmt"
import "core:strconv"
import "core:strings"

MAX_U8 :: 255
MAX_U16 :: 65535
U8_COUNT :: MAX_U8 + 1
DEBUG_PRINT_CODE :: true

current : ^Compiler = ---

compile :: proc(src: []u8) -> (^Function ,bool) {
	init_scanner(src)
	compiler := Compiler{}
	init_compiler(&compiler, .Script)

	parser.had_error = false
	parser.panic_mode = false

	advance_parser()

	for !match_parser(.Eof) {
		declaration()
	}
	fn: = end_compiler()
	return fn, !parser.had_error
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
	enclosing: ^Compiler,
	function: ^Function,
	type: Function_Type,
	locals: [U8_COUNT]Local,
	local_count: int,
	upvalues: [U8_COUNT]Upvalue,
	scope_depth: int,
}

init_compiler :: proc(compiler: ^Compiler, type: Function_Type) {
	compiler.enclosing = current
	compiler.function = nil
	compiler.type = type
	compiler.local_count = 0
	compiler.scope_depth = 0
	compiler.function = new_fn()
	current = compiler

	if type != .Script {
		current.function.name = string(copy_str(parser.previous.lexeme).data)
	}

	local := &current.locals[current.local_count]
	current.local_count += 1
	local.depth = 0
	local.is_captured = false
	local.name.lexeme = {}
}


Local :: struct {
	name       : Token,
	depth      : int,
	is_captured: bool,
}

Upvalue :: struct {
	index: u8,
	is_local: bool,
}

Function_Type :: enum {
	Function,
	Script,
}

parser := Parser{}

expression :: proc() {
	parse_precedence(.Prec_Assignment)
}

declaration :: proc() {
	if match_parser(.Fun) {
		fn_decl()
	} else if match_parser(.Var) {
		var_decl()
	} else {
		statement()
	}

	if parser.panic_mode do synchronize()
}

fn_decl :: proc() {
	global := parse_var("Expect function name.")
	mark_initialized()
	function(.Function)
	define_var(global)
}

function :: proc(type: Function_Type) {
	compiler := Compiler{}
	init_compiler(&compiler, type)
	begin_scope()

	consume(.Left_Paren, "Expect '(' after function name.")
	if !check(.Right_Paren) {
		for {
			current.function.arity += 1
			if current.function.arity > 255 {
				error_at_current("Can't have more than 255 parameters.")
			}

			constant := parse_var("Expect parameter name.")
			define_var(constant)
			if !match_parser(.Comma) do break
		}
	}

	consume(.Right_Paren, "Expect ')' after parameters.")
	consume(.Left_Brace, "Expect '{' before function body.")
	block()

	fn := end_compiler()
	emit_op(.Op_Closure)
	emit_byte(make_const(&fn.obj))

	for i in 0..<fn.upvalue_count {
		emit_byte(compiler.upvalues[i].is_local ? 1 : 0)
		emit_byte(compiler.upvalues[i].index)
	}
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
		name        = name,
		depth       = -1,
		is_captured = false,
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
	if scope_depth == 0 do return

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
	case match_parser(.Return):
		return_stmt()
	case match_parser(.While):
		while_stmt()
	case match_parser(.For):
		for_stmt()
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
	for local_count > 0 && locals[local_count - 1].depth > scope_depth {
		if locals[local_count - 1].is_captured {
			emit_op(.Op_Close_Upvalue)
		} else {
			emit_op(.Op_Pop)
		}
		local_count -= 1
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

return_stmt :: proc() {
	if current.type == .Script do error("Can't return from top-level code.")

	if match_parser(.Semicolon) {
		emit_return()
	} else {
		expression()
		consume(.Semicolon, "Expect ';' after return value.")
		emit_op(.Op_Ret)
	}
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

for_stmt :: proc() {
	begin_scope()
	consume(.Left_Paren, "Expect '(' after for.")

	if match_parser(.Semicolon) {
		// do nothing
	} else if match_parser(.Var) {
		var_decl()
	} else {
		expression_stmt()
	}
	
	loop_start := len(current_chunk().code)
	exit_jmp := -1
	if !match_parser(.Semicolon) {
		expression()
		consume(.Semicolon, "Expect ';' after loop condition.")
		exit_jmp = emit_jmp(.Op_Jmp_False)
		emit_op(.Op_Pop)
	}

	if !match_parser(.Right_Paren) {
		body_jmp := emit_jmp(.Op_Jmp)
		increment_start := len(current_chunk().code)

		expression()
		emit_op(.Op_Pop)
		consume(.Right_Paren, "Expect ')' after for clauses.")

		emit_loop(loop_start)
		loop_start = increment_start
		patch_jmp(body_jmp)
	}

	statement();
	emit_loop(loop_start)
	if exit_jmp != -1 {
		patch_jmp(exit_jmp)
		emit_op(.Op_Pop)
	}
	end_scope()
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
	arg, ok := resolve_local(current, name)
	if ok {
		get_op = .Op_Get_Local
		set_op = .Op_Set_Local
	} else if arg, ok = resolve_upvalue(current, name); ok {
		get_op = .Op_Get_Upvalue
		set_op = .Op_Set_Upvalue
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

resolve_upvalue :: proc(compiler: ^Compiler, name: Token) -> (u8, bool) {
	if compiler.enclosing == nil do return 0, false

	local, ok := resolve_local(compiler.enclosing, name)
	if ok {
		compiler.enclosing.locals[local].is_captured = true
		return add_upvalue(compiler, local, true)
	}

	upvalue, ok2 := resolve_upvalue(compiler.enclosing, name)
	if ok2 {
		return add_upvalue(compiler, upvalue, false)
	}

	return 0, false
}

add_upvalue :: proc(using compiler: ^Compiler, index: u8, is_local: bool) -> (u8, bool) {
	for i in 0..<function.upvalue_count {
		upvalue := compiler.upvalues[i]
		if upvalue.index == index && upvalue.is_local == is_local {
			return u8(i), true
		}
	}

	if function.upvalue_count == U8_COUNT {
		error("Too many closure variables in function")
		return 0, false
	}

	upvalues[function.upvalue_count].is_local = is_local
	upvalues[function.upvalue_count].index = index
	function.upvalue_count += 1
	return u8(function.upvalue_count - 1), true
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

call :: proc(ctx: Compile_Ctx) {
	arg_count := argument_list();
	emit_op(.Op_Call)
	emit_byte(arg_count)
}

argument_list :: proc() -> u8 {
	arg_count : u8 = 0
	if !check(.Right_Paren) {
		for {
			expression()
			if arg_count == 255 do error("Can't have more than 255 arguments.")

			arg_count += 1
			if !match_parser(.Comma) do break
		}
	}
	consume(.Right_Paren, "Expect ')' after arguments")
	return arg_count
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
	return &current.function.chunk
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
	emit_op(.Op_Nil)
	emit_op(.Op_Ret)
}

end_compiler :: proc() -> ^Function{
	emit_return()
	function := current.function

	when DEBUG_PRINT_CODE {
		if !parser.had_error {
			disassemble_chunk(current_chunk(), function.name if function.name != "" else "<script>")
		}
	}

	current = current.enclosing
	return function
}

parse_rules := [Token_Type]Parse_Rule {
	.Left_Paren    = {grouping,     call,   .Prec_Call},
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

