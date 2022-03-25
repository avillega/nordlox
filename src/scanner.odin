package nordlox

import "core:fmt"

Scanner :: struct {
	src : []u8,
	start, current, line : int,
}

Token :: struct {
	type  : Token_Type,
	lexeme: []u8,
	line  : int,
}

Token_Type :: enum {
	// Single-Character Tokens.
	Left_Paren, Right_Paren,
	Left_Brace, Right_Brace,
	Comma, Dot, Minus, Plus,
	Semicolon, Slash, Star,
	// One Or Two Character Tokens.
	Bang, Bang_Equal,
	Equal, Equal_Equal,
	Greater, Greater_Equal,
	Less, Less_Equal,
	// Literals.
	Identifier, String, Number,
	// Keywords.
	And, Class, Else, False,
	For, Fun, If, Nil, Or,
	Print, Return, Super, This,
	True, Var, While,
	Error, Eof,
}

scanner : Scanner = ---

init_scanner :: proc(src: []u8) {
	scanner = Scanner {
		src = src,
		start = 0,
		current = 0,
		line = 1,
	}
}

next_token :: proc() -> Token {
	skip_whitespace()
	scanner.start = scanner.current

	if is_at_end() do return make_token(.Eof)

	c := advance_scanner()
	if is_digit(c) do return make_number()
	if is_alpha(c) do return make_identifier()

	switch c {
	case '(': return make_token(.Left_Paren)
	case ')': return make_token(.Right_Paren)
	case '{': return make_token(.Left_Brace)
	case '}': return make_token(.Right_Brace)
	case ';': return make_token(.Semicolon)
	case ',': return make_token(.Comma)
	case '.': return make_token(.Dot)
	case '-': return make_token(.Minus)
	case '+': return make_token(.Plus)
	case '/': return make_token(.Slash)
	case '*': return make_token(.Star)
	case '!':
		return make_token(.Bang_Equal    if match('=') else .Bang)
	case '=':
		return make_token(.Equal_Equal   if match('=') else .Equal)
	case '<':
		return make_token(.Less_Equal    if match('=') else .Less)
	case '>':
		return make_token(.Greater_Equal if match('=') else .Greater)
	case '"': return make_string()
	}

	return error_token("Unexpected character.")
}

is_at_end :: proc() -> bool {
	return scanner.current >= len(scanner.src)
}

advance_scanner :: proc() -> u8 {
	scanner.current += 1
	return scanner.src[scanner.current-1]
}

match :: proc(expected : u8) -> bool {
	using scanner
	if is_at_end() do return false
	if src[current] != expected do return false
	current += 1
	return true
}

peek :: proc() -> u8 {
	if is_at_end() do return 0
	using scanner
	return src[current]
}

peek_next :: proc() -> u8 {
	if is_at_end() do return 0
	using scanner
	return src[current+1]
}

skip_whitespace :: proc() {
	for {
		c := peek()
		switch c {
		case ' ', '\r', '\t': advance_scanner()
		case '\n':
			scanner.line += 1
			advance_scanner()
		case '/':
			if peek_next() == '/' {
				for peek() != '\n' && !is_at_end() {
					advance_scanner()
				}
			} else {
				return
			}
		case: return
		}
	}
}

make_string :: proc() -> Token {
	for peek() != '"' && !is_at_end() {
		if peek() == '\n' do scanner.line += 1
		advance_scanner()
	}

	if is_at_end() do return error_token("Unterminated string.")

	advance_scanner()
	return make_token(.String)
}

is_digit :: proc(c: u8) -> bool {
	return '0' <= c && c <= '9'
}

is_alpha :: proc(c: u8) -> bool {
	return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_'
}

make_number :: proc() -> Token {
	for is_digit(peek()) do advance_scanner()

	if peek() == '.' && is_digit(peek_next()) {
		advance_scanner()
		for is_digit(peek()) do advance_scanner()
	}

	return make_token(.Number)
}

make_identifier :: proc() -> Token {
	for is_alpha(peek()) || is_digit(peek()) do advance_scanner()
	return make_token(identifier_type())
}

identifier_type :: proc() -> Token_Type {
	using scanner
	using Token_Type
	switch string(src[start:current]) {
	case "and"   : return And
	case "class" : return Class
	case "else"  : return Else
	case "false" : return False
	case "for"   : return For
	case "fun"   : return Fun
	case "if"    : return If
	case "nil"   : return Nil
	case "or"    : return Or
	case "print" : return Print
	case "return": return Return
	case "super" : return Super
	case "this"  : return This
	case "true"  : return True
	case "var"   : return Var
	case "while" : return While
	case         : return Identifier
	}
}

make_token :: proc(type: Token_Type) -> Token {
	using scanner
	return Token{
		type   = type,
		lexeme = src[start:current],
		line   = line,
	}
}

error_token :: proc(msg: string) -> Token {
	return Token{
		type   = Token_Type.Error,
		lexeme = transmute([]u8) msg,
		line   = scanner.line,
	}
}