package nordlox

Chunk :: struct {
	code: [dynamic]u8,
	consts: [dynamic]Value,
	lines: [dynamic]int,
}

write_byte :: proc(chunk: ^Chunk, b: u8, line: int) {
	append(&chunk.code, b)
	append(&chunk.lines, line)
}

add_const :: proc(chunk: ^Chunk, val: Value) -> int {
	append(&chunk.consts, val)
	return len(chunk.consts) - 1
}

free_chunk :: proc(chunk: ^Chunk) {
	delete(chunk.code)
	delete(chunk.consts)
	delete(chunk.lines)
}