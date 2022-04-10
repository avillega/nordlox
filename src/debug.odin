package nordlox
import "core:fmt"

Sign :: enum {
	Pos, Neg,
}

disassemble_chunk :: proc(chunk: ^Chunk, name: string) {
	fmt.printf("== %s ==\n", name)
	for i := 0; i < len(chunk.code); {
		i = disassemble_instruction(chunk, i)
	}
}

disassemble_instruction :: proc(chunk: ^Chunk, idx: int) -> int {
	fmt.printf("%04d ", idx)

	if (idx > 0 && chunk.lines[idx] == chunk.lines[idx - 1]) {
		fmt.print("   | ")
	} else {
		fmt.printf("% 4d ", chunk.lines[idx])
	}

	instruction := Op_Code(chunk.code[idx])

	switch instruction {
	case .Op_Ret, .Op_Neg, .Op_Add, .Op_Sub, .Op_Mul, .Op_Div, .Op_False, .Op_True,
	.Op_Nil, .Op_Not, .Op_Eql, .Op_Gt, .Op_Lt, .Op_Print, .Op_Pop, .Op_Close_Upvalue:
		return simple_instruction(instruction, idx)
	
	case .Op_PopN, .Op_Get_Local, .Op_Set_Local, .Op_Call, .Op_Get_Upvalue,
	.Op_Set_Upvalue:
		return byte_instruction(instruction, chunk, idx)
	
	case .Op_Const, .Op_Def_Global, .Op_Get_Global, .Op_Set_Global:
		return constant_instruction(instruction, chunk, idx)

	case .Op_Jmp_False, .Op_Jmp_True, .Op_Jmp:
		return jmp_instruction(instruction, .Pos, chunk, idx)
	
	case .Op_Loop:
		return jmp_instruction(instruction, .Neg, chunk, idx)
	
	case .Op_Closure:
		offset := idx
		offset += 1
		const_idx := chunk.code[offset]
		offset += 1
		fmt.printf("%-15s % 4d ", instruction, const_idx)
		print_value(chunk.consts[const_idx])
		fmt.println()

		fn := as_fn(chunk.consts[const_idx])
		for i in 0..<fn.upvalue_count {
			is_local := bool(chunk.code[offset])
			offset += 1
			upvalue_idx := chunk.code[offset]
			offset += 1
			fmt.printf("%04d    |                    %s %d\n", offset - 2, is_local ? "local" : "upvalue", upvalue_idx)
		}
		return offset
	
	case:
		unreachable()
	}

}

constant_instruction :: proc(name: Op_Code, chunk: ^Chunk, idx: int) -> int {
	const_idx := chunk.code[idx + 1]
	fmt.printf("%-15s % 4d '", name, const_idx)
	print_value(chunk.consts[const_idx])
	fmt.println("'")
	return idx + 2
}

byte_instruction :: proc(name: Op_Code, chunk: ^Chunk, idx: int) -> int {
	slot := chunk.code[idx + 1]
	fmt.printf("%-15s % 4d\n", name, slot)
	return idx + 2
}

jmp_instruction :: proc(name: Op_Code, $sign: Sign, chunk: ^Chunk, idx: int) -> int {
	b0 := chunk.code[idx + 1]
	b1 := chunk.code[idx + 2]
	jmp := (int(b0) << 8) | int(b1)
	when sign == .Neg {
		jmp *= -1
	}

	fmt.printf("%-15s % 4d -> %d\n", name, idx, idx + 3 + jmp)
	return idx + 3
}

simple_instruction :: proc(name: Op_Code, idx: int) -> int {
	fmt.printf("%s\n", name)
	return idx + 1
}