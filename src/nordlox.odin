package nordlox

import "core:fmt"
import "core:os"
import "core:mem"

main :: proc() {
	track: mem.Tracking_Allocator
	mem.tracking_allocator_init(&track, context.allocator)
	context.allocator = mem.tracking_allocator(&track)

	_main()

	for _, leak in track.allocation_map {
		fmt.printf("%v leaked %v bytes\n", leak.location, leak.size)
	}
	for bad_free in track.bad_free_array {
		fmt.printf("%v allocation %p was freed badly\n", bad_free.location, bad_free.memory)
	}
}

_main :: proc() {
	init_vm()
	defer free_vm()

	chunk: Chunk = Chunk{}
	defer free_chunk(&chunk)
	args := os.args

	if len(args) == 1 {
		repl()
	} else if len(args) == 2 {
		run_file(args[1])
	} else {
		fmt.eprintln("Usage: nordlox [path]")
		os.exit(64)
	}
}

repl :: proc() {
	buf : [1024]u8 = ---
	for {
		fmt.print("> ")
		if n, _ := os.read(os.stdin, buf[:]); n > 0 {
			if string(buf[:n-1]) == "exit" do return
			interpret(buf[:n])
		} else {
			fmt.println("")
		}   
	}
}

run_file :: proc(path: string) {
	bytes, ok := os.read_entire_file(path)
	defer delete(bytes)
	if !ok {
		fmt.eprintf("Couldn't open file \"%s\".\n", path)
		os.exit(74)
	}
	interpret(bytes)
}