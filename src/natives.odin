package nordlox

import "core:fmt"
import "core:time"

clock_native :: proc(arg_count: int, args: []Value) -> Value {
    return f64(time.tick_now()._nsec) / 1000000.0
}