package nordlox

import "core:fmt"
import "core:time"

clock_native :: proc(arg_count: int, args: []Value) -> Value {
    using time
    return duration_milliseconds(Duration(tick_now()._nsec))
}