# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Res_3

export main

# Import modules that we'll need.
import io

func main() -> Int uses IO {
    # It's an error to use the same resource twice in the same statement.
    print!(use_io_and_return_string!())

    # It's also an error to use a parent and child resource in the same
    # statement.
    print!(int_to_string(test_uses_time!()))

    # Or to use and observe related resources.
    print!(int_to_string(observe_io!()))

    # or any ancestor and child (using/using).
    print!(int_to_string(test_uses_time!()))

    # or any ancestor and child (using/observing).
    print!(int_to_string(test_gettimeofday!()))

    # Like above but the other way around.
    _ = use_env!() + observe_io!()

    if (3 == 3) {
        # Any test within a compound statement.
        x = use_env!() + observe_io!()
    } else {}

    return 0
}

func use_io_and_return_string() -> String uses IO {
    return "Hello world\n"
}


func test_uses_time() -> Int uses Time {
    return test_gettimeofday!()
}

func test_gettimeofday() -> Int observes Time {
    ok, s, _ = gettimeofday!()
    if (ok) {
        return s
    } else {
        return 0
    }
}

func observe_io() -> Int observes IO {
    return 4
}

func use_env() -> Int uses Environment {
    return 12
}

