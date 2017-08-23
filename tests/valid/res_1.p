# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Res_1

export main

import io

func main() -> Int uses IO {
    print!("Hello world\n")

    use_state!()

    test_setenv!("test_env", "test value")

    time_s = test_gettimeofday!()
    print!("# The time is " ++ int_to_string(time_s) ++ "s\n")

    return use_foo!()
}

resource MyState from IO
resource MySubState from MyState

func use_state() uses MySubState {}

# resource Environment from IO

func test_setenv(name : String, value : String) uses Environment {
    _ = setenv!(name, value)
    return
}

# resource Time from IO

func test_gettimeofday() -> Int observes Time {
    b, s, us = gettimeofday!()
    if (b) {
        return s
    } else {
        return -1
    }
}

# define our own resources
resource Foo from IO
resource Bar from Foo

func use_foo() -> Int uses Foo {
    return 0
}

func observe_foo() -> Int observes Foo {
    return observe_bar!()
}

func observe_bar() -> Int observes Bar {
    return 42
}

