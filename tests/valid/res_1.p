# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Res_1

export main

import io

func main() uses IO -> Int {
    print!("Hello world\n")

    use_state!()

    test_setenv!("test_env", "test value")

    time_s = test_gettimeofday!()
    print!("# The time is " ++ int_to_string(time_s) ++ "s\n")
    r = use_foo!()

    # Safe resource use in a sub-statement.
    if (0 == 0) {
        print!("Two uses of IO\n")
        print!("Within the same compound statement (the if)\n")
    } else {
        print!("Are okay, also okay in different branches\n")
    }

    return r
}

func use_env() uses Environment -> Int {
    return 0
}

resource MyState from IO
resource MySubState from MyState

# Parens are valid but optional here.
func use_state() uses (MySubState) {}

# resource Environment from IO

func test_setenv(name : String, value : String) uses Environment {
    _ = setenv!(name, value)
    return
}

# resource Time from IO

func test_gettimeofday() observes Time -> Int {
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
resource Bax from Foo

func use_foo() uses Foo -> Int {
    return 0
}

func observe_foo() observes Foo -> Int {
    return observe_bar!()
}

func observe_bar() observes Bar -> Int {
    return 42
}

func use_bar_and_baz() uses (Bar, Bax) -> Int {
    return 43
}

