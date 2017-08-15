# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Res_1

export main

# Import modules that we'll need.
import io

func main() -> Int uses IO {
    # Calls without bang.
    foo()
    baz()

    # These calls are okay.
    foo!()
    baz!()

    # Unnecessary bang
    return bar!()
}

func foo() uses IO {
    print!("Hello world\n")
}

func bar() -> Int {
    # Use of a resource we don't have.
    print!("Hi\n")

    return 3
}

func baz() observes IO {
    # Use of a resource we only have read access to.
    print!("Hi baz\n")
}

# Function declares that it uses a resource but doesn't actually need it.
func troz() -> Int uses IO {
    return 6
}

