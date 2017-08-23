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

resource Foo from IO
resource Bar from Foo

func use_bar_call_foo() -> Int uses Bar {
    # We need the parent resource for this call.  Bar isn't enough.
    return use_foo!()
}

func use_foo() -> int uses Foo {
    # We need a sibling resource for this call.  Foo isn't enough.
    setenv!("abc", "xyz")
    return 3
}

