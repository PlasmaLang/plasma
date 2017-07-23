# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Arity

func main() -> Int using IO {
    foo!(int_to_string(bar(3, 5)) ++ "\n")
    return 0
}

# Test a function that returns nothing.
func foo(x : String) using IO {
    print!(x)
}

# A function that returns one thing.
func bar(a : Int, b : Int) -> Int {
    return a + b
}

