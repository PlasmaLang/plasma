# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Arity_1 

export main

import io

func main() -> Int using IO {
    # Arity mismatch in call
    print!(hello())
    bar!()
    return 0
}

func bar() -> Int using IO {
    # Arity mismatch in return.
}

func hello() -> String, Int {
    return "Hi", 3
}

