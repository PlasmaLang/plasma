# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Arity

export main

import io

func main() uses IO -> Int {
    # Arity mismatch in call
    print!(hello())
    bar!()
    return 0
}

func bar() uses IO -> Int {
    # Arity mismatch in return.
    return
}

func bar2() uses IO {
    return 3
}

func test1() uses IO {
    # It is an error not to capture the returned values when there are some.
    cube(3)
}

func test2() uses IO {
    cube(2)
    return
}

func test3() uses IO {
    # There are no returned values here, this is an arity mismatch.
    _ = print!("Boo\n")
}

func test4() uses IO {
    _ = print!("Boo\n")
    return
}



func cube(n : Int) -> Int { return n * n * n }

func hello() -> (String, Int) {
    return "Hi", 3
}

