
# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module HO_8

export main
import IO

func main() uses IO -> Int {
    test()
    return 0
}

func test() {
    # Basic HO use.
    # These currently generate confusing error messages, but it's still
    # something we can test.
    x = hello_msg
    x!("Paul")
}

func hello_msg(name : String) uses IO {
    print!("Hello " ++ name ++ "\n")
}

