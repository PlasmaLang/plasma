# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

# Hello module declaration, this gives the name of the module.
module HO_1 

export main
import io

# TODO:
#  Need to implement and test HO values in type arguments

func main() -> Int uses IO {
    print!(apply(hello_msg, "Paul"))

    return 0
}

func hello_msg(name : String) -> String {
    return "Hello " ++ name ++ "\n"
}

func apply(f : func(a) -> (b), arg : a) -> b {
    return f(arg)
}

