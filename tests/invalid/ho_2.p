/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module HO_2

func main() uses IO -> Int {
    print!(apply(hello_msg, "Paul"))

    return 0
}

func hello_msg(name : String) -> String {
    return "Hello " ++ name ++ "\n"
}

func apply(f : func(a) -> (b), arg : a) -> a {
    // Return type of f doesn't match apply's return type.
    return f(arg)
}

