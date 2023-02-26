/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module HO_Bad_6

func main() uses IO -> Int {
    print!(apply(hello_msg, "Paul"))

    return 0
}

func hello_msg(name : String) -> String {
    return "Hello " ++ name ++ "\n"
}

func apply(f : func('a) -> ('b), arg : 'a) -> 'b {
    // Expect f to take two paramers when it takes only 1.
    var b = f(arg, 5)
    return b
}

