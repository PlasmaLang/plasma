/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module HO_5

func main() uses IO -> Int {
    // hello_msg takes one argument but apply expects its first argument to
    // take two.
    print!(apply(hello_msg, "Paul"))

    return 0
}

func hello_msg(name : String) -> String {
    return "Hello " ++ name ++ "\n"
}

func apply(f : func('a) -> ('b), arg : 'a) -> 'b {
    var b, _ = f(arg)
    return b
}

