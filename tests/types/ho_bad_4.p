/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module HO_Bad_4

func main() uses IO -> Int {
    // hello_msg returns one argument but apply expects it to return two.
    print!(apply(hello_msg, "Paul"))

    return 0
}

func hello_msg(name : String) -> String {
    return "Hello " ++ name ++ "\n"
}

func apply(f : func('a) -> ('b, Int), arg : 'a) -> 'b {
    var b, _ = f(arg)
    return b
}

