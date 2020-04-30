/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module HO_1 

func main() uses IO -> Int {
    // Only one of these will be raised until compiler error handling is
    // improved.

    // Type mismatched ho call passed in
    print!(apply(hello_msg, 3))

    // Return type mismatch:
    print!(int_to_string(apply(hello_msg, "ho")))

    // TODO different function types in homogenous array.

    return 0
}

func hello_msg(name : String) -> String {
    return "Hello " ++ name ++ "\n"
}

func apply(f : func('a) -> ('b), arg : 'a) -> 'b {
    return f(arg)
}

