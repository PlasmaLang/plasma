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
    print!(int_to_string(reduce(add, up_to(10), 0)) ++ "\n")

    return 0
}

func hello_msg(name : String) -> String {
    return "Hello " ++ name ++ "\n"
}

func apply(f : func(a) -> (b), arg : a) -> b {
    return f(arg)
}

func reduce(f : func(x, a) -> (a), l : List(x), a : a) -> a {
    match (l) {
        Nil ->         { return a }
        Cons(x, xs) -> { return f(x, reduce(f, xs, a)) }
    }
}

func add(a : Int, b : Int) -> Int {
    return a + b
}

func up_to(a : Int) -> List(Int) {
    if (a == 0) {
        return Nil
    } else {
        return Cons(a, up_to(a - 1))
    }
}

