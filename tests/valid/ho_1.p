# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

# Hello module declaration, this gives the name of the module.
module HO_1 

export main
import io

# TODO:
#  Need to implement and test HO values in type arguments

func f1(a : Int) -> Int { return a + 1 }
func f2(a : Int) -> Int { return a * 2 }
func f3(a : Int) -> Int { return pow(a, 3) }

func main() -> Int uses IO {
    # Basic HO call
    print!(apply(hello_msg, "Paul"))

    # Reduce a function over a list.
    print!(int_to_string(reduce(add, up_to(10), 0)) ++ "\n")

    # Store functions in data.
    l = map(apply_to_12, [f1, f2, f3])

    # TODO ho code with a resource.
    print_each!(l)

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
        [] ->       { return a }
        [x | xs] -> { return f(x, reduce(f, xs, a)) }
    }
}

func map(f : func(x) -> (y), l : List(x)) -> List(y) {
    match (l) {
        [] ->       { return [] }
        [x | xs] -> { return [f(x) | map(f, xs)] }
    }
}

func apply_to_12(f : func(Int) -> (y)) -> y { return f(12) }

func print_each(l : List(Int)) uses IO {
    match (l) {
        [] ->       {
            print!("\n")
        }
        [x | xs] -> {
            print!(int_to_string(x) ++ " ")
            print_each!(xs)
        }
    }
}

####

func add(a : Int, b : Int) -> Int {
    return a + b
}

func pow(a : Int, b : Int) -> Int
{
    match b {
        0 -> { return 1 }
        1 -> { return a }
        n -> { return a * pow(a, n-1) }
    }
}

func up_to(a : Int) -> List(Int) {
    if (a == 0) {
        return []
    } else {
        return [a | up_to(a - 1)]
    }
}

