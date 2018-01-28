# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module HO_1 

export main
import io

# TODO:
#  Need to implement and test HO values in type arguments

func f1(a : Int) -> Int { return a + 1 }
func f2(a : Int) -> Int { return a * 2 }
func f3(a : Int) -> Int { return pow(a, 3) }

func main() uses IO -> Int {
    # Basic HO call
    print!(apply(hello_msg, "Paul"))

    # Reduce a function over a list.
    print!(int_to_string(reduce(add, up_to(10), 0)) ++ "\n")

    # Store functions in data.
    l = map(apply_to_12, [f1, f2, f3])

    # Ho code with a resource. TODO: Polymorphic resource use.
    do_for!(print_one, l)
    print!("\n")

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

func print_one(n : Int) uses IO {
    print!(int_to_string(n) ++ ", ")
}

func do_for(f : func(x) uses IO, l : List(x)) uses IO {
    match (l) {
        [] -> {}
        [x | xs] -> {
            f!(x)
            do_for!(f, xs)
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

