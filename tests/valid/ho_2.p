# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module HO_2

export main
import IO

# Test higher-order code that uses resources

func main() uses IO -> Int {
    # Ho code with a resource. TODO: Polymorphic resource use.
    do_for!(print_one, [1, 2, 3])
    print!("\n")

    # Put a higher order thing in a structure, then use it.
    x = MyType(print_wrap)
    do!(x, "Hi\n")

    return 0
}

func print_one(n : Int) uses IO {
    print!(int_to_string(n) ++ ", ")
}

func print_wrap(s : String) uses IO { print!(s) }

func do_for(f : func(x) uses IO, l : List(x)) uses IO {
    match (l) {
        [] -> {}
        [x | xs] -> {
            f!(x)
            do_for!(f, xs)
        }
    }
}

type MyType(x) = MyType(x : x)

func do(tf : MyType(func(x) uses IO), x : x) uses IO {
    match (tf) {
        MyType(f) -> { f!(x) }
    }
}
