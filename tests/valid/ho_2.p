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

    return 0
}

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

