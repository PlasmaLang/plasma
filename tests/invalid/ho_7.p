# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module HO_7 

export main
import io

func main() uses IO -> Int {
    # print_one uses a resource that do_for will not make available.
    do_for2!(print_one, [1, 2, 3])
    print!("\n")

    # Put a higher order thing in a structure, then use it but without the
    # correct resource.
    x = MyType(print)
    apply!(x, "Hi\n")

    return 0
}

####

func do_for1(f : func(x) uses IO, l : List(x)) uses IO {
    match (l) {
        [] -> {}
        [x | xs] -> {
            # Missing bang.
            f(x)
            do_for1!(f, xs)
        }
    }
}

func do_for2(f : func(x), l : List(x)) uses IO {
    match (l) {
        [] -> {}
        [x | xs] -> {
            # f doesn't use a resource.
            f!(x)
            do_for2!(f, xs)
        }
    }
}

func print_one(n : Int) uses IO {
    print!(int_to_string(n) ++ ", ")
}

####

type MyType(x) = MyType(x : x)

func apply(mt : MyType(func(x)), x : x) uses IO {
    match(mt) {
        MyType(f) -> { f(x) }
    }
}

func apply2(mt : MyType(func(x) uses IO), x : x) uses IO {
    match(mt) {
        # Call to f should have a !.
        MyType(f) -> { f(x) }
    }
}

###
