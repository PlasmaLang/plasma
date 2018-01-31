# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module HO_7 

export main
import io

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

