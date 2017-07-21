# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Types_2

func main() -> Int using IO {
    return 0
}

# List is not a concrete type.
type List(a) = Nil | Cons ( head : a, tail : List )

func list_length(l : List(t, w)) -> Int {
    match (l) {
        Nil -> { return 0 }
        Cons(_, rest) -> { return 1 + list_length(rest) }
    }
}

