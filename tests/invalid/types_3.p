# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Types_3

func main() -> Int using IO {
    return 0
}

# Type variable b is not on the LHS.
type List(a) = Nil | Cons ( head : a, tail : List(b) )

