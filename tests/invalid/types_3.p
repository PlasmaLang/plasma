# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Types_3

func main() uses IO -> Int {
    return 0
}

# Type variable b is not on the LHS.
type MyList(a) = MyNil | MyCons ( head : a, tail : MyList(b) )

