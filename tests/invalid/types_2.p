/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Types_2

func main() uses IO -> Int {
    return 0
}

// List is not a concrete type.
type MyList(a) = MyNil | MyCons ( head : a, tail : MyList )

func list_length(l : MyList(t, w)) -> Int {
    match (l) {
        MyNil -> { return 0 }
        MyCons(_, var rest) -> { return 1 + list_length(rest) }
    }
}

