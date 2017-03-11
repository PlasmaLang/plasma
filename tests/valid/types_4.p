# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Types_4

# Simple enum
type List = Nil | Cons ( head :: Int, tail :: List )

func main() -> Int using IO {
    print!(list_str(Cons(1, Cons(2, Cons(3, Nil)))) ++ "\n")
    return 0
}

func list_str(c :: List) -> String {
    match (c) {
        Nil -> { return "" }
        Cons(n, l) -> { return int_to_string(n) ++ list_str2(l) }
    }
}

func list_str2(c :: List) -> String {
    match (c) {
        Nil -> { return "" }
        Cons(n, l) -> { return ", " ++ int_to_string(n) ++ list_str2(l) }
    }
}

