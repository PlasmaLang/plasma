# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Types_5

func main() -> Int uses IO {
    List1 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    List2 = Cons("A", Cons("B", Cons("C", Nil)))
    
    print!(int_to_string(list_length(append(List1, List2))) ++ "\n")
    
    return 0
}

# Demonstrate an abstract type.
type List(a) = Nil | Cons ( head : a, tail : List(a) )

func list_length(l : List(t)) -> Int {
    match (l) {
        Nil -> { return 0 }
        Cons(_, rest) -> { return 1 + list_length(rest) }
    }
}

# Type error here, because a != b.
func append(l1 : List(a), l2 : List(b)) -> List(a) {
    match (l1) {
        Nil -> { return l2 }
        Cons(head, tail) -> {
            return Cons(head, append(tail, l2))
        }
    }
}

