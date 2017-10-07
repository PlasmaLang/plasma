# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Types_5

func main() -> Int uses IO {
    List1 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    print!(int_to_string(list_length(List1)) ++ "\n")
    
    List2 = Cons("A", Cons("B", Cons("C", Nil)))
    print!(int_to_string(list_length(List2)) ++ "\n")
    
    return 0
}

# Demonstrate a parametric type.
type List(a) = Nil | Cons ( head : a, tail : List(a) )

func list_length(l : List(t)) -> Int {
    match (l) {
        Nil -> { return 0 }
        Cons(_, rest) -> { return 1 + list_length(rest) }
    }
}

# Attempt to confuse type inference:

# This type has constructor symbols with the same names as above.
type OtherList(a) = Cons ( ohead : a, otail : OtherList(a) ) | ONil

# Again with different type variable nmaes,
type OtherList2(b) = Cons ( o2head : b, o2tail : OtherList(b) ) | ONil


