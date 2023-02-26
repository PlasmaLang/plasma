/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Parametric 

entrypoint
func main() uses IO -> Int {
    var list1 = MyCons(1, MyCons(2, MyCons(3, MyCons(4, MyNil))))
    print!(int_to_string(list_length(list1)) ++ "\n")
    
    var list2 = MyCons("A", MyCons("B", MyCons("C", MyNil)))
    print!(int_to_string(list_length(list2)) ++ "\n")

    // Oh, if we use strings of length one the typechecker can't decide if
    // these are strings of charaters.  It doesn't matter for our program.
    // So by doing this we give the typechecker enough information to
    // resolve it.
    _ = MyCons("a string", list2)
    
    return 0
}

// Demonstrate a parametric type.
type MyList('a) = MyNil | MyCons ( head : 'a, tail : MyList('a) )

func list_length(l : MyList('t)) -> Int {
    match (l) {
        MyNil -> { return 0 }
        MyCons(_, var rest) -> { return 1 + list_length(rest) }
    }
}

// Attempt to confuse type inference:

// This type has constructor symbols with the same names as above.
type OtherList('a) = MyCons ( ohead : 'a, otail : OtherList('a) ) | ONil

// Again with different type variable nmaes,
type OtherList2('b) = MyCons ( o2head : 'b, o2tail : OtherList('b) ) | ONil


