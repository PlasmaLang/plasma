/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Types_4

func main() uses IO -> Int {
    var list1 = MyCons(1, MyCons(2, MyCons(3, MyCons(4, MyNil))))
    var list2 = MyCons("Aa", MyCons("Bb", MyCons("Cc", MyNil)))
    
    print!(int_to_string(list_length(append(list1, list2))) ++ "\n")
    
    return 0
}

// Demonstrate an abstract type.
type MyList('a) = MyNil | MyCons ( head : 'a, tail : MyList('a) )

func list_length(l : MyList('t)) -> Int {
    match (l) {
        MyNil -> { return 0 }
        MyCons(_, var rest) -> { return 1 + list_length(rest) }
    }
}

func append(l1 : MyList('a), l2 : MyList('a)) -> MyList('a) {
    match (l1) {
        MyNil -> { return l2 }
        MyCons(var head, var tail) -> {
            return MyCons(head, append(tail, l2))
        }
    }
}

