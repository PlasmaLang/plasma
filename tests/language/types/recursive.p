/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Recursive

entrypoint
func main() uses IO -> Int {
    print!(list_str(MyCons(1, MyCons(2, MyCons(3, MyNil)))) ++ "\n")
    print!(a_str(TermAB(TermBA(TermA(2), 2), 5)) ++ "\n")
    return 0
}

// Demonstrate a recursive type
type MyList = MyNil | MyCons ( head : Int, tail : MyList )

func list_str(c : MyList) -> String {
    match (c) {
        MyNil -> { return "" }
        MyCons(var n, var l) -> { return int_to_string(n) ++ list_str2(l) }
    }
}

func list_str2(c : MyList) -> String {
    match (c) {
        MyNil -> { return "" }
        MyCons(var n, var l) -> {
            return ", " ++ int_to_string(n) ++ list_str2(l)
        }
    }
}

// And mutually recursive types (and functions).
type TermA = TermA (ai : Int)
           | TermAB (ab : TermB, abi : Int)
type TermB = TermBA (ba : TermA, bai : Int)

func a_str(a : TermA) -> String {
    return int_to_string(a_int(a))
}

func a_int(a : TermA) -> Int {
    match (a) {
        TermA(var n) -> { return n }
        TermAB(var b, var n) -> { return b_int(b) + n }
    }
}

func b_int(b : TermB) -> Int {
    match(b) {
        TermBA(var a, var n) -> { return a_int(a) * n }
    }
}

