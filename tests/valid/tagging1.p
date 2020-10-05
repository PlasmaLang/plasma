/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Tagging1

// Simple enum
type Suit = Hearts | Diamonds | Spades | Clubs
type Type2 = A | B (bf : Int )
type Type3 = A | B | C (cf : Int) | D (df : String)

entrypoint
func main() uses IO -> Int {
    print!(suit_str(Diamonds) ++ "\n")
    print!(type2_str(A) ++ "\n")
    print!(type2_str(B(3)) ++ "\n")
    print!(type3_str(A) ++ "\n")
    print!(type3_str(B) ++ "\n")
    print!(type3_str(C(4)) ++ "\n")
    print!(type3_str(D("dee")) ++ "\n")
    return 0
}

func suit_str(s : Suit) -> String {
    match (s) {
        Hearts -> { return "Hearts" }
        Diamonds -> { return "Diamonds" }
        Spades -> { return "Spades" }
        Clubs -> { return "Clubs" }
    }
}

func type2_str(x : Type2) -> String {
    match (x) {
        A -> { return "A" }
        B(var v) -> { return int_to_string(v) }
    }
}

func type3_str(x : Type3) -> String {
    match (x) {
        A -> { return "A" }
        B -> { return "B" }
        C(var n) -> { return "C " ++ int_to_string(n) }
        D(var s) -> { return s }
    }
}

