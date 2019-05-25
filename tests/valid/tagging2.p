/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Tagging2

// Simple enum
type Type = A                 // ptag 0, value 0 
          | B                 // ptag 0, value 1
          | C (cf : Int)      // ptag 1
          | D (df : String)   // ptag 2
          | E (ef : Int)      // ptag 3, stag 0
          | F (ff : String)   // ptag 3, stag 1

func main() uses IO -> Int {
    print!(type_str(A) ++ "\n")
    print!(type_str(B) ++ "\n")
    print!(type_str(C(4)) ++ "\n")
    print!(type_str(D("dee")) ++ "\n")
    print!(type_str(E(53)) ++ "\n")
    print!(type_str(F("fFfFf")) ++ "\n")
    return 0
}

func type_str(x : Type) -> String {
    match (x) {
        A -> { return "A" }
        B -> { return "B" }
        C(n) -> { return "C " ++ int_to_string(n) }
        D(s) -> { return "D " ++ s }
        E(n) -> { return "E " ++ int_to_string(n) }
        F(s) -> { return "F " ++ s }
    }
}

