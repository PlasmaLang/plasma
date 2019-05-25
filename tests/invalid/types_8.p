/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Types_8

// Test type parameters.

func main() uses IO -> Int {
    // TODO: Improve the typechecker's error handling and add tests that
    // mirror those in valid/types_8.p

    print!(baz(Troz(Zort(return3))) ++ "\n")

    return 0
}

func return3() -> Int { return 3 } 

type Troz(x) = Troz(x : x)
type Zort(x) = Zort(x : x)

func baz(t : Troz(Zort(func() -> q))) -> q {
    match (t) {
        Troz(z) -> {
            match (z) {
                Zort(f) -> { return f() }
            }
        }
    }
}

