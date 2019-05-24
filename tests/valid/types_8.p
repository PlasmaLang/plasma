/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Types_8

// Type parameters.

func main() uses IO -> Int {
    // Type checking must accept this, a, and a are the same.
    print_list!(int_to_string, foo([1, 2], [3, 4]))

    // Even if the full type of the second a is unknown, there's enough to
    // constrain it.
    print_list!(int_to_string, foo([1, 2], []))

    // They're also allowed to be the same if bar accepts a and b.
    print_list!(int_to_string, bar([1, 2], [3, 4]))

    // This also works when the type parameter is buried deep within a type
    // expression in the callee's declaration.
    print!(int_to_string(baz(Troz(Zort(return3)))) ++ "\n")

    return 0
}

// Some of the same except from a polymorphic context
func test2(a1 : a, c : c, a2 : a, la1 : List(a), la2 : List(a)) uses IO {
    // Type checking must accept this, a, and a are the same.
    _ = foo(a1, a1)
    _ = foo(a1, a2)
    _ = foo(la1, la2)

    // They're also allowed to be the same if bar accepts a and b.
    _ = bar(a1, a1)
    _ = bar(a1, a2)
    _ = bar(la1, la2)
}

func foo(a1 : a, a2 : a) -> a {
    return a1
}

func bar(a : a, b : b) -> b {
    return b
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

/*-----*/

func print_list(f : func(a) -> String, l0 : List(a)) uses IO {
    print!(join(", ", (map(f, l0))) ++ "\n")
}

func map(f : func(a) -> b, l : List(a)) -> List(b) {
    match (l) {
        [] -> { return [] }
        [x | xs] -> { return [f(x) | map(f, xs)] }
    }
}

func join(j : String, l : List(String)) -> String {
    match (l) {
        [] -> {
            return ""
        }
        [x | xs] -> {
            match (xs) {
                [] -> {
                    return x
                }
                [_ | _] -> {
                    return x ++ j ++ join(j, xs)
                }
            }
        }
    }
}

