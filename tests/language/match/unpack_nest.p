/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module UnpackNest

export
func main() uses IO -> Int {
    test3!()
    return 0
}

type Foo = Foo(a : Bar, b : Int)

// We have to test with the unit type to make an irrefutable pattern
type Bar = Bar

func foo_to_str(f : Foo) -> String {
    Foo(Bar, var n) = f
    return "Foo(Bar, " ++ int_to_string(n) ++ ")"
}

func test3() uses IO {
    var a = Foo(Bar, 28)
    print!("a = " ++ foo_to_str(a) ++ "\n")
}

