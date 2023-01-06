/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Match_Bad_3

type Foo = Foo ( a : Int, b : Int )

export
func main() uses IO -> Int {
    var x = Foo(2, 170)
    var y
    match (x) {
        // Error, variable used twice in the same pattern
        Foo(var a, var a) -> {
            y = a + a
        }
    }

    print!("Number is " ++ int_to_string(y) ++ "\n")
    return 0
}

