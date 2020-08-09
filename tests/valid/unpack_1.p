/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Unpack_1 

export
func main() uses IO -> Int {
    test1!()
    return 0
}

type Point = Point(x : Int, y : Int)

func test1() uses IO {
    // For now we can only extract a single variable.
    Point(var x, _) <- Point(3, 6)
    print!("x part is " ++ int_to_string(x) ++ "\n")
}

