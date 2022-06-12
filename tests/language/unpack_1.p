/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Unpack_1

entrypoint
func main() uses IO -> Int {
    test1!()
    test2!()
    return 0
}

type Point = Point(x : Int, y : Int)

func test1() uses IO {
    // Initially we could only extract a single variable.
    Point(var x, _) = Point(3, 6)
    print!("x part is " ++ int_to_string(x) ++ "\n")
}

func point_to_str(p : Point) -> String {
    // Test that we can unpack fields from a structure
    Point(var x, var y) = p

    return int_to_string(x) ++ ", " ++ int_to_string(y)
}

func add(a : Point, b : Point) -> Point {
    Point(var x1, var y1) = a

    // We can declare the variables before unpacking them, like other
    // asignments.
    var x2
    var y2
    Point(x2, y2) = b

    return Point(x1 + x2, y1 + y2)
}

func test2() uses IO {
    var p1 = Point(3, 2)
    var p2 = Point(-2, 7)
    print!("p1 + p2 = " ++ point_to_str(add(p1, p2)) ++ "\n")
}

