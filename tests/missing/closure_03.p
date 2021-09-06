/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_03 

/*
 * The type checker seems to be giving an over-specific type to some of the
 * higher order code here that later causes a type conflict.
 */
func test1() uses IO -> String {
    func map(f : func('a) -> 'b, m : Maybe('a)) -> Maybe('b) {
        return match(m) {
            None -> None
            Some(var x) -> Some(f(x))
        }
    }

    return maybe_str(map(int_to_string, map(plus1, None)))
}

// There's a different error when you pass a Some(3) in
func test2() uses IO -> String {
    func map(f : func('a) -> 'b, m : Maybe('a)) -> Maybe('b) {
        return match(m) {
            None -> None
            Some(var x) -> Some(f(x))
        }
    }

    return maybe_str(map(int_to_string, map(plus1, Some(3))))
}

func plus1(x : Int) -> Int {
    return x + 1
}

func maybe_str(m : Maybe(String)) -> String {
    return match(m) {
        None -> "None"
        Some(var x) -> "Some(" ++ x ++ ")"
    }
}

