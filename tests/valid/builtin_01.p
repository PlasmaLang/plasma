/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Builtin01

/*
 * This test should be kept up-to-date with the documentation for the
 * builtins in docs/plasma_ref.txt
 */
entrypoint
func main() uses IO -> Int {
    test_maybe!()

    return 0
}

/* ****************************************** */

func test_maybe() uses IO {
    var a = None
    var b = Some(23)
    func maybe_str(m : Maybe(String)) -> String {
        return match(m) {
            None -> "None"
            Some(var x) -> "Some(" ++ x ++ ")"
        }
    }

    print!("Map result: " ++
           maybe_str(maybe_map(int_to_string, maybe_map(plus1, a))) ++
           "\n") 
    print!("Map result: " ++
           maybe_str(maybe_map(int_to_string, maybe_map(plus1, b))) ++
           "\n")
}

func maybe_map(f : func('a) -> 'b, m : Maybe('a)) -> Maybe('b) {
    return match(m) {
        None -> None
        Some(var x) -> Some(f(x))
    }
}

func plus1(x : Int) -> Int {
    return x + 1
}

/* ****************************************** */

