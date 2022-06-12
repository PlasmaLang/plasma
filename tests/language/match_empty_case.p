/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module MatchEmptyCase

entrypoint
func main() uses IO -> Int {
    var x = 3
    if (x == 4) {
        // The compiler would crash for an empty case like this.
    } else {
        print!("Hello\n")
    }

    match (x) {
        0 -> { print!("bye?") }
        // The compiler would crash for an empty case like this.
        3 -> { }
        _ -> { print!("hello?") }
    }

    return 0
}

