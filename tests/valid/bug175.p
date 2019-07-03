/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Bug175

func main() uses IO -> Int {
    var x = 3
    if (x == 4) {
    } else {
        print!("Hello\n")
    }

    match (x) {
        0 -> { print!("bye?") }
        3 -> { }
        _ -> { print!("hello?") }
    }

    return 0
}

