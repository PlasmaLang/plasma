/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Readline

import Trim
import Util

entrypoint
func hello() uses IO -> Int {
    func loop() uses IO -> Bool {
        print!("What's your name (empty to exit)? ")
        // Readline returns a line from standard input without the newline
        // character.
        var name = Trim.trim(readline!())
        if (not string_equals(name, "")) {
            print!("Hello " ++ name ++ ".\n")
            return True
        } else {
            return False
        }
    }
    Util.while!(loop)

    print!("Some trim examples:\n")
    func do_ex(s : String) uses IO {
        print!("Trim of '" ++
            s ++ 
            "' is '" ++
            Trim.trim(s) ++
            "'\n")
    }
    map!(do_ex, ["", "   ", "  Paul", "Paul   ", " Paul Bone ",
        " \na quick brown fox \t  "])

    // 0 is the operating system's exit code for success.  This should be
    // symbolic in the future.
    return 0
}

func map(f : func('x) uses IO, l : List('x)) uses IO {
    match (l) {
        []               -> {}
        [var x | var xs] -> {
            f!(x)
            map!(f, xs)
        }
    }
}

