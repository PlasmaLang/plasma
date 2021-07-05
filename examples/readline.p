/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Readline

entrypoint
func hello() uses IO -> Int {
    print!("What's your name? ")
    // Readline returns a line from standard input without the newline
    // character.
    var name = readline!()
    print!("Hello " ++ name ++ "\n")

    // 0 is the operating system's exit code for success.  This should be
    // symbolic in the future.
    return 0
}

