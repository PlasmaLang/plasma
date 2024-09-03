
/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module PragmaBadArgs 

pragma foreign_include("globly", "glorp")

entrypoint
func hello() uses IO -> Int {
    print!("Hello world\n")
    return 0
}

