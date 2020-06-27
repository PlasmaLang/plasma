/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_01

// The import declaration works, it causes the interface file to be read.
import Module_01a

export
func main() uses IO -> Int {
    // The call to the imported function works.
    print!("Hello " ++ Module_01a.getMessage() ++ "\n")

    return 0
}

