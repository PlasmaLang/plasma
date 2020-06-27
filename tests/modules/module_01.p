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
    print!("Hello world\n")

    return 0
}

