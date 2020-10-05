/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_01

// The import declaration works, it causes the interface file to be read.
import Module_01a

entrypoint
func main() uses IO -> Int {
    // The calls to the imported functions work.
    Module_01a.printMessage!("Hello " ++ Module_01a.getMessage())

    return 0
}

