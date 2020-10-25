/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_09

// The import declaration works, it causes the interface file to be read.
import Module_09a

entrypoint
func main() uses IO -> Int {
    // The calls to the imported functions work.
    Module_09a.printMessage!("Hello " ++ Module_09a.getMessage())

    return 0
}

