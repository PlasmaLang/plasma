/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module DynLink01 

// The import declaration works, it causes the interface file to be read.
import DynLink01A as A 

entrypoint
func main() uses IO -> Int {
    // The calls to the imported functions work.
    A.printMessage!("Hello " ++ A.getMessage())

    return 0
}

