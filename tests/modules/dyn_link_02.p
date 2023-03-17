/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module DynLink02 

import DynLink02A as A 
import DynLink02B as B

entrypoint
func main() uses IO -> Int {
    A.printMessage!("Hello " ++ B.getMessage())

    return 0
}

