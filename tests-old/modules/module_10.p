/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_10

import Module_10a
import Module_10b

entrypoint
func main() uses IO -> Int {
    Module_10a.printMessage!("Hello " ++ Module_10b.getMessage())

    return 0
}

