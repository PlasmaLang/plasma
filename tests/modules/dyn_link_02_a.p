/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module DynLink02A 

export
func printMessage(message : String) uses IO {
    print!(message ++ "\n")
}

