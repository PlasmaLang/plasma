/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_09_RefUnexported

// Forces the compiler to build an interface file.  But if this test is
// working should be unsuccesful.
import Module_09_RefUnexported_a

entrypoint
func main() uses IO -> Int {
    print!("Hello\n")
    return 0
}

