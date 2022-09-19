/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

// When a program's module doesn't match the one listed in the build file it
// causes the build program to crash.

module BadModuleName2 

entrypoint
func main() uses IO -> Int {
    print!("Test!\n")
  
    return 0
}

