/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

// The filename is module_example, note that they don't have to match by case
// or underscores.
module ModuleToImport

export
func test() uses IO {
    print!("Hello from ModuleToImport\n")
}

