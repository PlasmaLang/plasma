/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

// The filename is module_example, note that they don't have to match by case
// or underscores.
module ModuleExample

// Import a module
import ModuleToImport

entrypoint
func main() uses IO -> Int {
    ModuleToImport.test!()
    return 0
}

