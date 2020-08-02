/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

// Module declaration, this sets the name of the module.
module Hello

// The main function, there's multiple things in the signature:
//  * It has zero parameters but in the future in the future it will
//    probably take an argument for command line options.
//  * It returns Int.
//  * It uses the IO resource.
//  * It is exported (callable from other modules).

export
func main() uses IO -> Int {
    // the ! indicates that this call uses a resource, which resource is
    // determined automatically.
    print!("Hello world\n")

    // 0 is the operating system's exit code for success.  This should be
    // symbolic in the future.
    return 0
}

