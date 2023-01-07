/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module EntrypointNone 

// This is exported but not marked as an entrypoint, the linker will be
// unable to find an entrypoint in the program.
export
func main() uses IO -> Int {
    // the ! indicates that this call uses a resource, which resource is
    // determined automatically where possible.
    print!("Hello world\n")

    // The value of a function (or block) is the value of its last
    // statement.
    // XXX EXIT_SUCCESS
    return 0
}

