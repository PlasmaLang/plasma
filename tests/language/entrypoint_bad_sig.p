/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module EntrypointBadSig 

/*
 * This program has an entrypoint function, but it has an incorrect
 * signature for an entrypoint.
 */

entrypoint
func main(foo : String) uses IO -> Int {
    // the ! indicates that this call uses a resource, which resource is
    // determined automatically where possible.
    print!("Hello world\n")

    // The value of a function (or block) is the value of its last
    // statement.
    // XXX EXIT_SUCCESS
    return 0
}

