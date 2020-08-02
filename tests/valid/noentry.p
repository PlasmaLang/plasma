/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module NoEntry

/*
 * This program has no main function and therefore doesn't have an
 * entrypoint. That's not an error until runtime which is why this test is
 * in the valid/ directory, the invalid/ tests are for tests that fail
 * compilation.
 */

export
func qqmain() uses IO -> Int {
    // the ! indicates that this call uses a resource, which resource is
    // determined automatically where possible.
    print!("Hello world\n")

    // The value of a function (or block) is the value of its last
    // statement.
    // XXX EXIT_SUCCESS
    return 0
}

