/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module NoEntry

// Import modules that we'll need.
import io

/*
 * The main function, returns int, takes a single argument, argv, which is
 * list of strings.  The main function uses the IO resource.
 */

// XXX: should have the parameter: argv : List(String)

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

