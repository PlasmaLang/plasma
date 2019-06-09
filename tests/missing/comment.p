/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Hello

export main

import io

func main() uses IO -> Int {
    print!("Hello world\n")

    // This will match the second */ and so we must throw an error if there's a
    // */ within a coment.

    // It's an odd number of *'s in the middle to trigger the test, but try
    // with an even number too and we'll know if the test changes.
    /*****/  /* */
    /****/  /* */

    return 0
}

