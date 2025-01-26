/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Entrypoint1a

// Function is not exported.
entrypoint
func test1a() uses IO -> Int {
    print!("Test 1\n")

    return 0
}

