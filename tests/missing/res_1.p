/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Res_1

import io

export
func main() uses IO -> Int {
    // Future: Use disjoint resources in the same statement.
    _ = use_env!() + test_gettimeofday!()

    // Future: Observe the same or related resources in the same statement.
    // XXX But not use and observe
    var d = test_gettimeofday!() - test_gettimeofday!()
    print!("# The difference between two times is: " ++ int_to_string(d) ++
        "\n")

    return 0
}

func use_env() uses Environment -> Int {
    return 0
}

func test_gettimeofday() observes Time -> Int {
    var b, s, us = gettimeofday!()
    if (b) {
        return s
    } else {
        return -1
    }
}

