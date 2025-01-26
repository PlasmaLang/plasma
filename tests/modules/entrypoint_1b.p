/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Entrypoint1b

import Entrypoint1a as T1a

func test1b() uses IO -> Int {
    // Call should fail because although test2a is an entrypoin it is not
    // exported.
    return T1a.test1a!()
}

