/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Entrypoint3b

import Entrypoint3a as T3a

func test3b() uses IO -> Int {
    return T3a.test3a!()
}

