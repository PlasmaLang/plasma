/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Entrypoint2b

import Entrypoint2a as T2a

func test2b() uses IO -> Int {
    return T2a.test2a!()
}

