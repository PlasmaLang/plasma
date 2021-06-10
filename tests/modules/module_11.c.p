/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_11.C

import Module_11.D as D

export
resource Res1 from IO
export
resource Res3 from D.Res2

export
func test(f : func() uses Res3) uses Res1 {
    f!()
}

