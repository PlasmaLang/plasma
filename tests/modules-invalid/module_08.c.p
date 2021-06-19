/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_08.C

import Module_08.D as D

export
resource Res1 from IO
export
resource Res3 from D.Res2

export
func test1() uses D.Res4 {
}

export
func test2() uses Res3 {
}

