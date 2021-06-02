/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_11.A

import Module_11.B as B

export
resource Res1 from IO
export
resource Res3 from B.Res2

export
type Type1 = Struct1 ( n : Int )

