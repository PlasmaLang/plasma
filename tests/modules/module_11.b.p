/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_11.B

import Module_11.A as A

export
resource Res2 from A.Res1
export
resource Res4 from A.Res3

export
type Type2 = Struct2 ( t1 : A.Type1 )
export
type Type4 = Struct4 ( t3 : A.Type3 )

