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
type TypeA2 = StructA2 ( t1 : A.TypeA1 )
export
type TypeA4 = StructA4 ( t3 : A.TypeA3 )

export
type TypeB2('t) = StructB2 ( t1 : A.TypeB1('t) )
export
type TypeB4('t) = StructB4 ( t3 : A.TypeB3('t) )

