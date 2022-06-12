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
type TypeA1 = StructA1 ( n : Int )
export
type TypeA3 = StructA3 ( t2 : B.TypeA2 )

export
type TypeB1('t) = StructB1 ( n : 't )
export
type TypeB3('t) = StructB3 ( t2 : B.TypeB2('t) )
