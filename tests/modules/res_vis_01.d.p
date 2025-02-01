/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ResVis01.D

import ResVis01.C as C

export
resource Res2 from C.Res1
export
resource Res4 from C.Res3

export
type TypeA2 = StructA2 ( t2 : C.TypeA1 )
export
type TypeA4 = StructA4 ( t4 : C.TypeA3 )

export
type TypeB2('t) = StructB2 ( t2 : C.TypeB1('t) )
export
type TypeB4('t) = StructB4 ( t4 : C.TypeB3('t) )

