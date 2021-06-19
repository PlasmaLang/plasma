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

// Show that we can export a function that depends on another module's
// resource, which also isn't visible to Module_11.
export
func test2() uses D.Res2 {
}
// Same, but Res4 doesn't get implicitly created by this module's own
// resources (different path in the compiler).
export
func test3() uses D.Res4 {
}

export
type TypeA1 = StructA1 ( n : Int )
export
type TypeA3 = StructA3 ( t2 : D.TypeA2 )

export
type TypeB1('t) = StructB1 ( n : 't )
export
type TypeB3('t) = StructB3 ( t3 : D.TypeB2('t) )

