/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module OpaqueResource

export
resource Res1 from IO

export opaque
resource Res2 from Res1 

export
func do_with_res(f : func('t1) uses Res2 -> 't2, x : 't1) uses Res1 -> 't2 {
    return f!(x)
}

