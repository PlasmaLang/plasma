/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ExportBadType

type T0 = T0 ( 
    f1 : Int
)

// Error, T0 is not exported.
export
type T1 = T1 (
    f2 : T0
)

// Error, T0 is not exported.
export
type T2 = T2 (
    f3 : List(T0)
)

