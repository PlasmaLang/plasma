/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_05a

// Type references form a cycle.

export
type Foo = Foo ( a : Bar )

export
type Bar = Bar ( b : Foo ) 

