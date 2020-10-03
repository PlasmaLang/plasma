/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_06a

export abstract
type Foo = Foo ( a : Int )

export
func makeFoo(n : Int) -> Foo {
    return Foo(n*3)
}

export
func fooStr(f : Foo) -> String {
    Foo(var n) = f
    return "Foo(" ++ int_to_string(n) ++ ")"
}

