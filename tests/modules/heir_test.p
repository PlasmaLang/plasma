/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module HeirTest

// Test modules that are organised in a heirachy.  Also test importing them
// _to_ different names and multiple times.
import Heir.Foo
import Heir.Foo as Foo
import Heir.Foo as F
import Heir.Foo.Bar

entrypoint
func main() uses IO -> Int {
    Heir.Foo.test1!()
    Heir.Foo.Bar.test!()
    Foo.test1!()
    F.test1!()

    return 0
}

