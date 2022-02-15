/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ExportBadResource

resource Foo from IO

/*
 * The error will be reported on the exported item that need a non-exported
 * resource.  It should (but doesn't) refer to the line of the non-exported
 * resource.  A non-exported thing can be reported more than once.
 */

// Error Foo is not exported and Bar expects this.
export
resource Bar from Foo

// also an error.
export
resource Bar2 from Foo

// Not an error
export
resource BarBar from Bar

// No error.
resource Baz from Foo
resource Baz2 from Foo

// Error Foo is not exported
export
func troz() uses Foo {
}

// Error Baz is not exported
export
func zort() uses Baz {
}

// No error.
func zort2() uses Baz {
}

// This time the error is in a type used by a function.
export
func silly_sound(x : func(Int) uses Baz) {
}

// Should have two errors, one for each resource.
export
func silly_sound2(x : func(Int) uses (Bar, Baz, Baz2)) {
}

// Should have only one error (same resource twice)
export
func silly_sound3(x : func(Int) uses (Baz, Bar), y : func(String) uses Baz)
    uses Baz
{
}

