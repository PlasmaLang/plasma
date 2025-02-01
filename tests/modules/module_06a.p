/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_06a

// export resources

export
resource Foo from IO

export
resource Bar from Foo

resource Baz from Foo

export
func troz() uses Bar {
}

