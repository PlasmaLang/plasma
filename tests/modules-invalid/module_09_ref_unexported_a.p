/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_09_RefUnexported_a

// This isn't exported
resource R0 from IO

// But this one is and refers to it.  It should be an error since it can't
// be used properly.
export
resource R1 from R0

