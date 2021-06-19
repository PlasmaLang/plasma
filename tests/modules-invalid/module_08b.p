/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_08b

// Import C but not D, show that we can use both C's resources, even the
// one that depends on D.
import Module_08.C as C

// These resources shouldn't exist in this environment.
func bad_use() uses (D.Res2, Module_08.D.Res2) {
}

