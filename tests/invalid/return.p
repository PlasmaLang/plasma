/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Return_1 

import io

func foo() uses IO -> Int {
    // The arity of the expression matches, but there's no explicit return
    // statement.  This was silently accepted as correct.
    return1()
}

// The same but when there's no statements at all.
func bar() -> Int { }

func return1() -> Int {
    return 3
}

