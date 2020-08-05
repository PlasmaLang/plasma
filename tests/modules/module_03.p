/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_03

// The import declaration works, it causes the interface file to be read.
import Module_03a

export
func main() uses IO -> Int {
    test1!()

    Module_03a.Pair(var s, _) = test2!()
    print!("Str is " ++ s ++ "\n")

    return 0
}

func test1() uses IO {
    // Test that we can use constructors from types defined in another
    // module.
    var c = Module_03a.Card(Module_03a.Spade, Module_03a.Ace)
    print!("Your card is the " ++ Module_03a.card_str(c) ++ "\n")
}

// Test that we can reference types defined in another module, and the types
// may be polymorphic.
func test2() uses IO -> Module_03a.Pair(String, Int) {
    return Module_03a.Pair("Boo!", 28)
}

