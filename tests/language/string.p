/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module String

entrypoint
func example() uses IO -> Int { 
    // We can print strings
    print!("This is a string\n")
    
    // Assign them to variables
    var s1 = "abc"
    var s2 = "def"

    // append them
    print!("Append: " ++ s1 ++ s2 ++ "\n")

    // A single character in quotes can be a string
    var dot = "."
    var nl = "\n"
    print!("The End" ++ dot ++ nl)

    // Or a codepoint (aka character)
    print!("A codepoint: " ++ codepoint_to_string(".") ++ nl)
    print!("A codepoint: " ++ codepoint_to_string("\n") ++ nl)
    print!("A codepoint: " ++ codepoint_to_string(
        Builtin.int_to_codepoint(113)) ++ nl)

    return 0
}

