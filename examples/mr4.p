/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Mr4

/*
 * This is Mr 4's first computer program, 2019-12-01.  Okay so he had some
 * help but I'm still impressed that he understood variables (but not
 * functions).
 */
entrypoint
func main() uses IO -> Int {
    func greeting(name : String) uses IO {
        print!("Hello " ++ name ++ "\n")
        print!("Goodbye " ++ name ++ "\n")
    }

    // Name redacted until he can consent to internet privacy.
    greeting!("Mr 4") 
    greeting!("Daddy")
    greeting!("Mummy")

    return 0
}

