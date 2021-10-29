/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ImportFunction

func getpid() -> Int
    foreign

entrypoint
func hello() uses IO -> Int {
    print!("Hello world\n")

    var pid = getpid!()
    print!("# My pid is " ++ int_to_string(pid) ++ "\n")

    var pid2 = getpid!()
    if (pid == pid2) {
        print!("My pid didn't change\n")
    } else {
        print!("My pid changed, that's weird\n")
    }

    return 0
}

