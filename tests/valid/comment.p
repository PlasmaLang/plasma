/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Comment

func main() uses IO -> Int {
    // We support coments like this.
    /* and like this */

    str = "" ++ // this comments out the rest of the line
        "1" ++ /* this commented out */ "e" ++ // but the line continued
        "2" ++ /**/ "e" ++
        "3" ++ /* * */ "e" ++
        "4" ++ /* ** */ "e" ++
        "5" ++ /* *** */ "e" ++
        "6" ++ /*** */ "e" ++
        "7" ++ /* // aq */ "e" ++
        "8" ++ // /*<- not the beginning of a comment.
            "e" ++
        ""
    /*
     * Note that we don't support a * next to the ending star-slash due to
     * limitations in the regex library.
     */

    print!(str ++ "\n")

    print!(" // comment in a string, not realy a comment.\n")
    print!(" /* comment in a string, not realy a comment.\n")
    print!(" */ comment in a string, not realy a comment.\n")

    return 0
}

