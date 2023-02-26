/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Match_Bad_1

export
func main() uses IO -> Int {
    var x = 3
    var y
    match (x) {
        3 -> {
            y = 2
        }
        // yy does not exist
        yy -> {
            y = yy * 26
        }
    }

    print!(int_to_string(y))
    return 0
}

