/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Trim

export
func trim(s : String) -> String {
    return trim_right(trim_left(s))
}

/*
 * This might be how we implement this in the future, but we lack the
 * language features:
 *  * while loops
 *  * state variables
 *  * object.func() syntax
 */
/*
func trim_left(s : String) -> String {
    var $pos = s.begin()

    while not $pos.at_end() {
        if $pos.next_char().class() != WHITESPACE
            break
        $pos = $pos.next()
    }

    return string_substring($pos, s.end())
}
*/

func trim_left(s : String) -> String {
    func loop(pos : StringPos) -> String {
        match (strpos_next(pos)) {
            Some(var c) -> {
                match char_class(c) {
                    Whitespace -> { return loop(strpos_forward(pos)) }
                    Other -> { return string_substring(pos, string_end(s)) }
                }
            }
            None -> {
                // We're at the end of the string
                return ""
            }
        }
    }

    return loop(string_begin(s))
}

func find_last(test : func(CodePoint) -> Bool,
               string : String) -> StringPos {
    func loop(pos : StringPos) -> StringPos {
        // We can't fold these tests into one because Plasma's || isn't
        // necessarily short-cutting.
        match (strpos_prev(pos)) {
            Some(var c) -> {
                if test(c) {
                    return pos
                } else {
                    return loop(strpos_backward(pos))
                }
            }
            None -> {
                return pos
            }
        }
    }

    return loop(string_end(string))
}

func trim_right(s : String) -> String {
    func is_not_whitespace(c : CodePoint) -> Bool {
        return match (char_class(c)) {
            Whitespace -> False
            Other -> True
        }
    }

    return string_substring(string_begin(s), find_last(is_not_whitespace, s))
}

