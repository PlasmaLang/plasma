/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module String

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
                match codepoint_category(c) {
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
        return match (codepoint_category(c)) {
            Whitespace -> False
            Other -> True
        }
    }

    return string_substring(string_begin(s), find_last(is_not_whitespace, s))
}

export
func str_to_num(s : String) -> Int {
    var base = 10

    func loop(pos : StringPos, num : Int) -> Int {
        var maybe_cp = strpos_next(pos)
        match (maybe_cp) {
            None -> {
                // End of input.
                return num
            }
            Some(var cp) -> {
                var maybe_digit = codepoint_to_digit(cp)
                match (maybe_digit) {
                    Some(var digit) -> {
                        return loop(strpos_forward(pos), num * base + digit)
                    }
                    None -> {
                        // Could make this function return a maybe.
                        Builtin.die("Bad number")
                        return 0
                    }
                }
            }
        }
    }
    return loop(string_begin(s), 0)
}

func codepoint_to_digit(cp : CodePoint) -> Maybe(Int) {
    var num = codepoint_to_number(cp)
    // Recognise the digits range within ASCII, I don't know if there are
    // other ranges for numbers in Unicode.
    if num <= 57 and num >= 48 {
        return Some(num - 48)
    } else {
        return None
    }
}

