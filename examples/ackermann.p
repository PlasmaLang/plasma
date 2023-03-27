/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Ackermann

entrypoint
func main() uses IO -> Int {
    test_ack!(3, 9)
    return 0
}

func test_ack(m : Int, n : Int) uses IO {
    var ans = ack(m, n)

    var m_str = int_to_string(m)
    var n_str = int_to_string(n)
    var ans_str = int_to_string(ans)

    print!("ack(" ++ m_str ++ ", " ++ n_str ++ ") = " ++ ans_str ++ "\n")
}

func ack(m : Int, n : Int) -> Int {
    return if m == 0
        then n + 1
        else if n == 0
            then ack(m - 1, 1)
            else ack(m - 1, ack(m, n - 1))
}

