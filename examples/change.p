/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Change

import String
import Util

entrypoint
func main() uses IO -> Int {
    // Plasma should add while loops as well as the planned for loops.  This
    // is a good example of where a while loop would be helpful.
    func loop() uses IO -> Bool {
        print!("Type a number of cents to give as change: ")
        match (readline!()) {
            Ok(var line) -> {
                var str = String.trim(line)
                if (not string_equals(str, "")) {
                    print!("Trimmed string is: " ++ str ++ ".\n")
                    var num = String.str_to_num(str)
                    print!("Which is the value: " ++ currency_str(num) ++
                        "\n")
                    var coins = change(num)
                    print!("To give this amount in /perfect/ change you " ++
                        "should give:\n")
                    Util.do_for!(print_coin, coins)
                    return True
                } else {
                    return False
                }
            }
            EOF -> {
                return False
            }
        }
    }

    Util.while!(loop)

    return 0
}

func currency_str(num : Int) -> String {
    var cents = num % 100
    var dollars = num / 100
    return "$" ++ int_to_string(dollars) ++ "." ++ 
        (if cents < 10 then "0" else "") ++ 
        int_to_string(cents)
}

type CoinNum = CoinNum ( 
    c : Coin,
    n : Int
)

type Coin = c1
          | c5
          | c10
          | c20
          | c50
          | d1
          | d2

func change(n : Int) -> List(CoinNum) {
    if (n < 1) {
        return []
    } else {
        var coin
        var value
        if n >= 200 {
            coin = d2
            value = 200
        } else if n >= 100 {
            coin = d1
            value = 100
        } else if n >= 50 {
            coin = c50
            value = 50
        } else if n >= 20 {
            coin = c20
            value = 20
        } else if n >= 10 {
            coin = c10
            value = 10
        } else if n >= 5 {
            coin = c5
            value = 5
        } else {
            coin = c1
            value = 1
        }

        var num = n / value
        var rem = n - value * num
        return [CoinNum(coin, num) | change(rem)] 
    }
}

func coin_name(coin : Coin) -> String {
    return match (coin) {
        c1  -> "1c"
        c5  -> "5c"
        c10 -> "10c"
        c20 -> "20c"
        c50 -> "50c"
        d1  -> "$1"
        d2  -> "$2"
    }
}

func print_coin(c : CoinNum) uses IO {
    CoinNum(var coin, var num) = c
    print!(int_to_string(num) ++ " x " ++ coin_name(coin) ++ "\n")
}

