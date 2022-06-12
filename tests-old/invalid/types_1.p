/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Types_1

// Simple enum, but it is invalid because a constructor is duplicated.
type Suit = Hearts | Diamonds | Diamonds | Clubs

func main() uses IO -> Int {
    print!("Queen of " ++ suit_str(Hearts) ++ "\n")
    print!("Ace of " ++ suit_str(Clubs) ++ "\n")
    return 0
}

func suit_str(s : Suit) -> String {
    match (s) {
        Hearts -> { return "Hearts" }
        Diamonds -> { return "Diamonds" }
        Clubs -> { return "Clubs" }
    }
}

