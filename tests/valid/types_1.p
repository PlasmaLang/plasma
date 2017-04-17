# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Types_1

# Simple enum
type Suit = Hearts | Diamonds | Spades | Clubs

func main() -> Int using IO {
    print!("Queen of " ++ suit_str(Hearts) ++ "\n")
    print!("Ace of " ++ suit_str(Spades) ++ "\n")
    return 0
}

func suit_str(s : Suit) -> String {
    match (s) {
        Hearts -> { return "Hearts" }
        Diamonds -> { return "Diamonds" }
        Spades -> { return "Spades" }
        Clubs -> { return "Clubs" }
    }
}

