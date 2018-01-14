# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Types_3

# Simple enum
type Suit = Hearts | Diamonds | Spades | Clubs
type Card = Card( c_suit : Suit, c_face : Int )

func main() uses IO -> Int {
    print!(card_str(Card(Hearts, 12)) ++ "\n")
    print!(card_str(Card(Spades, 1)) ++ "\n")
    print!(card_str(Card(Clubs, 3)) ++ "\n")
    print!(card_str(Card(Diamonds, 0)) ++ "\n")
    return 0
}

func card_str(c : Card) -> String {
    match (c) {
        Card(s, f) -> { return face_str(f) ++ " of " ++ suit_str(s) }
    }
}

func suit_str(s : Suit) -> String {
    match (s) {
        Hearts -> { return "Hearts" }
        Diamonds -> { return "Diamonds" }
        Spades -> { return "Spades" }
        Clubs -> { return "Clubs" }
    }
}

func face_str(f : Int) -> String {
    match (f) {
        1 -> { return "Ace" }
        11 -> { return "Jack" }
        12 -> { return "Queen" }
        13 -> { return "King" }
        _ -> {
            if (f < 2) or (f > 10) {
                return "INVALID"
            } else {
                return int_to_string(f)
            }
        }
    }
}

