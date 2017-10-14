# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Coverage_1

type Suit = Hearts | Diamonds | Spades | Clubs

func main() -> Int uses IO {
    print!("Queen of " ++ suit_str(Hearts) ++ "\n")
    print!("Ace of " ++ suit_str(Spades) ++ "\n")
    return 0
}

func suit_str(s : Suit) -> String {
    # Uncovered data tag.
    match (s) {
        Hearts -> { return "Hearts" }
        Diamonds -> { return "Diamonds" }
        Clubs -> { return "Clubs" }
    }
}

func num_word(n : Int) -> String {
    # Uncovered data.
    match (n) {
        0 -> { return "zero" }
        1 -> { return "one" }
    }
}

func num_word2(n : Int) -> String {
    match (n) {
        0 -> { return "zero" }
        1 -> { return "one" }
        _ -> { return "many" }

        # This case is never tested.
        5 -> { return "five" }
    }
}

func suit_str2(s : Suit) -> String {
    match (s) {
        Hearts -> { return "Hearts" }
        Diamonds -> { return "Diamonds" }
        Clubs -> { return "Clubs" }
        Spades -> { return "Spades" }

        # This case is never tested.
        _ -> { return "Unknown" }
    }
}

func suit_str3(s : Suit) -> String {
    # Uncovered data tag.
    match (s) {
        Hearts -> { return "Hearts" }
        Diamonds -> { return "Diamonds" }

        # This case always fails.
        Diamonds -> { return "Girl's best friend" }
        Clubs -> { return "Clubs" }
        Spades -> { return "Spades" }
    }
}

func num_word3(n : Int) -> String {
    match (n) {
        0 -> { return "zero" }
        1 -> { return "one" }

        # This case always fails.
        1 -> { return "onesies" }

        _ -> { return "many" }
    }
}
