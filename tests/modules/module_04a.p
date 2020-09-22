/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_04a

export
type Suit = Diamond
          | Heart
          | Spade
          | Club

export
type CardNum = Ace
             | Num ( num : Int )
             | Jack
             | Queen
             | King

export
type Card = Card(suit : Suit, num : CardNum)

export
type Pair('a, 'b) = Pair(pa : 'a, pb : 'b)

export
func card_str(c : Card) -> String {
    Card(var s, var n) = c

    var num_str = match (n) {
        Ace         -> "Ace"
        Num(var no) -> int_to_string(no)
        Jack        -> "Jack"
        Queen       -> "Queen"
        King        -> "King"
    }

    var suit_str = match (s) {
        Diamond -> "Diamonds"
        Heart   -> "Hearts" 
        Spade   -> "Spades"
        Club    -> "Clubs"
    }

    return num_str ++ " of " ++ suit_str
}

