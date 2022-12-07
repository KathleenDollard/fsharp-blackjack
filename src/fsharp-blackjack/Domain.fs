module Domain

open CardDeck

type Score =
    | Bust
    | ValueScore of int
    | Blackjack

type Action =
    | Start
    | Hit
    | Stand

let handValue hand =
    let isAnAce card =
        match card with
        | Ace _ -> true
        | _ -> false

    let cardScore card =
        match card with
        | FaceCard(_, _) -> 10
        | ValueCard(_, value) -> value
        | Ace(_) -> 11 // this arm is not used

    let aceScores aceCount = // there are only two options as only one ace can be 11
        let low = aceCount
        let high = if aceCount = 0 then 0 else 10 + aceCount // this is 11 + (aceCount - 1) reduced
        low, high

    let aces, nonAces = hand |> Array.partition isAnAce

    let handScoreNoAce = nonAces |> Array.sumBy cardScore
    let aceScoreLow, aceScoreHigh = aceScores (aces |> Array.length)

    if (handScoreNoAce + aceScoreHigh) <= 21 then
        handScoreNoAce + aceScoreHigh
    else
        handScoreNoAce + aceScoreLow

let score (hand) =
    let value = handValue hand
    let cardCount = hand |> Array.length

    if value = 21 && cardCount = 2 then Blackjack
    else if value > 21 then Bust
    else ValueScore value

// Note table specific rules that dealer always wins any tie, except a player
// blackjack against a dealer multi-card 21
let isWinner dealerScore (_, handScore) = // player and parameter order support currying/partial application
    match dealerScore, handScore with
    | _, Bust
    | Blackjack, _ -> false
    | _, Blackjack // Blackjack tie previousy handled
    | Bust, _ -> true // Bust tie previousy handled
    | ValueScore dealerScore, ValueScore handScore when handScore > dealerScore -> true
    | _ -> false
