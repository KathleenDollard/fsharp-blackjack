module Strategy

open Domain

type Strategy = Score -> Action

let aggressivePlayer: Strategy = fun _ -> Hit

let defensivePlayer: Strategy = fun _ -> Stand

let standAt target score =
    match score with
    | Blackjack
    | Bust -> Stand
    | ValueScore score -> if score >= target then Stand else Hit

// The dealer strategy is set by table rules
let dealerStrategy: Strategy = standAt 17
