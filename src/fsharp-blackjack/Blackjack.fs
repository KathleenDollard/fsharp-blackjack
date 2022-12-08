module Blackjack

open CardDeck
open Domain
open Strategy

type Player =
    { Name: string
      Hand: Card array
      Strategy: Strategy
      IsDealer: bool }

    static member Default =
        { Name = "?"
          Hand = [||]
          IsDealer = false
          Strategy = defensivePlayer }

    static member Dealer =
        { Name = "Dealer"
          Hand = [||]
          IsDealer = true
          Strategy = dealerStrategy }

let hit deck player =
    let cards, deck = draw 1 deck
    let player = { player with Hand = player.Hand |> Array.append cards }
    deck, player

let rec playerTurn deck player =
    let action = player.Strategy(score player.Hand)

    match action with
    | Hit ->
        let newDeck, newPlayer = hit deck player
        playerTurn newDeck newPlayer
    | Start
    | Stand -> deck, player

// leaving for now to compare to recursion
let playerTurn2 deck player =
    let mutable currentAction = Start
    let mutable player = player
    let mutable deck = deck

    while currentAction <> Stand do
        // @Chet: I find the following line a bit odd. Seems a smell to pass a
        // player hand to a property of the player
        currentAction <- player.Strategy(score player.Hand)

        let newDeck, newPlayer =
            match currentAction with
            | Hit -> hit deck player
            | Start
            | Stand -> (deck, player)

        player <- newPlayer
        deck <- newDeck

    deck, player

let play deck (players: Player array) =
    let mutable deck = deck

    let newPlayers =
        [| for player in players do
               let newDeck, player = playerTurn deck player
               deck <- newDeck
               player |]

    deck, newPlayers

let gameSetup players =
    let playerCount = players |> Array.length
    let players = players |> Array.insertAt playerCount Player.Dealer

    let deck: Card array = fullDeck () |> shuffle
    // Note that playerCount is no longer valid here
    let (hands: Hand array, deck) = deal 2 (Array.length players) deck

    let playersWithHands =
        players
        |> Array.zip hands
        |> Array.map (fun (hand, player) -> { player with Hand = hand })

    deck, playersWithHands

let gameResults players =
    let dealer =
        players
        |> Array.find (fun (player) -> player.IsDealer)

    let dealerScore = score dealer.Hand

    let nonDealersWithScores =
        players
        |> Array.where (fun player -> not player.IsDealer)
        |> Array.map (fun player -> player, score player.Hand)

    let allWinners, losers = 
        nonDealersWithScores 
        |> Array.partition (isWinner dealerScore)

    let blackjackWinners, winners =
        allWinners 
        |> Array.partition (fun (_, score) -> score = Blackjack)

    dealerScore, blackjackWinners, winners, losers

let playGame players =
    let deck, players = gameSetup players
    let _, players = play deck players
    gameResults players
