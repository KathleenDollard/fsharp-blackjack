module Blackjack

open CardDeck
open Basics
open Strategy

type Player =
    { Name: string
      Hand: Card list
      Strategy: Strategy
      IsDealer: bool }
    static member Default =
        { Name = "?"
          Hand = []
          IsDealer = false
          Strategy = defensivePlayer }
    static member Dealer =
        { Name = "Dealer"
          Hand = []
          IsDealer = true
          Strategy = dealerStrategy }

let hit deck player =
    let cards, deck = draw 1 deck
    let player = { player with Hand = player.Hand |> List.append cards }
    deck, player

let rec playerTurn deck player =
    let action = player.Strategy (score player.Hand)

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
        currentAction <- player.Strategy (score player.Hand)

        let newDeck, newPlayer =
            match currentAction with
            | Hit -> hit deck player
            | Start
            | Stand -> (deck, player)

        player <- newPlayer
        deck <- newDeck

    deck, player

let play deck players =
    let mutable deck = deck

    let newPlayers =
        [ for player in players do
              let newDeck, player = playerTurn deck player
              deck <- newDeck
              player ]

    deck, newPlayers

let gameSetup players =
    let playerCount = players |> List.length
    let players = players |> List.insertAt playerCount Player.Dealer

    let deck = fullDeck () |> shuffle
    // Note that playerCount is no longer valid here
    let hands, deck = deal 2 (List.length players) deck

    let playersWithHands =
        players
        |> List.zip hands
        |> List.map (fun (hand, player) -> { player with Hand = hand })

    deck, playersWithHands

let gameResults players =
    let dealer = players |> List.where (fun (player) -> player.IsDealer) |> List.head
    let dealerScore = score dealer.Hand

    let nonDealersWithScores =
        players
        |> List.where (fun player -> not player.IsDealer)
        |> List.map (fun player -> player, score player.Hand)

    let winAgainstDealer = isWinner dealerScore
    let allWinners, losers = nonDealersWithScores |> List.partition winAgainstDealer

    let blackjackWinners, winners =
        allWinners |> List.partition (fun (_, score) -> score = Blackjack)

    dealerScore, blackjackWinners, winners, losers

let playGame players =
    let deck, players = gameSetup players
    let _, players = play deck players
    gameResults players
