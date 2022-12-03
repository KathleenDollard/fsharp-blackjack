
module Blackjack
    open CardDeck

    type Player =
        { Name: string
          Hand: Card list
          Strategy: Strategy
          LastAction: Action
          IsDealer : bool }

    and Action =
        | Start
        | Hit
        | Stand

    and Strategy = Hand -> Action

    type Score =
        | Bust
        | ValueScore of int
        | Blackjack

    let handValue hand = 
        let isAnAce card =
            match card with 
            | Ace _ -> true
            | _ -> false

        let cardScore card =
            match card with
            | FaceCard (_, _) -> 10
            | ValueCard (_, value) -> value
            | Ace (_) -> 11 // this arm is not used

        let aceScores aceCount = // there are only two options as only one ace can be 11
            let low = aceCount
            let high = 
                if aceCount = 0 then 0
                else 10 + aceCount // this is 11 + (aceCount - 1) reduced
            low, high
         
        let aces, nonAces = hand |> List.partition isAnAce

        let handScoreNoAce = nonAces |> List.sumBy cardScore
        let aceScoreLow, aceScoreHigh = aceScores (aces |> List.length)

        if (handScoreNoAce + aceScoreHigh) <= 21 then 
            handScoreNoAce + aceScoreHigh
        else 
            handScoreNoAce + aceScoreLow

    let score (hand) = 
        let score = handValue hand
        let cardCount = hand |> List.length

        if score = 21 && cardCount = 2 then Blackjack
        else 
            if score > 21 then Bust
            else ValueScore score
      
    let hit deck player =
        let cards, deck  = draw 1 deck
        let player = { player with Hand = player.Hand |> List.append cards }
        deck, player

    let aggressivePlayer: Strategy = fun _ -> Hit

    let defensivePlayer: Strategy = fun _ -> Stand

    let defaultPlayer = { Name = "?"
                          Hand = []
                          LastAction = Start
                          IsDealer = false
                          Strategy = defensivePlayer }

    let standAt target hand =
        match score hand with
        | Blackjack
        | Bust -> Stand
        | ValueScore score ->
            if score >= target then
                Stand
            else
                Hit

    // The dealer strategy is set by table rules
    let dealerStrategy: Strategy =
        standAt 17

    let rec playerTurn deck player  =
        let action = player.Strategy player.Hand

        match action with 
        | Hit -> 
            let newDeck, newPlayer = hit deck player
            playerTurn newDeck newPlayer
        | Start
        | Stand -> deck, player

    // leaving for now to compare to recursion
    let playerTurn2 deck player  =
        let mutable currentAction = Start
        let mutable player = player
        let mutable deck = deck
        while currentAction <> Stand do
            // @Chet: I find the following line a bit odd. Seems a smell to pass a 
            // player hand to a property of the player
            currentAction <- player.Strategy player.Hand
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
        let newPlayers = [
            for player in players do
                let newDeck, player = playerTurn deck player
                deck <- newDeck
                player
        ]
        deck, newPlayers

    let defaultDealer =
        { Name = "Dealer"
          Hand = []
          LastAction = Start
          IsDealer = true
          Strategy = dealerStrategy }

    let gameSetup players =
        let playerCount = players |> List.length
        let players = players |> List.insertAt playerCount defaultDealer
            
        let deck = fullDeck () |> shuffle
        // Note that playerCount is no longer valid here
        let hands, deck = deal 2 (List.length players) deck
        let playersWithHands = 
            players 
            |> List.zip hands
            |> List.map (fun(hand, player) -> { player with Hand = hand})
        deck, playersWithHands

    // Note table specific rules that dealer always wins any tie, except a player 
    // blackjack against a dealer multi-card 21
    let isWinner dealerScore (_, handScore) =  // player and parameter order support currying/partial application
        match dealerScore, handScore with
        | _, Bust
        | Blackjack, _ -> false
        | _, Blackjack     // Blackjack tie previousy handled
        | Bust, _ -> true  // Bust tie previousy handled
        | ValueScore dealerScore, ValueScore handScore 
            when handScore > dealerScore -> true
        | _ -> false

    let gameResults players =
        let dealer = players |> List.where (fun(player) -> player.IsDealer)
                             |> List.head
        let dealerScore = score dealer.Hand
        let nonDealersWithScores = 
            players |> List.where (fun player -> not player.IsDealer)
                    |> List.map (fun player -> player, score player.Hand)

        let winAgainstDealer = isWinner dealerScore
        let allWinners, losers = 
            nonDealersWithScores |> List.partition winAgainstDealer
        let blackjackWinners, winners = 
            allWinners |> List.partition (fun (_, score) -> score = Blackjack)
        dealerScore, blackjackWinners, winners, losers


    let playGame players =
        let deck, players = gameSetup players
        let _, players = play deck players
        gameResults players
