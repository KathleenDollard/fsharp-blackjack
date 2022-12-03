
module Blackjack
    open CardDeck

    type Player =
        { Name: string
          Hand: Card list
          Strategy: Strategy
          LastAction: Action
          IsDealer : bool }

    and Action =
        | None
        | Hit
        | Stand

    and Strategy = Player -> Action

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

    let aggressivePlayer: Strategy = fun player -> Hit

    let defensivePlayer: Strategy = fun player -> Stand

    let defaultPlayer = { Name = "?"
                          Hand = []
                          LastAction = None
                          IsDealer = true
                          Strategy = defensivePlayer }

    let standAt (target: int) (player: Player) =
        match score player.Hand with
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

    let playerTurn deck player  =
        // @Chet: I find the following line a bit odd. Seems a smell
        let mutable currentAction = None
        let mutable player = player
        let mutable deck = deck
        while currentAction <> Stand do
            currentAction <- player.Strategy player
            let newDeck, newPlayer =
                match currentAction with 
                | Hit -> hit deck player
                | None
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

    let gameSetup players =
        let deck = fullDeck () |> shuffle
        let hands, deck = deal 2 (List.length players) deck
        let zipped = players |> List.zip hands
        let players = zipped |> List.map (fun(hand, player) -> { player with Hand = hand})
        deck, players

    let reportOutcome dealerScore winners losers =
        if List.length winners = 0 then 
             printfn $"Dealer wins with a score of {dealerScore}"
        else
             printfn $"Dealer score is {dealerScore}"
             for player in winners do
                 printfn $"Player {player.Name} wins with a score of {score player.Hand}"

        for player in losers do
             printfn $"Player {player.Name} loses with a score of {score player.Hand}"

    let dealer =
        { Name = "Dealer"
          Hand = []
          LastAction = None
          IsDealer = true
          Strategy = dealerStrategy }

    let playGame players =
        let players = players |> List.insertAt 0 dealer
        let deck, players = gameSetup players
        let _, players = play deck players

        let dealerScore = score dealer.Hand
        let nonDealers = players |> List.filter (fun player -> not player.IsDealer)
        let winners, losers = nonDealers |> List.partition (fun player -> score player.Hand > dealerScore)
        
        reportOutcome dealerScore winners losers
 
        None
