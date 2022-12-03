
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

    // TODO: Consider DU for bust, blackjack, and score
    let score ({ Hand = cards }) = 

        let cardScore aceOne card =
            match card with
            | FaceCard (_, _) -> 10
            | ValueCard (_, value) -> value
            | Ace (_) -> if aceOne then 1 else 11
        
        let handScore cards =
            let cardScoreAceLow = cardScore true
            let cardScoreAceHigh = cardScore false
            let lowScore = cards |> List.sumBy cardScoreAceLow
            let highScore =   cards |> List.sumBy cardScoreAceHigh
            if highScore > 21 then lowScore else highScore

        let score = handScore cards

        if score = 21 then Blackjack
        else 
            if score > 21 then Bust
            else ValueScore score
      
    let hit deck player =
        let cards, deck  = draw 1 deck
        let player = { player with Hand = player.Hand |> List.append cards }
        deck, player

    let aggressivePlayer: Strategy = fun player -> Hit

    let defensivePlayer: Strategy = fun player -> Stand

    let standAt (target: int) (player: Player) =
        match score player with
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

    let gameOver players =
        players |> List.exists (fun player -> player.LastAction <> Stand )

    let playerTurn deck player  =
        // @Chet: I find the following line a bit odd. Seems a smell
        let action = player.Strategy player
        match action with 
        | Hit -> hit deck player
        | None
        | Stand -> (deck, player)

    let rec turns deck players =
        if gameOver players then 
            deck, players
        else
            let mutable deck = deck
            let newPlayers = [
                for player in players do
                    let newDeck, player = playerTurn deck player
                    deck <- newDeck
                    player
            ]
            turns deck newPlayers

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
                 printfn $"Player {player.Name} wins with a score of {score player}"

        for player in losers do
             printfn $"Player {player.Name} loses with a score of {score player}"


    let dealer =
        { Name = "Dealer"
          Hand = []
          LastAction = None
          IsDealer = true
          Strategy = dealerStrategy }

    let playGame players =
        let players = players |> List.insertAt 0 dealer
        let deck, players = gameSetup players
        let _, players = turns deck players

        let dealerScore = score dealer
        let nonDealers = players |> List.filter (fun player -> not player.IsDealer)
        let winners, losers = nonDealers |> List.partition (fun player -> score player > dealerScore)
        
        reportOutcome dealerScore winners losers
 
        None
