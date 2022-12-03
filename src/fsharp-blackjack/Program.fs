open CardDeck

module Blackjack =

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

    let score ({ Hand = cards }) = 

        let cardScore aceOne card =
            match card with
            | FaceCard (_, _) -> 10
            | ValueCard (_, value) -> value
            | Ace (_) -> if aceOne then 1 else 11
            
        let cardScoreAceLow = cardScore true
        let cardScoreAceHigh = cardScore false
        let lowScore = cards |> List.sumBy cardScoreAceLow
        let highScore =   cards |> List.sumBy cardScoreAceHigh
        let score = if highScore > 21 then lowScore else highScore
        if score > 21 then 0 else score
      
    let hit deck player =
        let cards, deck  = draw 1 deck
        let player = { player with Hand = player.Hand |> List.append cards }
        deck, player

    type TurnResult =
        | PlayerBust of score: int
        | PlayerWin of score: int
        | DealerWin of score: int
        | Continue of Deck * player: Player * dealer: Player


    let aggressivePlayer: Strategy = fun player -> Hit

    let defensivePlayer: Strategy = fun player -> Stand

    let standAt (target: int) (player: Player) =
        if score player >= target then
            Stand
        else
            Hit

    // This is part of the game as the dealer strategy is
    // set by table rules
    let dealerStrategy: Strategy =
        standAt 17

    let gameOver players =
        players |> List.exists (fun player -> player.LastAction = Hit )

    let playerTurn deck player  =
        // I find the following line a bit odd. Seems a smell
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
        let deck, hands = deal 2 deck (List.length players)
        let zipped = players |> List.zip hands
        let players = zipped |> List.map (fun(hand, player) -> { player with Hand = hand})
        deck, players

    let playGame players =
        let dealer =
                { Name = "Dealer"
                  Hand = []
                  LastAction = None
                  IsDealer = true
                  Strategy = dealerStrategy }
        let players = players |> List.insertAt 0 dealer
        let deck, players = gameSetup players
        let deck, players = turns deck players

        let dealerScore = score dealer
        let nonDealers = players |> List.filter (fun player -> not player.IsDealer)
        let winners, losers = nonDealers |> List.partition (fun player -> score player > dealerScore)
        
        if List.length winners = 0 then 
            printfn $"Dealer wins with a score of {dealerScore}"
        else
            printfn $"Dealer score is {dealerScore}"
            for player in winners do
                printfn $"Player {player.Name} wins with a score of {score player}"

        for player in losers do
            printfn $"Player {player.Name} loses with a score of {score player}"

        None
