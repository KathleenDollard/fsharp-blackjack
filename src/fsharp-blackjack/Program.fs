open CardDeck
open Blackjack
open Basics

module Entry =

    let reportOutcome dealerScore winners losers =
        if List.length winners = 0 then
            printfn $"Dealer wins with a score of {dealerScore}"
        else
            printfn $"Dealer score is {dealerScore}"

            for player in winners do
                printfn $"Player {player.Name} wins with a score of {score player.Hand}"

        for player in losers do
            printfn $"Player {player.Name} loses with a score of {score player.Hand}"



    printfn "Hello world"

// set players
// play game
