module GameTests

open Xunit
open CardDeck
open Blackjack
open System.Collections.Generic

[<Fact>]
let ``Player turn with unshuffled deck takes 4 cards to total 20`` () =
    let expectedHand =
        [ Ace(Spades)
          ValueCard(Spades, 2)
          ValueCard(Spades, 3)
          ValueCard(Spades, 4) ]

    let expectedScore = ValueScore 20
    let deck = fullDeck ()
    let hand, deck = draw 2 deck
    Assert.Equal<IEnumerable<Card>>((expectedHand |> List.take 2), hand) // prelim sanity check

    let player =
        { defaultPlayer with
            Hand = hand
            Strategy = standAt 17 }

    let newDeck, newPlayer = playerTurn deck player

    // We do not care what order the hand appears in
    Assert.Equal<IEnumerable<Card>>((expectedHand |> List.sort), (newPlayer.Hand |> List.sort))
    Assert.Equal(expectedScore, score newPlayer.Hand)
    Assert.Equal(ValueCard(Spades, 5), newDeck[0])

[<Fact>]
let ``Player turn with a shuffled deck has no crashes or hangs`` () =

    let mutable deck = fullDeck () |> shuffle
    let basePlayer = { defaultPlayer with Strategy = standAt 17 }

    while (deck |> List.length) > 10 do // this should avoid end of deck errors
        let hands, deckAfterDeal = deal 2 1 deck
        let player = { basePlayer with Hand = hands[0] }
        let deckAfterTurn, _ = playerTurn deckAfterDeal player
        deck <- deckAfterTurn

    // We just want this test to complete without error
    Assert.True((deck |> List.length) <= 10)

[<Fact>]
let ``Game setup includes dealer ??with stand at 17 strategy??`` () =
    let expectedPlayersLength = 4
    let explicitPlayers = [ defaultPlayer; defaultPlayer; defaultPlayer ]

    let _, players = gameSetup explicitPlayers

    Assert.Equal((expectedPlayersLength), (players |> List.length))
    let dealer = players[3]
    Assert.Equal("Dealer", dealer.Name)
// @Chet - The below fails (for obvious reasons), so I do not
// think I can test the assignment of the dealer strategy, except indirectly.
// Does this seem an issue.
//Assert.Equal(standAt(17), dealer.Strategy)


[<Fact>]
let ``Game setup results in players with hands that have been shuffled`` () =
    let expectedDeckCount = 44
    let players = [ defaultPlayer; defaultPlayer; defaultPlayer ]

    let deck, players = gameSetup players

    Assert.Equal(expectedDeckCount, (deck |> List.length))

    if players[0].Hand[0] = Ace(Spades) then // to handle one in 52 chance it is
        Assert.NotEqual(ValueCard(Spades, 2), players[0].Hand[1])
    else

        Assert.NotEqual(Ace(Spades), players[0].Hand[0]) // For clarity, always true

let player1 =
    { defaultPlayer with
        Name = "One"
        Hand = [ ValueCard(Spades, 8); ValueCard(Hearts, 7); ValueCard(Clubs, 6) ] }

let player2 =
    { defaultPlayer with
        Name = "Two"
        Hand = [ ValueCard(Spades, 8); ValueCard(Hearts, 8); ValueCard(Clubs, 6) ] }

let player3 =
    { defaultPlayer with
        Name = "Three"
        Hand = [ Ace(Clubs); FaceCard(Clubs, Queen) ] }

let player4 =
    { defaultPlayer with
        Name = "Four"
        Hand = [ ValueCard(Spades, 8); ValueCard(Hearts, 6); ValueCard(Clubs, 6) ] }

let startingPlayers = [ player1; player2; player3; player4 ]

let playerScoreName (player, score) = player.Name

[<Fact>]
let ``Dealer blackjack wins against all`` () =
    let dealer = { defaultDealer with Hand = [ Ace(Spades); FaceCard(Hearts, Queen) ] }

    let players = startingPlayers |> List.append [ dealer ]

    let dealerScore, blackjackWinners, winners, losers = gameResults players

    Assert.Equal(Blackjack, dealerScore)
    Assert.Empty(blackjackWinners)
    Assert.Empty(winners)
    Assert.Equal((startingPlayers |> List.length), (losers |> List.length))


[<Fact>]
let ``Only player blackjack wins against dealer 21`` () =
    let dealer =
        { defaultDealer with Hand = [ ValueCard(Spades, 6); ValueCard(Spades, 5); FaceCard(Hearts, Queen) ] }

    let players = startingPlayers |> List.append [ dealer ]

    let dealerScore, blackjackWinners, winners, losers = gameResults players

    Assert.Equal(ValueScore 21, dealerScore)
    Assert.Equal("Three", playerScoreName (blackjackWinners[0]))
    Assert.Empty(winners)
    Assert.Equal((startingPlayers |> List.length) - 1, (losers |> List.length))

[<Fact>]
let ``All but bust win against dealer 17`` () =
    let dealer =
        { defaultDealer with Hand = [ ValueCard(Spades, 3); ValueCard(Spades, 4); FaceCard(Hearts, Queen) ] }

    let players = startingPlayers |> List.append [ dealer ]

    let dealerScore, blackjackWinners, winners, losers = gameResults players

    Assert.Equal(ValueScore 17, dealerScore)
    Assert.Equal("Three", playerScoreName blackjackWinners[0])
    Assert.Equal(2, (winners |> List.length))
    Assert.Equal("One", playerScoreName winners[0])
    Assert.Equal("Four", playerScoreName winners[1])
    Assert.Equal(1, (losers |> List.length))
    Assert.Equal("Two", playerScoreName (losers[0]))
