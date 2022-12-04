module Tests

open System
open Xunit
open CardDeck
open System.Collections.Generic

[<Fact>]
let ``Full deck is 52 unique cards`` () =
    let deck = fullDeck ()
    let length = deck |> List.length
    Assert.Equal(52, length)
    Assert.Distinct(deck)

[<Fact>]
let ``Shuffle changes order of deck`` () =
    let deck = fullDeck ()
    let firstCard = deck |> List.head
    let secondCard = deck |> List.skip 1 |> List.head

    let shuffledDeck = deck |> shuffle

    // We look at two cards so the statistical failure is 52*52
    let shuffledFirstCard = shuffledDeck |> List.head
    let shuffledSecondCard = shuffledDeck |> List.skip 1 |> List.head

    if firstCard = shuffledFirstCard then
        Assert.NotEqual(secondCard, shuffledSecondCard)
    else
        Assert.NotEqual(firstCard, shuffledFirstCard)

[<Fact>]
let ``Draw 1 card retrieves one card and leaves 51`` () =
    let deck = fullDeck ()
    let expected = deck |> List.take 1

    let cards, newDeck = draw 1 deck

    let length = newDeck |> List.length
    Assert.Equal<IEnumerable<Card>>(expected, cards)
    Assert.Equal(51, length)

[<Fact>]
let ``Draw 3 cards retrieves three card`` () =
    let deck = fullDeck ()
    let expected = deck |> List.take 3

    let cards, newDeck = draw 3 deck

    let length = newDeck |> List.length
    Assert.Equal<IEnumerable<Card>>(expected, cards)
    Assert.Equal(49, length)

[<Fact>]
let ``Deal 2 cards to 3 hands`` () =
    let deck = fullDeck ()
    let numberOfHands = 3
    let numberOfCards = 2

    let hands, newDeck = deal numberOfCards numberOfHands deck

    let length = newDeck |> List.length
    Assert.Equal(46, length)

    for i in [ 0 .. numberOfCards - 1 ] do
        for j in [ 0 .. numberOfHands - 1 ] do
            let pos = j + (numberOfHands * i)
            Assert.Equal(deck[pos], hands[j][i])
