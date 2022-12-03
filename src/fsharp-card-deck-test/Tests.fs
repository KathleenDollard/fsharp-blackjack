module Tests

open System
open Xunit
open CardDeck

[<Fact>]
let ``Full deck is 52 unique cards`` () =
    let deck = fullDeck()
    let length = deck |> List.length 
    Assert.Equal( 52, length)
    Assert.Distinct(deck)


[<Fact>]
let ``Shuffle changes order of deck`` () =
    let deck = fullDeck()
    let firstCard = deck |> List.head
    let secondCard = deck |> List.skip 1 |> List.head

    let shuffledDeck = deck |> shuffle

    // We look at two cards so the statistical failure is 52*52
    let shuffledFirstCard = shuffledDeck |> List.head
    let shuffledSecondCard = shuffledDeck |> List.skip 1 |> List.head
    if firstCard = shuffledFirstCard then
        Assert.NotEqual (secondCard, shuffledSecondCard)
    else
        Assert.NotEqual (firstCard, shuffledFirstCard)

