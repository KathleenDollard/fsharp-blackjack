module StrategyTests

open Xunit
open CardDeck
open Blackjack
open System.Collections.Generic
    
[<Fact>]
let ``Hit for standAt when hand score is below target`` () =
    let hand = 
         [ ValueCard(Hearts, 6)
           FaceCard(Spades, King) ]

    let action = standAt 17 hand

    Assert.Equal(Hit, action)
    
[<Fact>]
let ``Stand for standAt when hand score is at target`` () =
    let hand = 
         [ ValueCard(Hearts, 6)
           FaceCard(Spades, King) ]

    let action = standAt 16 hand

    Assert.Equal(Stand, action)

[<Fact>]
let ``Stand for standAt when hand score is above target`` () =
    let hand = 
         [ ValueCard(Hearts, 8)
           FaceCard(Spades, King) ]

    let action = standAt 17 hand

    Assert.Equal(Stand, action)

[<Fact>]
let ``Stand for standAt when hand score is blackjack`` () =
    let hand = 
         [ Ace(Hearts)
           FaceCard(Spades, King) ]

    let action = standAt 17 hand

    Assert.Equal(Stand, action)

[<Fact>]
let ``Stand for standAt when hand score is bust`` () =
    let hand = 
         [ ValueCard(Hearts, 6)
           ValueCard(Hearts, 6)
           FaceCard(Spades, King) ]

    let action = standAt 16 hand

    Assert.Equal(Stand, action)
