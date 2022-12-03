module Tests

open Xunit
open CardDeck
open Blackjack
open System.Collections.Generic

[<Fact>]
let ``Hand value is 21 for face card and ace`` () =
    let hand = 
        [ FaceCard(Hearts, Queen)
          Ace(Diamonds) ]

    let value = handValue hand

    Assert.Equal(21, value)

[<Fact>]
let ``Hand value is 22 for face card, 5, 6 and ace`` () =
    let hand = 
        [ FaceCard(Spades, Jack)
          ValueCard(Clubs, 5)
          ValueCard(Spades, 6)
          Ace(Hearts) ]

    let value = handValue hand

    Assert.Equal(22, value)

[<Fact>]
let ``Hand value is 20 for 5, 6 and 9`` () =
    let hand = 
        [ ValueCard(Clubs, 5)
          ValueCard(Spades, 6)
          ValueCard(Hearts, 9) ]

    let value = handValue hand

    Assert.Equal(20, value)

[<Fact>]
let ``Hand value is 21 for face card, 5, 5, and ace`` () =
    let hand = 
        [ FaceCard(Spades, Jack)
          ValueCard(Clubs, 5)
          ValueCard(Spades, 5)
          Ace(Hearts) ]

    let value = handValue hand

    Assert.Equal(21, value)

[<Fact>]
let ``Hand value is 14 for 3 and ace`` () =
    let hand = 
         [ ValueCard(Clubs, 3)
           Ace(Hearts) ]

    let value = handValue hand

    Assert.Equal(14, value)

[<Fact>]
let ``Score is 12 for two aces`` () =
    let hand = 
         [ Ace(Spades)
           Ace(Hearts) ]

    let value = handValue hand

    Assert.Equal(12, value)

[<Fact>]
let ``Score is 14 for four aces`` () =
    let hand = 
         [ Ace(Spades)
           Ace(Hearts)
           Ace(Clubs)
           Ace(Diamonds) ]

    let value = handValue hand

    Assert.Equal(14, value)

[<Fact>]
let ``Score is 21 for 7, four aces, and a face card`` () =
    let hand = 
         [ ValueCard(Hearts, 7)
           ValueCard(Spades, 10)
           Ace(Spades)
           Ace(Hearts)
           Ace(Clubs)
           Ace(Diamonds) ]

    let value = handValue hand

    Assert.Equal(21, value)

[<Fact>]
let ``Score is 22 for 8, four aces, and a face card`` () =
    let hand = 
         [ ValueCard(Hearts, 8)
           ValueCard(Spades, 10)
           Ace(Spades)
           Ace(Hearts)
           Ace(Clubs)
           Ace(Diamonds) ]

    let value = handValue hand

    Assert.Equal(22, value)

[<Fact>]
let ``Blackjack for 2 cards totaling 21`` () =
    let hand = 
         [ ValueCard(Hearts, 10)
           Ace(Spades) ]

    let score = score hand

    Assert.Equal(Blackjack, score)
    
[<Fact>]
let ``Value of 21 for 3 cards totaling 21`` () =
    let hand = 
         [ ValueCard(Hearts, 5)
           ValueCard(Spades, 5)
           Ace(Spades) ]

    let score = score hand

    Assert.Equal(ValueScore 21, score)
   
[<Fact>]
let ``Bust for 3 cards totaling 22`` () =
    let hand = 
         [ ValueCard(Hearts, 5)
           ValueCard(Spades, 7)
           FaceCard(Spades, King) ]

    let score = score hand

    Assert.Equal(Bust, score)
    
[<Fact>]
let ``Value 18 for 2 cards totaling 18`` () =
    let hand = 
         [ ValueCard(Hearts, 8)
           FaceCard(Spades, King) ]

    let score = score hand

    Assert.Equal(ValueScore 18, score)

    
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
  
[<Fact>]
let ``Player turn with unshuffled deck takes 4 cards to total 20`` () =
    let expectedHand = 
         [ Ace(Spades)
           ValueCard(Spades, 2)
           ValueCard(Spades, 3)
           ValueCard(Spades, 4) ]
    let expectedScore = ValueScore 20
    let deck = fullDeck()
    let hand, deck = draw 2 deck
    Assert.Equal<IEnumerable<Card>> ((expectedHand |> List.take 2), hand) // prelim sanity check
    let player = { defaultPlayer with Hand = hand; Strategy = standAt 17 }

    let newDeck, newPlayer = playerTurn deck player

    // We do not care what order the hand appears in
    Assert.Equal<IEnumerable<Card>>((expectedHand |> List.sort), (newPlayer.Hand |> List.sort))
    Assert.Equal(expectedScore, score newPlayer.Hand)
    Assert.Equal(ValueCard(Spades, 5), newDeck[0])


[<Fact>]
let ``Player turn with a shuffled deck has no crashes or hangs`` () =

    let mutable deck = fullDeck() |> shuffle
    let basePlayer = { defaultPlayer with Strategy = standAt 17 }

    while (deck |> List.length) > 10 do // this should avoid end of deck errors
        let hands, deckAfterDeal = deal 2 1 deck
        let player = { basePlayer with Hand = hands[0] }
        let deckAfterTurn, _ = playerTurn deckAfterDeal player
        deck <- deckAfterTurn

    // We just want this test to complete without error
    Assert.True((deck |> List.length) <= 10)  
