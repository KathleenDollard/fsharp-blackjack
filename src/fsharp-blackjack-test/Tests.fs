module Tests

open Xunit
open CardDeck
open Blackjack

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
