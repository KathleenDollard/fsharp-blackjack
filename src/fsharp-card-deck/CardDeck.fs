module CardDeck

open System

type Suit =
    | Spades
    | Clubs
    | Diamonds
    | Hearts

type Face =
    | Jack
    | Queen
    | King

type Card =
    | FaceCard of Suit * Face
    | ValueCard of Suit * int
    | Ace of Suit

    override x.ToString() =
        match x with
        | FaceCard(suit, face) -> $"{face} of ${suit}"
        | ValueCard(suit, value) -> $"{value} of ${suit}"
        | Ace suit -> $"Ace of ${suit}"

type Deck = Card list
type Hand = Card list

let fullDeck () =
    let suits = [ Spades; Clubs; Diamonds; Hearts ]
    let faces = [ Jack; Queen; King ]

    [ for suit in suits do
          Ace(suit)

          for i = 2 to 10 do
              ValueCard(suit, i)

          for face in faces do
              FaceCard(suit, face) ]

let draw numberOfCards (deck: Deck) =
    if List.length deck < numberOfCards then
        failwith "Not enough cards in deck"

    deck |> List.splitAt numberOfCards

let drawOne = draw 1

let shuffle inDeck : Deck =
    inDeck
    |> List.sortBy (fun c -> Random.Shared.Next())

// Dealing implies each player gets one card
let deal numberOfCards numberOfPlayers deck =
    let countCardsToDeal = numberOfCards * numberOfPlayers

    if (List.length deck) < countCardsToDeal then
        failwith "Not enough cards in deck"
    let cardsToDeal, remainingCards = deck |> List.splitAt countCardsToDeal

    let indexed = cardsToDeal |> List.indexed // creates a tupled list with index/member
    let groups = indexed |> List.groupBy (fun (pos, _) -> pos % numberOfPlayers)
    let hands = 
        groups 
        |> List.map (fun (_, t) -> 
           t |> List.map (fun (_, card) -> card))

    hands, remainingCards
