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

type Deck = Card array
type Hand = Card array

let fullDeck () =
    let suits = [| Spades; Clubs; Diamonds; Hearts |]
    let faces = [| Jack; Queen; King |]

    [| for suit in suits do
           Ace(suit)

           for i = 2 to 10 do
               ValueCard(suit, i)

           for face in faces do
               FaceCard(suit, face) |]

let draw numberOfCards (deck: Deck) =
    if Array.length deck < numberOfCards then
        failwith "Not enough cards in deck"

    deck |> Array.splitAt numberOfCards

let drawOne = draw 1

let shuffle inDeck : Deck =
    inDeck |> Array.sortBy (fun _ -> Random.Shared.Next())

let deal numberOfCards numberOfPlayers deck =

    // check preconditions first before doing any more work
    let countCardsToDeal = numberOfCards * numberOfPlayers

    // Split deck to cards to deal and remaining cards
    if (Array.length deck) < countCardsToDeal then
        failwith "Not enough cards in deck"

    // easily create the arrays for the individual player hands
    let playerHands =
        Array.init numberOfPlayers (fun _playerNumber -> Array.zeroCreate numberOfCards)

    // logically separate how to deal a round of cards to each player
    let dealRoundToPlayers roundNumber (roundOfCards: Card array) =
        roundOfCards
        |> Array.iteri (fun playerIndex card ->
            playerHands[playerIndex][roundNumber] <- card)

    let cardsToDeal, remainingCards = deck |> Array.splitAt countCardsToDeal

    cardsToDeal
    |> Array.chunkBySize numberOfPlayers
    |> Array.iteri dealRoundToPlayers

    playerHands, remainingCards

let dealOld numberOfCards numberOfPlayers deck =
    let countCardsToDeal = numberOfCards * numberOfPlayers

    // Split deck to cards to deal and remaining cards
    if (List.length deck) < countCardsToDeal then
        failwith "Not enough cards in deck"

    let cardsToDeal, remainingCards = deck |> List.splitAt countCardsToDeal

    // Add an index, group, remove indexes
    let indexed = cardsToDeal |> List.indexed // creates a tupled list with index/member
    let groups = indexed |> List.groupBy (fun (pos, _) -> pos % numberOfPlayers)
    let hands = groups |> List.map (fun (_, t) -> t |> List.map (fun (_, card) -> card))

    hands, remainingCards
