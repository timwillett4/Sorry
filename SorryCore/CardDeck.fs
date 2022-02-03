module Sorry.Core.CardDeck

open System

let shuffleDeck (randomGenerator:System.Random) =

    let masterDeck =   [for _ in 1..5 -> Card.One]
                     @ [for _ in 1..4 -> Card.Two]
                     @ [for _ in 1..4 -> Card.Three]
                     @ [for _ in 1..4 -> Card.Four]
                     @ [for _ in 1..4 -> Card.Five]
                     @ [for _ in 1..4 -> Card.Seven]
                     @ [for _ in 1..4 -> Card.Eight]
                     @ [for _ in 1..4 -> Card.Ten]
                     @ [for _ in 1..4 -> Card.Eleven]
                     @ [for _ in 1..4 -> Card.Twelve]
                     @ [for _ in 1..4 -> Card.Sorry]

    let rec buildIndexes currentIndexes = 
        let next = randomGenerator.Next (masterDeck.Length)
        let currentIndexes = next :: currentIndexes |> List.distinct
        match currentIndexes with
        | finished when finished.Length = masterDeck.Length -> 
            finished 
        | notFinished -> 
            notFinished |> buildIndexes

    [] |> buildIndexes |> List.map (fun i -> masterDeck.[i])

let shuffleDeckWithNewRandom() = Random() |> shuffleDeck

let newDeck() = shuffleDeckWithNewRandom()

let drawCard (deck:Deck) =
    if deck.Length > 0
    then Ok (deck.Head,deck.Tail)
    else Error "Deck is empty no cards to draw. Please reshuffle deck" // @TODO return a enum/du rather than string ?