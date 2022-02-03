#r "../../../FSharp.Core.Extensions/bin/Release/netstandard2.0/FSharp.Core.Extensions.dll"
#r "../bin/Release/netstandard2.0/SorryCore.dll"

open Sorry.Core.CardDeck

//let newDeck = newDeck()
//let nextCard, remaining = newDeck |> drawCard

//[] |> drawCard1 shuffleDeckWithNewRandom


//type TurnAction =
//    | Move of int
//    | Sorry
//    | Swap
//    // @TODO add special action (ie red next move home, slide, etc)

//let getCardAction (card:Card) =

//    let cardActions =
//        [(One, [Move 1]);
//         (Two, [Move 2]);
//         (Three, [Move 3]);
//         (Four, [Move 4])
//         (Five, [Move 1])
//         (Seven, [Move 1])
//         (Eight, [Move 1])
//         (Ten, [Move 1])
//         (Eleven, [Move 1])
//         (Twelve, [Move 1])
//        ] |> Map.ofList

//    cardActions.[card]

//type Color =
//    | Green
//    | Red
//    | Blue
//    | Yellow

//type Position = 
//    | Outer of Color * int // 0 -14
//    | Start of Color
//    | HomeRow of Color * int // 0 - 5
//    | Home of Color

//type TokenID =
//    | One
//    | Two
//    | Three

//type Token = Color * TokenID

//type BoardState = {
//    Deck : Deck
//    CurrentPlayer : Color
//    TokenPositions : Map<Color, Position * Position * Position>
//}

//type DrawingState = {
//    BoardState : BoardState
//}

//type ChooseActionState = {
//    DrawnCard : Card
//    BoardState : BoardState
//}

//type GameOverState = {
//    BoardState : BoardState
//    Winner : Color
//}

//let drawCard (state:DrawingState) =
//    let topCard,remaining = state.BoardState.Deck |> drawCard1
//    {DrawnCard = topCard; BoardState = {state.BoardState with Deck=remaining}}

