#r "../../submodules/FSharp.Core.Extensions/bin/Release/netstandard2.0/FSharp.Core.Extensions.dll"
#r "../bin/Release/netstandard2.0/SorryCore.dll"

// @TODO - design api and state machine

open FSharp.Core.Extensions.Result
open FSharp.Core.Extensions.List
open Sorry.Core
open Sorry.Core.BoardPosition
open Sorry.Core.CardDeck
open Sorry.Core.Types

//let beginGame gameOptions -> gameState

type PlayingState = {
    Deck : Deck
    BoardState : BoardState
    ActivePlayer : Color
}

type DrawingState = {
    GameState : PlayingState
}

// drawState -> chooseActionState
//let drawCard drawState =

type ChooseActionState = {
    GameState : PlayingState
    DrawnCard : Card
}

// let getAvailableActions = chooseActionState -> Valid turnAction list
// let chooseAction = chooseActionState -> validTurnAction -> 


type TurnAction =
    | Move of Pawn * Spaces : int
    | Split of Token1 : Pawn * Spaces1 : int * Token2 : Pawn * Spaces2 : int
    | Sorry of HappyToken : Pawn * SadToken : Pawn
    | Swap of Token1 : Pawn * Token2 : Pawn
    | ForfeitTurn

type GameOverState = {
    GameState : PlayingState
    Winner : Color
}

type GameState =
    | DrawingState of DrawingState
    | ChooseActionState of ChooseActionState
    | GameOverState of GameOverState

//let newGame players =
//let deck = newDeck()
//let game = colors |> initializeGameBoard
//let activePlayer = ??? // @TODO look this up
//(DrawingState {Deck:deck;BoardState:game;ActivePlayer:Color.Yellow})
// @TODO move from start - (support it both ways (regular move or only 1 and 2 can move from start)
// @TODO sorry
// @TODO - swap

//let getValidActions (state:ChooseActionState) =
    
//    let cardActions = 
//        [(Card.One, [MoveFromStart;MoveForward 1])
//         (Card.Two, [MoveFromStart;MoveForward 2]) // GoAgain - how to represent ??
//         (Card.Three, [MoveForward 3])
//         (Card.Four, [MoveBackwards 4])
//         (Card.Five, [MoveForward 5])
//         (Card.Seven, [MoveForward 7;Split 7])
//         (Card.Eight, [MoveForward 8])
//         (Card.Ten),  [MoveForward 10, MoveBackwards 1])
//         (Card.Eleven) [MoveForward 11, Swap]) // swap or forfeit if you can't play 11
//         (Card.Twelve) [MoveForward 12]] 
//        |> Map.ofList

//    let tokens =
//        state.GameState.BoardState.[state.GameState.ActivePlayer]

//    match state.DrawnCard with
//    | Card.One ->
//        tokens |> 
//    | _ -> [ForfeitTurn]

// setup
//"sample console output"
//"How Many Players?"

//"Enter Player 1 name:"
//"Select Player 1 color"

//"Enter Player 2 name:"
//"Select Player 2 color"

//let newGame = createNewGame(Red "Levi", Yellow "Tim")
//// game loop

//let rec gameLoop gameState =
//    printGameInfo(gameState)
//    let actions = getAvailableActions(newGame)
//    let newGameState = selectAction(actions.[1])
//    // if not game over
//    //    gameLoop newGameState
//...
//"[Game Info]"
//"BoardState"
//"Levi's turn (Red)"
//"Available Action: [D]raw"

//"Drew an 8"
//"Available Action: [1]Move Token1 1"
//"Available Action: [2]Move Token1 2"
//...

//// over
//"Levi wins"

//"New Game?"
