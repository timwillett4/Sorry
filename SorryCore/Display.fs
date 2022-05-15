module Sorry.Core.Display

open System
open Sorry.Core

type Direction =
    | Up
    | Down
    | Left
    | Right
    
type CellType =
    | Regular
    | BoostStart of Color*Direction
    | BoostLine of Color*Direction
    | BoostEnd of Color*Direction
    | Special of Color
    | Empty

let Board =
    // Row 1
    [[Regular
      BoostStart(Color.Blue, Direction.Right)
      BoostLine(Color.Blue, Direction.Right)
      BoostLine(Color.Blue, Direction.Right)
      BoostEnd(Color.Blue, Direction.Right)
      Regular
      Regular
      Regular
      Regular
      BoostStart(Color.Blue, Direction.Right)
      BoostLine(Color.Blue, Direction.Right)
      BoostLine(Color.Blue, Direction.Right)
      BoostLine(Color.Blue, Direction.Right)
      BoostEnd(Color.Blue, Direction.Right)
      Regular
      Regular
     ]
     
     // Row 2
     [Regular
      Empty
      Special(Color.Blue)
      Empty
      Special(Color.Blue)
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      BoostStart(Color.Yellow, Down)]
      
     // Row 3
     [BoostEnd(Color.Red, Up)
      Empty
      Special(Color.Blue)
      Empty
      Special(Color.Blue)
      Empty
      Empty
      Empty
      Empty
      Special(Color.Yellow)
      Special(Color.Yellow)
      Special(Color.Yellow)
      Special(Color.Yellow)
      Special(Color.Yellow)
      Special(Color.Yellow)
      BoostLine(Color.Yellow, Direction.Down)]
      
     // Row 4
     [BoostEnd(Color.Red, Up)
      Empty
      Special(Color.Blue)
      Empty
      Special(Color.Blue)
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      Empty
      BoostLine(Color.Yellow, Direction.Down)]
    ]
let drawBoard tokenPositions =
    let drawBorder() = printf "____" 
    let drawCell c =
      match c with
      // @TODO - convert to match cell type and  draw cell type
      | Some(c) -> printf "| c " 
      | None -> printf "|   "
     
    let drawRow() =
        [0..15] |> List.iter (fun _ -> drawBorder())
        printfn "|"
        [0..15] |> List.iter (fun _ -> drawCell None)
        printfn "|"
    
    Console.Clear();
    [0..15] |> List.iter (fun _ -> drawRow())
    [0..15] |> List.iter (fun _ -> drawBorder())


let printBoardState (game:GameState) =
    let availableActions = game |> GameState.getAvailableActions
    
    let printTokenPosition tokenPosition =
       let boardState = (tokenPosition |> Map.fold (fun str (pawn:Pawn) position -> str + $"[%A{pawn.Color}] Token %A{pawn.ID} is at board position: %A{position}" + Environment.NewLine) "")
       "Board State:" + Environment.NewLine + boardState + Environment.NewLine

    let printActivePlayer activePlayer =
       $"Active Player: %A{activePlayer}" + Environment.NewLine
       
    let printDrawnCard card =
        match card with
        | Some(card) ->
            $"Drawn Card: %A{card}" + Environment.NewLine
        | None -> ""
        
    let printAvailableActions (availableActions:Action list) =
        let actions = availableActions |> List.mapi(fun i action -> (i, action))
        let actions = List.fold (fun str (i:int, action:Action) -> str + $"Action[%i{i}]: %A{action}" + Environment.NewLine) "" actions 
        "Available Actions: " + actions + Environment.NewLine

    let tokenPositions = game |> GameState.getTokenPositions
    let activePlayer = game |> GameState.getActivePlayer
    let drawnCard = game |> GameState.getDrawnCard 
    
    let activePlayer =
        match activePlayer with
        | Some(activePlayer) -> activePlayer |> printActivePlayer
        | None -> ""
        
    let tokenPositions = tokenPositions |> printTokenPosition
    
    let drawnCard = drawnCard |> printDrawnCard
    let actions = availableActions |> printAvailableActions
    
    activePlayer + tokenPositions + drawnCard + actions