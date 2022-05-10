module Sorry.Desktop.Main

open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls.Shapes

open Sorry.Core

type State =
    { gameState : Result<GameState, GameState*string> }
    
// TokenPositions
open System
open FSharp.Core.Extensions.Result

let initialState() =
    let levi = {Color=Color.Green; Name = "Levi"}
    let corbin = {Color=Color.Yellow; Name = "Corbin"}
    let micah = {Color=Color.Blue; Name = "Micah"}
    let barrett = {Color=Color.Red; Name = "Barrett"}
    
    let game = result {
        let random = Random()
        let random() = random.Next(Int32.MaxValue)
        
        let game = GameState.newGame
        let! game = game |> GameState.tryAddPlayer "Levi" Color.Green
        let! game = game |> GameState.tryAddPlayer "Corbin" Color.Yellow
        let! game = game |> GameState.tryAddPlayer "Micah" Color.Blue
        let! game = game |> GameState.tryAddPlayer "Barrett" Color.Red
        
        return! game |> GameState.tryStartGame random
    }
    { gameState = game }, Elmish.Cmd.none

type Msg =
| Start
| Stop

let update (msg: Msg) (state: State) : State * Elmish.Cmd<_>=
    state, Elmish.Cmd.none
    //match msg with
    //| Start -> { state with state = true }, Elmish.Cmd.none
    //| Stop -> { state with state = false }, Elmish.Cmd.none

let toScreenCoords borderWidth squareWidth x y =
    let left = borderWidth + squareWidth * x
    let top = borderWidth + squareWidth * y
    (left, top)
    
let view (state: State) (dispatch: Msg -> unit) =
    let borderWidth = 20.0
    let squareWidth = 30.0
    
    let createSquare borderWidth squareWidth heightRatio widthRatio (color:string) (x, y) =
        let left,top = toScreenCoords borderWidth squareWidth x y
        Rectangle.create [
            Rectangle.fill color
            Rectangle.width (squareWidth * heightRatio)
            Rectangle.height (squareWidth * widthRatio)
            Rectangle.left left
            Rectangle.top top
            Rectangle.stroke "Black"
            Rectangle.strokeThickness 1.0
        ] :> Types.IView
        
    let createSquare = createSquare borderWidth squareWidth
    
    let createRow color (xs, ys) =
        let xs = xs |> List.map double
        let ys = ys |> List.map double
        let coords = (xs, ys) ||> List.allPairs
        coords |> List.map (createSquare 1.0 1.0 color)
        
    let createEllipse borderWidth squareWidth widthRatio heightRatio x y (color:string) =
        let left,top = toScreenCoords borderWidth squareWidth x y
        Ellipse.create [
            Ellipse.fill color
            Ellipse.width (squareWidth * widthRatio)
            Ellipse.height (squareWidth * heightRatio)
            Ellipse.left left
            Ellipse.top top
            Ellipse.stroke "Black"
            Ellipse.strokeThickness 2.0
        ] :> IView
        
    let createEllipse = createEllipse borderWidth squareWidth
    
    let createBigCircle = createEllipse 2.0 2.0
    
    let createArrow startPoint stopPoint color =
        let startX,startY = startPoint
        let stopX,stopY = stopPoint
        let boostLine =
            let createVertLine (x, y) = createSquare 0.3 1.0 color (double x + 0.35, double y)
            let createHorLine (x, y) = createSquare 1.0 0.3 color (double x, double y + 0.35)
            let vertStartLine = createSquare 0.3 0.6 color ((double startX + 0.35), (double startY + 0.2))
            let horStartLine = createSquare 0.6 0.3 color ((double startX + 0.2), (double startY + 0.35))
            match (stopX - startX), (stopY - startY) with
            | 0, vertLen when vertLen > 0 ->
                let boostLine =
                    [0..vertLen - 1]
                    |> List.map (fun y -> startX, startY + y)
                    |> List.map (fun (x,y) -> double x, double y + 0.5)
                    |> List.map createVertLine
                boostLine@[horStartLine]
            | 0, vertLen when vertLen < 0 ->
                let boostLine =
                    [0..abs(vertLen) - 1]
                    |> List.map (fun y -> startX, startY - y)
                    |> List.map (fun (x,y) -> double x, double y - 0.5)
                    |> List.map createVertLine
                boostLine@[horStartLine]
            | horLen, 0 when horLen > 0 ->
                let boostLine =
                    [0..horLen - 1]
                    |> List.map (fun x -> startX + x, startY)
                    |> List.map (fun (x,y) -> double x + 0.5, double y)
                    |> List.map createHorLine
                boostLine@[vertStartLine]
            | horLen, 0 when horLen < 0 ->
                let boostLine =
                    [0..abs(horLen) - 1]
                    |> List.map (fun x -> startX - x, startY)
                    |> List.map (fun (x,y) -> double x - 0.5, double y)
                    |> List.map createHorLine
                boostLine@[vertStartLine]
            | _ -> failwith "Invalid line coordinate"
        let endCircle =
            let circleX = (stopPoint |> fst |> double) + 0.2
            let circleY = (stopPoint |> snd |> double) + 0.2
            createEllipse 0.6 0.6 circleX circleY color
        
        boostLine@[endCircle]
    
    let createPawn (color:string) (x, y) =
        let pawnTop = createEllipse 0.4 0.4 (x + 0.3) (y) color
        let pawnNeck = createEllipse 0.3 0.5 (x + 0.35) (y + 0.3) color
        let pawnBase = createEllipse 0.8 0.4 (x + 0.1) (y + 0.6) color
        [ pawnBase; pawnNeck; pawnTop ]
        
    let outerSquares =
        [ ([0..15],[0])
          ([0..15],[15])
          ([0],[1..14])
          ([15],[1..14]) ]
        |> List.collect (createRow "Gray")
        
    let safetySquares = 
        [ ([13],[10..14]) |> createRow "Green"
          ([1..5],[13]) |> createRow "Red"
          ([2],[1..5]) |> createRow "Blue"
          ([10..14],[2]) |> createRow "Yellow" ]
        |> List.concat
        
    let startCircles =
        [ createBigCircle 13 3.5 "Yellow"
          createBigCircle 10.5 13.0 "Green"
          createBigCircle 3.5 1.0 "Blue"
          createBigCircle 1 10.5 "Red" ]
    
    let homeCircles =
        [ createBigCircle 8.0 1.5 "Yellow"
          createBigCircle 12.5 8.0 "Green"
          createBigCircle 1.5 6 "Blue"
          createBigCircle 6.0 12.5 "Red" ]
        
    let boostArrows =
        [ createArrow (14, 15) (11, 15) "Green"
          createArrow (6, 15) (2, 15) "Green"
          createArrow (0, 14) (0, 11) "Red"
          createArrow (0, 7) (0, 3) "Red"
          createArrow (1, 0) (4, 0) "Blue"
          createArrow (10, 0) (14, 0) "Blue"
          createArrow (15, 1) (15, 4) "Yellow"
          createArrow (15, 10) (15, 14) "Yellow" ]
        |> List.concat
        
    let pawns =
        let toColorString color =
            match color with
            | Color.Red -> "Red"
            | Color.Green -> "Green"
            | Color.Blue -> "Blue"
            | Color.Yellow -> "Yellow"
            | _ -> failwith "Invalid enum"
            
        let toBoardPos boardPos (pawn:Pawn) =
            match boardPos, pawn.Color with
            | Outer(Color.Green, outerCoord), _ when outerCoord |> int <= 11 ->
               (11.0 - (outerCoord |> double), 15.0)
            | Outer(Color.Green, outerCoord), _ when outerCoord |> int >= 12 ->
               (0, 26.0 - (outerCoord |> double))
            | Outer(Color.Red, outerCoord), _ when outerCoord |> int <= 11 ->
               (0.0, 11.0 - (outerCoord |> double))
            | Outer(Color.Red, outerCoord), _ when outerCoord |> int >= 12->
               (0.0, 1.0 + (outerCoord |> int - 11))
            | Outer(Color.Blue, outerCoord), _ when outerCoord |> int <= 11 ->
               ((outerCoord |> double) + 4.0, 0.0)
            | Outer(Color.Blue, outerCoord), _ when outerCoord |> int >= 12 ->
               (15.0, 1.0 + outerCoord |> int - 11)
            | Outer(Color.Yellow, outerCoord), _ when outerCoord |> int <= 11 ->
               (15.0, (outerCoord |> double) + 4.0)
            | Outer(Color.Yellow, outerCoord), _ when outerCoord |> int >= 12 ->
               (14.0, 26.0 - outerCoord)
            | BoardPosition.Safety(safetySquare), Color.Green ->
                (13, 15 - safetySquare |> double)
            | BoardPosition.Safety(safetySquare), Color.Red ->
                (safetySquare |> double, 13)
            | BoardPosition.Safety(safetySquare), Color.Blue ->
                (2, safetySquare |> double)
            | BoardPosition.Safety(safetySquare), Color.Yellow ->
                (15 - safetySquare |> double, 2)
            | BoardPosition.Start, Color.Green ->
                match pawn.ID with
                | PawnID.One -> (10.5,13.0)
                | PawnID.Two -> (11.5,13.0)
                | PawnID.Three -> (10.5,14.0)
            | BoardPosition.Home, Color.Green ->
                match pawn.ID with
                | PawnID.One -> (12.5,8)
                | PawnID.Two -> (13.5,8)
                | PawnID.Three -> (13.5,9)
            | BoardPosition.Start, Color.Red ->
                match pawn.ID with
                | PawnID.One -> (1,10.5)
                | PawnID.Two -> (2,10.5)
                | PawnID.Three -> (1,11.5)
            | BoardPosition.Home, Color.Red ->
                match pawn.ID with
                | PawnID.One -> (6,12.5)
                | PawnID.Two -> (7,12.5)
                | PawnID.Three -> (6,13.5)
            | BoardPosition.Start, Color.Blue ->
                match pawn.ID with
                | PawnID.One -> (3.5, 1.0)
                | PawnID.Two -> (4.5, 1.0)
                | PawnID.Three -> (3.5, 2.0)
            | BoardPosition.Home, Color.Blue ->
                match pawn.ID with
                | PawnID.One -> (1.5, 6.0)
                | PawnID.Two -> (2.5, 6.0)
                | PawnID.Three -> (1.5, 7.0)
            | BoardPosition.Start, Color.Yellow ->
                match pawn.ID with
                | PawnID.One -> (13, 3.5)
                | PawnID.Two -> (14, 3.5)
                | PawnID.Three -> (13, 4.5)
            | BoardPosition.Home, Color.Yellow ->
                match pawn.ID with
                | PawnID.One -> (8, 1.5)
                | PawnID.Two -> (9, 1.5)
                | PawnID.Three -> (8, 2.5)
            // @TODO - safety
            | _ -> failwith "Invalid boardPos"
        let boardPosition = result {
            let! gameState = state.gameState
            return! gameState |> GameState.getTokenPositions
        }
        match boardPosition with
        | Ok(boardPosition) ->
            boardPosition
            |> Map.toList
            |> List.map (fun (p, pos) -> createPawn (p.Color |> toColorString) ((pos,p) ||> toBoardPos))
            |> List.concat
        | _ -> failwith "Unimplemented"
        
    (*let pawns =
        [ createPawn "Red" (5,0)
          createPawn "Yellow" (0, 0) ]
        |> List.concat *)
    Canvas.create [
        Canvas.background "Aqua"
        Canvas.children (outerSquares@safetySquares@startCircles@homeCircles@boostArrows@pawns)]
        