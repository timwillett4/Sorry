module Sorry.Desktop.Main

open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Media
open Avalonia.FuncUI.Types
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Internal.Utilities.StructuredFormat
open Sorry.Core

type State = {
  gameState : GameState
  error : string Option
}
    
// TokenPositions
open System
open FSharp.Core.Extensions.Result


let random = Random()
let getRandom() =
    random.Next(Int32.MaxValue)
let initialState() =
    let game = result {
        
        let game = GameState.newGame
        let! game = game |> GameState.tryAddPlayer "Levi" Color.Green
        //let! game = game |> GameState.tryAddPlayer "Corbin" Color.Red
        let! game = game |> GameState.tryAddPlayer "Tim" Color.Blue
        
        return! game |> GameState.tryStartGame getRandom
    }
    
    match game with
    | Ok(game) -> {gameState = game; error = None}, Elmish.Cmd.none
    | Error(game, error) -> {gameState = game; error = Some(error)}, Elmish.Cmd.none

type Msg =
| ChooseAction of DomainTypes.Action

let actionText action =
    let pawnText (pawn:Pawn) =
        let colorText color =
           match color with
           | Color.Green -> "Green"
           | Color.Red -> "Red"
           | Color.Blue -> "Blue"
           | Color.Yellow -> "Yellow"
           | _ -> failwith "Invalid pawn color"
        let IDText id =
           match id with
           | PawnID.One -> "One"
           | PawnID.Two -> "Two"
           | PawnID.Three -> "Three"
           | PawnID.Four -> "Four"
        (pawn.Color |> colorText) + "-" + (pawn.ID |> IDText)
    match action with
    | DomainTypes.Action.Sorry(pawnOnStart, pawnToBump) ->
        $"Sorry: Move %s{pawnOnStart |> pawnText} out of start and send %s{pawnToBump |> pawnText} back to start"
    | DomainTypes.Action.DrawCard ->
        "Draw a card"
    | DomainTypes.Action.MovePawn(pawn, spaces) ->
        $"Move pawn %s{pawn |> pawnText} %i{spaces} spaces"
    | DomainTypes.Action.PassTurn ->
        "Pass turn"
    | DomainTypes.Action.SplitMove7((pawn1, move1), (pawn2, move2)) ->
        $"Move pawn %s{pawn1 |> pawnText} %i{move1} spaces and %s{pawn2 |> pawnText} %i{move2} spaces"
    | DomainTypes.Action.SwitchPawns(pawn1, pawn2) ->
        $"Switch %s{pawn1 |> pawnText} with %s{pawn2 |> pawnText}"
let gameID = System.DateTime.Now.ToString("s").Replace(":","_");
let update (msg: Msg) (state: State) : State * Elmish.Cmd<_>=
    let logger game action =
        
        //let game = System.Text.Json.JsonSerializer.Serialize game
        //let action = System.Text.Json.JsonSerializer.Serialize action
        //System.IO.File.AppendAllText($"GameLog%s{gameID}.txt", game + Environment.NewLine)
        let game = game |> Display.printBoardState
        System.IO.File.AppendAllText($"GameLog_{gameID}.txt", game + Environment.NewLine)
        let action = action |> actionText
        System.IO.File.AppendAllText($"GameLog_{gameID}.txt", Environment.NewLine + "Chose Action: " + action + Environment.NewLine + Environment.NewLine)
    let chooseAction = GameState.tryChooseAction getRandom logger
    match msg with
    | ChooseAction(action) ->
        match state.gameState |> chooseAction action with
        | Ok(game) -> {gameState = game; error = None}, Elmish.Cmd.none
        | Error(game, error) -> {gameState = game; error = Some(error)}, Elmish.Cmd.none

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
    
    let createPawn (color:string) pawnID (x, y) =
        let pawnTop = createEllipse 0.4 0.4 (x + 0.3) (y) color
        let pawnNeck = createEllipse 0.3 0.5 (x + 0.35) (y + 0.3) color
        let pawnBase = createEllipse 0.8 0.4 (x + 0.1) (y + 0.6) color
        let pawnID =
             let left,top = toScreenCoords borderWidth squareWidth x y
             let drawTallyMark (startPoint:double*double) (endPoint:double*double) =
                    Line.create [
                        Line.startPoint (startPoint |> Avalonia.Point)
                        Line.endPoint (endPoint |> Avalonia.Point)
                        Line.strokeLineCap PenLineCap.Round
                        Line.strokeJoinCap PenLineJoin.Bevel
                        Line.stroke "Black"
                        Line.strokeThickness 2.0
                    ] :> IView
             
             match pawnID with
                | PawnID.One -> 
                    let startPoint = (left + (squareWidth/2.0), top + 20.0)
                    let endPoint = (left + (squareWidth/2.0), top + 30.0)
                    let tallyMark = drawTallyMark startPoint endPoint
                    
                    [tallyMark]
                | PawnID.Two ->
                    let tallyMark1 =
                        let startPoint = (left + (squareWidth/2.0) - 5.0, top + 20.0)
                        let endPoint = (left + (squareWidth/2.0) - 5.0, top + 30.0)
                        drawTallyMark startPoint endPoint
                    let tallyMark2 =
                        let startPoint = (left + (squareWidth/2.0) + 5.0, top + 20.0)
                        let endPoint = (left + (squareWidth/2.0) + 5.0, top + 30.0)
                        drawTallyMark startPoint endPoint
                    
                    [tallyMark1; tallyMark2]
                    
                | PawnID.Three ->
                    let tallyMark1 =
                        let startPoint = (left + (squareWidth/2.0) - 5.0, top + 20.0)
                        let endPoint = (left + (squareWidth/2.0) - 5.0, top + 30.0)
                        drawTallyMark startPoint endPoint
                    let tallyMark2 =
                        let startPoint = (left + (squareWidth/2.0), top + 20.0)
                        let endPoint = (left + (squareWidth/2.0), top + 30.0)
                        drawTallyMark startPoint endPoint
                    let tallyMark3 =
                        let startPoint = (left + (squareWidth/2.0) + 5.0, top + 20.0)
                        let endPoint = (left + (squareWidth/2.0) + 5.0, top + 30.0)
                        drawTallyMark startPoint endPoint
                        
                    [tallyMark1; tallyMark2; tallyMark3]
                    
                | PawnID.Four ->
                    let tallyMark1 =
                        let startPoint = (left + (squareWidth/2.0) - 5.0, top + 20.0)
                        let endPoint = (left + (squareWidth/2.0) - 5.0, top + 30.0)
                        drawTallyMark startPoint endPoint
                    let tallyMark2 =
                        let startPoint = (left + (squareWidth/2.0) - 1.7, top + 20.0)
                        let endPoint = (left + (squareWidth/2.0) - 1.7, top + 30.0)
                        drawTallyMark startPoint endPoint
                    let tallyMark3 =
                        let startPoint = (left + (squareWidth/2.0) + 1.7, top + 20.0)
                        let endPoint = (left + (squareWidth/2.0) + 1.7, top + 30.0)
                        drawTallyMark startPoint endPoint
                    let tallyMark4 =
                        let startPoint = (left + (squareWidth/2.0) + 5.0, top + 20.0)
                        let endPoint = (left + (squareWidth/2.0) + 5.0, top + 30.0)
                        drawTallyMark startPoint endPoint
                        
                    [tallyMark1; tallyMark2; tallyMark3; tallyMark4]
                         
        [pawnBase; pawnNeck; pawnTop]@pawnID
        
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
          createArrow (0, 6) (0, 2) "Red"
          createArrow (1, 0) (4, 0) "Blue"
          createArrow (9, 0) (13, 0) "Blue"
          createArrow (15, 1) (15, 4) "Yellow"
          createArrow (15, 9) (15, 13) "Yellow" ]
        |> List.concat
        
    let pawns =
        let toColorString color =
            match color with
            | Color.Red -> "Red"
            | Color.Green -> "Green"
            | Color.Blue -> "Blue"
            | Color.Yellow -> "Yellow"
            | _ -> failwith "Invalid enum"
        state.gameState |> GameState.getTokenPositions
        |> Map.toList
        |> List.map (fun (p, pos) -> createPawn (p.Color |> toColorString) (p.ID) ((pos,p) ||> Presentation.toScreenCoords))
        |> List.concat
    
    let actionListView (state:State) dispatch =
                
        ListBox.create [
            ListBox.dock Dock.Left
            (*ListBox.onSelectedItemChanged (fun obj ->
                match obj with
                | :? Product as p -> p |> Some |> Select |> dispatch
                | _ -> None |> Select |> dispatch
            )*)
            ListBox.dataItems (state.gameState |> GameState.getAvailableActions)
            ListBox.itemTemplate (
                DataTemplateView<DomainTypes.Action>.create (fun action ->
                    DockPanel.create [
                        DockPanel.lastChildFill false
                        DockPanel.children [
                            Border.create [
                                Border.width 16.0
                                Border.height 16.0
                                Border.cornerRadius 8.0
                                Border.margin 5.0
                                //Border.background data.FavoriteColor
                            ]
                            Button.create [
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.content (action |> actionText)
                                Button.onClick ((fun _ -> action |> Msg.ChooseAction |> dispatch), SubPatchOptions.OnChangeOf action)
                            ]                                         
                        ]
                    ]                                  
                )                  
            )
        ]
    
    let activePlayerView (activePlayer:Player) =
        TextBlock.create [
                Border.dock Dock.Top
                TextBlock.text $"Active player: %A{activePlayer}"
                TextBlock.fontWeight FontWeight.Bold
                TextBlock.margin 5.0
        ] : IView
    let drawnCardView (drawnCard:Card) =
        TextBlock.create [
                Border.dock Dock.Top
                TextBlock.text $"Drawn card: %A{drawnCard}"
                TextBlock.fontWeight FontWeight.Bold
                TextBlock.margin 5.0
        ] : IView
        
    let winnerView (winner:Player) =
        TextBlock.create [
                Border.dock Dock.Top
                TextBlock.text $"%A{winner} is the winner"
                TextBlock.fontWeight FontWeight.Bold
                TextBlock.margin 5.0
        ] :> IView
        
    SplitView.create [
        Grid.row 2

        SplitView.displayMode SplitViewDisplayMode.Inline
        SplitView.panePlacement SplitViewPanePlacement.Left
        SplitView.useLightDismissOverlayMode false 
        SplitView.isPaneOpen true 
        SplitView.openPaneLength 500.0 
        SplitView.compactPaneLengthProperty  500.0

        Canvas.create [
            Canvas.background "Aqua"
            Canvas.children (outerSquares@safetySquares@startCircles@homeCircles@boostArrows@pawns)
        ]
        |> SplitView.content

        let paneContent =
            match state.gameState with
            | GameOver(game) -> [winnerView game.Winner]
            | _ -> 
                let activePlayer = state.gameState |> GameState.getActivePlayer
                let drawnCard = state.gameState |> GameState.getDrawnCard
                match (activePlayer,drawnCard) with
                | Some(activePlayer), Some(card) -> [
                       (activePlayerView activePlayer) 
                       (drawnCardView card) 
                       (actionListView state dispatch) 
                    ]
                | Some(activePlayer), None -> [
                       (activePlayerView activePlayer) 
                       (actionListView state dispatch) 
                    ]
                | None, Some(_) -> failwith "Invalid state"
                | None,None -> [actionListView state dispatch]
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Vertical
            StackPanel.margin 10.0
            StackPanel.children paneContent 
        ]
        |> SplitView.pane
    ]