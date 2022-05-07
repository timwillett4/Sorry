module Sorry.Desktop.Main

open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls.Shapes
open Avalonia.Media

type State =
    { state : bool }
    
// TokenPositions
// Available Actions
// 
let initialState() =
    { state = false }, Elmish.Cmd.none

type Msg =
| Start
| Stop

let update (msg: Msg) (state: State) : State * Elmish.Cmd<_>=
    match msg with
    | Start -> { state with state = true }, Elmish.Cmd.none
    | Stop -> { state with state = false }, Elmish.Cmd.none

let toScreenCoords borderWidth squareWidth x y =
    let left = borderWidth + squareWidth * x
    let top = borderWidth + squareWidth * y
    (left, top)
    
let rec createSquare borderWidth squareWidth (color:string) (x, y) =
    let left,top = toScreenCoords borderWidth squareWidth x y
    Rectangle.create [
        Rectangle.fill color
        Rectangle.width squareWidth
        Rectangle.height squareWidth
        Rectangle.left left
        Rectangle.top top
        Rectangle.stroke "Black"
        Rectangle.strokeThickness 1.0
    ] :> Types.IView
    
let createCircle borderWidth squareWidth circleRatio x y (color:string) =
    let left,top = toScreenCoords borderWidth squareWidth x y
    Ellipse.create [
        Ellipse.fill color
        Ellipse.width (squareWidth * circleRatio)
        Ellipse.height (squareWidth * circleRatio)
        Ellipse.left left
        Ellipse.top top
        Ellipse.stroke "Black"
        Ellipse.strokeThickness 2.0
    ] :> IView

let createArrow borderWidth squareWidth (color:string) (startX, startY) (endX, endY) =
    let startX,startY = toScreenCoords borderWidth squareWidth startX startY
    let endX,endY = toScreenCoords borderWidth squareWidth endX endY
    [
        Line.create [
            Line.startPoint (startX, startY)
            Line.endPoint (endX, endY)
            Line.strokeLineCap PenLineCap.Round
            Line.strokeJoinCap PenLineJoin.Bevel
            Line.stroke color
            Line.strokeThickness 2.0
        ] :> IView
        
        // |--->
        // @TODO
        (*let horizontal = startY - endY = 0 
        let (cap1Start,cap1End) =
            match horizontal with
            | true -> ((endX-.25,endY-.25),(endX, endY))
            | false -> ((endX-.25,endY-.25),(endX, endY))*)
            
        Line.create [
            Line.startPoint (startX, startY)
            Line.endPoint (endX, endY)
            Line.strokeLineCap PenLineCap.Round
            Line.strokeJoinCap PenLineJoin.Bevel
            Line.stroke color
            Line.strokeThickness 2.0
        ] :> IView
        
        Line.create [
            Line.startPoint (startX, startY)
            Line.endPoint (endX, endY)
            Line.strokeLineCap PenLineCap.Round
            Line.strokeJoinCap PenLineJoin.Bevel
            Line.stroke color
            Line.strokeThickness 2.0
        ] :> IView
        
        Line.create [
            Line.startPoint (startX, startY)
            Line.endPoint (startX, startY)
            Line.strokeLineCap PenLineCap.Round
            Line.strokeJoinCap PenLineJoin.Bevel
            Line.stroke color
            Line.strokeThickness 2.0
        ] :> IView
    ]    
type Square = {
    x : int // @TODO contrained type 0 -16??
    y : int
    color : string
}

let view (state: State) (dispatch: Msg -> unit) =
    Canvas.create [
        Canvas.background "Aqua"
        // Start out with simple circle for each pawn
        // draw in center of square except for start/home draw by color
        // @TODO - add y and draw all squares
        
        let createRow color (xs, ys) =
            let xs = xs |> List.map double
            let ys = ys |> List.map double
            let coords = (xs, ys) ||> List.allPairs
            // @TODO - scale by screen width/height
            let createGameSquare = createSquare 20.0 30.0 
            coords |> List.map (createGameSquare color)
            
        let outerSquares =
            [([0..15],[0])
             ([0..15],[15])
             ([0],[1..14])
             ([15],[1..14])
            ]
            |> List.collect (createRow "Gray")
            
        let yellowSafety = ([13],[10..14]) |> createRow "Yellow"
        let greenSafety = ([1..5],[13]) |> createRow "Green"
        let redSafety = ([2],[1..5]) |> createRow "Red"
        let blueSafety = ([10..14],[2]) |> createRow "Blue"
        
        let createBigCircle = createCircle 20.0 30.0 2.0
        
        let blueStart = createBigCircle 13 3.5 "Blue"
        let blueHome = createBigCircle 8.0 1.5 "Blue"
        
        let yellowStart = createBigCircle 10.5 13.0 "Yellow"
        let yellowHome = createBigCircle 12.5 8.0 "Yellow"
        
        let redStart = createBigCircle 3.5 1.0 "Red"
        let redHome = createBigCircle 1.5 6 "Red"
        
        let greenStart = createBigCircle 1 10.5 "Green"
        let greenHome = createBigCircle 6.0 12.5 "Green"
        
        let circles = [blueStart
                       blueHome
                       yellowStart
                       yellowHome
                       redStart
                       redHome
                       greenStart
                       greenHome]
        
        
        let createArrow = createArrow 20.0 30.0
        
        let createYellowArrow = createArrow "Yellow"
        let createGreenArrow = createArrow "Green"
        let createRedArrow = createArrow "Red"
        let createBlueArrow = createArrow "Blue"
        let boostArrows = [createYellowArrow (14.5, 15.5) (11.5, 15.5)
                           createYellowArrow (6.5, 15.5) (2.5, 15.5)
                           createGreenArrow (0.5, 14.5) (0.5, 11.5) 
                           createGreenArrow (0.5, 6.5) (0.5, 2.5)
                           createRedArrow (1.5, 0.5) (4.5, 0.5) 
                           createRedArrow (9.5, 0.5) (13.5, 0.5)
                           createBlueArrow (15.5, 1.5) (15.5, 4.5) 
                           createBlueArrow (15.5, 9.5) (15.5, 13.5)]
                          |> List.concat
        
        // @TODO - drawPawns
        let createPawn = createCircle 20.0 30.0 0.5
        
        Canvas.children (outerSquares@yellowSafety@greenSafety@redSafety@blueSafety@circles@boostArrows)
    ]