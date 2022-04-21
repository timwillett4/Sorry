﻿module SorryFSharpConsole.Display

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