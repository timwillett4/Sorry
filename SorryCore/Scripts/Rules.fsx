#r "../../submodules/FSharp.Core.Extensions/bin/Release/netstandard2.0/FSharp.Core.Extensions.dll"
#r "../bin/Release/netstandard2.0/SorryCore.dll"

open Sorry.Core

open System
type Rule = BoardState -> Result<BoardState, string>

let createPerColorRule check (boardState:BoardState) =
    boardState |> Map.forall (fun tokenColor positions -> positions |> List.forall (tokenColor |> check))

let ``Start can only be occupied by the matching color`` = 
    createPerColorRule <| fun tokenColor position -> 
        match position with
        | Start startColor -> startColor = tokenColor
        | _ -> true;

let ``Home can only be occupied by the matching color`` = 
    createPerColorRule <| fun tokenColor position -> 
        match position with
        | Home homeColor -> homeColor = tokenColor
        | _ -> true;

let ``Safety can only be occupied by the matching color`` = 
    createPerColorRule <| fun tokenColor position -> 
        match position with
        | Safety (safetyColor,_) -> safetyColor = tokenColor
        | _ -> true;

//let ``Can not finish on the start of another colors slider`` = 
//    createPerColorRule <| fun tokenColor position -> 
//    | Outer (color, index) ->
//        match color with
//        | tokenColor -> true
//        | _ ->
//            match index with
//            // should have slid when landing on another slider
//            | i when i = smallSliderStart -> false 
//            | i when i = bigSliderStart -> false
//            | _ -> true        

let ``Two pieces can not be on the same square (except for start or home)`` boardState = 
    let piecesNotOnStartOrHome = 
        boardState 
        |> Map.toList
        |> List.collect (fun (key,value) -> value)
        |> List.filter (fun position -> 
            match position with
            | Home _ -> false
            | Start _ -> false
            | _ -> true)

    let numberOfTokensNotOnStartOrHome = piecesNotOnStartOrHome |> List.length
    let numberOfUniquePositions = piecesNotOnStartOrHome |> List.distinct |> List.length

    numberOfTokensNotOnStartOrHome = numberOfUniquePositions

let ``Every color must have 4 tokens`` boardState = 
    boardState |> Map.forall (fun tokenColor positions -> positions |> List.length = 4)

let ``Must be 2 to 4 players`` (boardState:BoardState) = 
    let numberOfPlayers = boardState |> Map.count
    match numberOfPlayers with
    | x when x >=2 && x <=4 -> true
    | _ -> false