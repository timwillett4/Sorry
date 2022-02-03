#r "../../../FSharp.Core.Extensions/bin/Release/netstandard2.0/FSharp.Core.Extensions.dll"
#r "../bin/Release/netstandard2.0/SorryCore.dll"

open Sorry.Core
open Sorry.Core.BoardPosition
open Sorry.Core.Types

//let convertGlobalOuterToLocalAndPrint localColor rowColor (position:int) =
//    match position |> Int0to14.create with
//    | Ok position -> 
//        match (Outer (rowColor,position)) |> toLocalPosition localColor with
//        | Ok localPosition -> printf " Coversion success, local position: %A\n" localPosition  
//        | Error msg -> printf "Failed to convert to global coord: %s\n" msg
//    | Error msg -> printf "Failed to create global coord: %s\n" msg

//let convertRedOuterToRedLocal = convertGlobalOuterToLocalAndPrint Color.Red Color.Red
//let convertRedOuterToBlueLocal = convertGlobalOuterToLocalAndPrint Color.Blue Color.Red
//let convertRedOuterToYellowLocal = convertGlobalOuterToLocalAndPrint Color.Yellow Color.Red
//let convertRedOuterToGreenLocal = convertGlobalOuterToLocalAndPrint Color.Green Color.Red

//[0..16] |> List.iter convertRedOuterToRedLocal
//[0..16] |> List.iter convertRedOuterToBlueLocal
//[0..16] |> List.iter convertRedOuterToYellowLocal
//[0..16] |> List.iter convertRedOuterToGreenLocal
        
//let advanceLocalPositionAndPrintResult (color:Color) (position:int) (spaces:int) = 
//    match position |> Int0to65.create with
//    | Ok pos ->
//        match (LocalPosition (color, pos)) |> advanceLocalPosition spaces with
//        | Ok newPosition -> printf "Succesfully advance position to %A" newPosition
//        | Error msg -> printf "Failed to advance pos: %s" msg
//    | Error msg -> printf "Failed to create local coord: %s" msg

//advanceLocalPositionAndPrintResult Color.Red 4 3 // 7
//advanceLocalPositionAndPrintResult Color.Red 3 -7 // 56
//advanceLocalPositionAndPrintResult Color.Red 60 7 // invalid

//let convertLocalToGlobalAndPrint color (position:int) =
//    match position |> Int0to65.create with
//    | Ok position -> 
//        match LocalPosition (color, position) |> toGlobalPosition with
//        | Ok globalPosition -> printf "Success. Global pos: %A\n" globalPosition
//        | Error msg -> printf "Unable to convert to global pos: %s\n" msg 
//    | Error msg -> printf "Failed to convert to global coord: %s\n" msg

//let convertRedLocalToGlobalAndPrint = convertLocalToGlobalAndPrint Color.Red
//let convertYellowLocalToGlobalAndPrint = convertLocalToGlobalAndPrint Color.Yellow
//let convertGreenLocalToGlobalAndPrint = convertLocalToGlobalAndPrint Color.Green
//let convertBlueLocalToGlobalAndPrint = convertLocalToGlobalAndPrint Color.Blue
 
//[-1..66] |> List.iter convertYellowLocalToGlobalAndPrint
//[-1..66] |> List.iter convertRedLocalToGlobalAndPrint
//[-1..66] |> List.iter convertGreenLocalToGlobalAndPrint
//[-1..66] |> List.iter convertBlueLocalToGlobalAndPrint