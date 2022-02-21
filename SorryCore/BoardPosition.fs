module Sorry.Core.BoardPosition
//
//open FSharp.Core.Extensions.Result
//open FSharp.Core.Extensions.List
//open Sorry.Core
//
//type private LocalPosition = | LocalPosition of Color * Int0to65.T
//
//// constants
//let private spacesInRow = 15
//let private spaceFromFirstToHome = 65
//let private spacesFromFirstToSafety = 60
//let private numberOfColors = 4
//let private firstLocalSpaceInGlobal = 3
//
//// helper functions
//let private addToIfNegative addAmount value =
//    if value >= 0
//    then value
//    else value + addAmount
//
//let private add60IfNegative = addToIfNegative 60
//let private add4IfNegative = addToIfNegative 4
//
//let private getRowDiff (color1:Color) (color2:Color) =
//    System.Convert.ToInt32(color1) - System.Convert.ToInt32(color2) |> add4IfNegative
//
//let private toLocalPosition localColor boardPosition =
//
//    match boardPosition with
//
//    | Outer (rowColor, position) ->
//        result {
//            let rowDiff = getRowDiff localColor rowColor
//            let! localPosition = 
//                rowDiff * spacesInRow - 3 + (position |> Int0to14.value)
//                |> add60IfNegative
//                |> Int0to65.create 
//
//            return (LocalPosition (localColor, localPosition))
//        }
//
//    | Safety (safetyColor, safetyPosition) -> 
//        match safetyColor with
//        | _ when safetyColor = localColor ->
//            result {
//                let! localPosition = safetyPosition |> Int0to4.value |> Int0to65.create
//                return LocalPosition (localColor, localPosition) 
//            }
//        | _ -> 
//            Error "Invalid conversion: pawn may only move to safety spaces of same color"
//
//    | Home color ->
//        match color with
//        | _ when color = localColor ->
//            result {
//                let! localPosition = spaceFromFirstToHome |> Int0to65.create
//                return LocalPosition (localColor, localPosition) 
//            }
//        | _ -> Error "Invalid conversion: pawn may only move to home space of same color"
//    
//    | Start _ -> Error "Invalid conversion: start can not be converted to local coords"
//
//let private advanceLocalPosition spaces localPosition =
//    match localPosition with
//    | LocalPosition (localColor, position) ->
//        result {
//            let! newPosition = 
//                position
//                |> Int0to65.apply (fun i -> i + spaces |> add60IfNegative)
//                |> Int0to65.create
//
//            return LocalPosition (localColor, newPosition)
//        }
//
//let private toBoardPosition localPosition =
//    match localPosition with
//    | LocalPosition (localColor, position) ->
//        match position |> Int0to65.value with
//        | position when position = spaceFromFirstToHome ->
//            Ok (Home localColor)
//        | position when position >= spacesFromFirstToSafety ->
//            result {
//                let! safetyPosition = (position - spacesFromFirstToSafety) |> Int0to4.create 
//                return Safety (localColor, safetyPosition)
//            }
//        | pos ->
//            let globalPosition = pos + firstLocalSpaceInGlobal
//            let rowDiff = globalPosition / spacesInRow 
//            let color = enum<Color>((System.Convert.ToInt32(localColor) + rowDiff) % numberOfColors)
//            let outerPosition = globalPosition - spacesInRow * rowDiff
//
//            result {
//                let! outerPosition = outerPosition |> Int0to14.create
//                return Outer (color, outerPosition)
//            }
//
////let rec private advanceBoardPosition spaces pawnColor boardPosition =
//    // start move to position zero than advance spaces - 1
//    // @TODO - support different rules (any card can move out of start vs just 1 and 2)
//    match boardPosition with
//    | Start _ -> result {
//            if spaces > 1 then
//                let! zero = 0 |> Int0to65.create
//                let! positionZero = LocalPosition (pawnColor,zero) |> toBoardPosition
//                return! advanceBoardPosition (spaces - 1) pawnColor positionZero
//            else
//                return! Error "Can't move backwards from start position" }
//           
//    | _ ->
//        result {
//            let! localPawnPosition = boardPosition |> toLocalPosition pawnColor
//            let! updatedPosition = localPawnPosition |> advanceLocalPosition spaces
//            return! updatedPosition |> toBoardPosition
//        }
//  
//let private updatePosition (pawn:Pawn) (newPos:BoardPosition) (boardState:BoardState) =
//    boardState.Add (pawn.Color, boardState.[pawn.Color].Add (pawn.ID, newPos))
//
//let getPawnPosition (boardState:BoardState) (pawn:Pawn) =
//    boardState.[pawn.Color].[pawn.ID]
//
//let move spaces (pawn:Pawn) (boardState:BoardState) =
//        
//    let smallSliderPosition = 1
//    let smallSliderLength = 3
//    let bigSliderPosition = 9
//    let bigSliderLength = 4
//    
//    let isOccupiedByOwnPawnColor (boardState:BoardState) position =
//        not (boardState.[pawn.Color] 
//            |> Map.toList 
//            |> List.forall (fun (_,pawnPosition) -> pawnPosition <> position))
//            
//    
//    let isOnSmallSlider globalPosition =
//        match globalPosition with
//        | Outer (sliderColor, position) ->
//            sliderColor <> pawn.Color && (position |> Int0to14.value) = smallSliderPosition
//        | _ -> false        
//    
//    let isOnBigSlider globalPosition =
//        match globalPosition with
//        | Outer (sliderColor, position) ->
//            sliderColor <> pawn.Color && (position |> Int0to14.value) = bigSliderPosition
//        | _ -> false  
//        
//    // bump any pawn you land on, or any pawn you pass on the slider
//    let performBumps (boardState:BoardState) bumpPosition =
//    
//        let pawnsToBump = 
//            boardState 
//            |> Map.toList
//            |> List.collect (fun (color,positionMap) -> 
//                positionMap 
//                |> Map.toList 
//                |> List.map (fun (id,position) -> color,id,position))
//            |> List.filter (fun (_,_,position) -> position = bumpPosition)
//    
//        match pawnsToBump with
//        | [color,pawnID,_] ->
//            boardState |> updatePosition {Color=color;ID=pawnID} (Start color)
//        | [] -> boardState
//        | _ ->
//            System.Diagnostics.Trace.Assert(false, "Board is in invalid state. More than one pawn on same position")
//            boardState
//        
//    let performSlideIfOnSlider pawnPosition =
//        result {
//            match pawnPosition with
//            | smallSlider when smallSlider |> isOnSmallSlider ->
//                return! smallSlider |> advanceBoardPosition smallSliderLength pawn.Color
//            | bigSlider when bigSlider |> isOnBigSlider ->
//                return! bigSlider |> advanceBoardPosition bigSliderLength pawn.Color
//            | other -> return other }
//    
//         
//    let getBumpSpaces pawnPosition =
//    
//        let createOuterPositionList color positions =
//            positions
//            |> traverseResultA (fun p -> 
//                result {
//                    let! p = p |> Int0to14.create
//                    return Outer (color, p)})
//    
//        match pawnPosition with
//        | Outer (color,position) when pawnPosition |> isOnSmallSlider ->
//            let position = position |> Int0to14.value
//            [position..position+smallSliderLength] |> createOuterPositionList color
//        | Outer (color,position) when pawnPosition |> isOnBigSlider ->
//            let position = position |> Int0to14.value
//            [position..position+bigSliderLength] |> createOuterPositionList color
//        | Outer _ -> Ok [pawnPosition]
//        | _ -> Ok []
//    
//    result {
//        let pawnPosition = pawn |> getPawnPosition boardState
//        let! newPosition = pawnPosition |> advanceBoardPosition spaces pawn.Color
//        let! bumpSpaces = newPosition |> getBumpSpaces
//        let! newPosition = newPosition |> performSlideIfOnSlider
//    
//        if newPosition |> isOccupiedByOwnPawnColor boardState then
//            return! Error "Illegal move.  Space is already occupied by own pawn color\n"
//        else
//            let boardState = List.fold performBumps boardState bumpSpaces
//            return boardState |> updatePosition pawn newPosition }
//
//let swap (pawn1:Pawn) (pawn2:Pawn) boardState =
//    if pawn1.Color = pawn2.Color then
//        Error "Can't swap pawns of same color"
//    else
//        let getPawnPosition = getPawnPosition boardState
//        let pawn1Position = pawn1 |> getPawnPosition
//        let pawn2Position = pawn2 |> getPawnPosition
//    
//        match pawn1Position, pawn2Position with
//        | Start _, _
//        | Home _, _
//        | _, Start _
//        | _, Home _ -> Error "Can't swap with player on home or start"
//        | _ -> 
//            let boardState = boardState |> updatePosition pawn1 pawn2Position
//            Ok (boardState |> updatePosition pawn2 pawn1Position)
//
//let sorry (happyPawn:Pawn) (sadPawn:Pawn) boardState =
//    if happyPawn.Color = sadPawn.Color then
//        Error "Can't swap pawns of same color"
//    else
//        let getPawnPosition = getPawnPosition boardState
//        let happyPawnPosition = happyPawn |> getPawnPosition
//        let sadPawnPosition = sadPawn |> getPawnPosition
//    
//        match happyPawnPosition, sadPawnPosition with
//        | Start _, Outer (color,_) ->
//            let boardState = boardState |> updatePosition sadPawn (Start color)
//            Ok (boardState |> updatePosition happyPawn sadPawnPosition) 
//        | _ -> 
//            Error "Sorry action must be played by player in start and must bump player not on start, home or safey"
//
//let initializeBoard colors =
//    match colors with
//    | tooFew when colors |> Set.count < 2 ->
//        Error "Must have at least 2 players"
//    | invalid when colors |> Set.forall (fun color -> 
//            System.Enum.IsDefined(typeof<Color>, color) = false) ->
//        Error "Invalid color"
//    | valid ->
//        Ok (colors
//            |> Set.toList
//            |> List.map (fun color ->        
//                color,[PawnID.One, Start color;
//                       PawnID.Two, Start color;
//                       PawnID.Three, Start color]
//                      |> Map.ofList)
//            |> Map.ofList)
// card actions?