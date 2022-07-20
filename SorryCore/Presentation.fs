module Sorry.Core.Presentation

let toScreenCoords boardPos (pawn:Pawn) =
    match boardPos, pawn.Color with
    | Outer(Color.Green, outerCoord), _ when outerCoord |> int <= 12 ->
       (12.0 - (outerCoord |> double), 15.0)
    | Outer(Color.Green, outerCoord), _ when outerCoord |> int >= 13 ->
       (0, 27.0 - (outerCoord |> double))
    | Outer(Color.Red, outerCoord), _ when outerCoord |> int <= 12 ->
       (0.0, 12.0 - (outerCoord |> double))
    | Outer(Color.Red, outerCoord), _ when outerCoord |> int >= 13 ->
       ((outerCoord |> double) - 12.0, 0.0)
    | Outer(Color.Blue, outerCoord), _ when outerCoord |> int <= 12 ->
       ((outerCoord |> double) + 3.0, 0.0)
    | Outer(Color.Blue, outerCoord), _ when outerCoord |> int >= 13 ->
       (15.0, (outerCoord |> double) - 12.0)
    | Outer(Color.Yellow, outerCoord), _ when outerCoord |> int <= 12 ->
       (15.0, (outerCoord |> double) + 3.0)
    | Outer(Color.Yellow, outerCoord), _ when outerCoord |> int >= 13 ->
       (27.0 - (outerCoord |> double), 15.0)
    | BoardPosition.Safety(safetySquare), Color.Green ->
        (13.0, 15.0 - (safetySquare |> double))
    | BoardPosition.Safety(safetySquare), Color.Red ->
        (safetySquare |> double, 13)
    | BoardPosition.Safety(safetySquare), Color.Blue ->
        (2, safetySquare |> double)
    | BoardPosition.Safety(safetySquare), Color.Yellow ->
        (15.0 - (safetySquare |> double), 2)
    | BoardPosition.Start, Color.Green ->
        match pawn.ID with
        | PawnID.One -> (10.5,13.0)
        | PawnID.Two -> (11.5,13.0)
        | PawnID.Three -> (10.5,14.0)
        | PawnID.Four -> (11.5,14.0)
    | BoardPosition.Home, Color.Green ->
        match pawn.ID with
        | PawnID.One -> (12.5,8)
        | PawnID.Two -> (13.5,8)
        | PawnID.Three -> (12.5,9)
        | PawnID.Four -> (13.5,9)
    | BoardPosition.Start, Color.Red ->
        match pawn.ID with
        | PawnID.One -> (1,10.5)
        | PawnID.Two -> (2,10.5)
        | PawnID.Three -> (1,11.5)
        | PawnID.Four -> (2,11.5)
    | BoardPosition.Home, Color.Red ->
        match pawn.ID with
        | PawnID.One -> (6,12.5)
        | PawnID.Two -> (7,12.5)
        | PawnID.Three -> (6,13.5)
        | PawnID.Four -> (7,13.5)
    | BoardPosition.Start, Color.Blue ->
        match pawn.ID with
        | PawnID.One -> (3.5, 1.0)
        | PawnID.Two -> (4.5, 1.0)
        | PawnID.Three -> (3.5, 2.0)
        | PawnID.Four -> (4.5,2.0)
    | BoardPosition.Home, Color.Blue ->
        match pawn.ID with
        | PawnID.One -> (1.5, 6.0)
        | PawnID.Two -> (2.5, 6.0)
        | PawnID.Three -> (1.5, 7.0)
        | PawnID.Four -> (2.5, 7.0)
    | BoardPosition.Start, Color.Yellow ->
        match pawn.ID with
        | PawnID.One -> (13, 3.5)
        | PawnID.Two -> (14, 3.5)
        | PawnID.Three -> (13, 4.5)
        | PawnID.Four -> (14, 4.5)
    | BoardPosition.Home, Color.Yellow ->
        match pawn.ID with
        | PawnID.One -> (8, 1.5)
        | PawnID.Two -> (9, 1.5)
        | PawnID.Three -> (8, 2.5)
        | PawnID.Four -> (9, 2.5)
    | _ -> failwith "Invalid board position"