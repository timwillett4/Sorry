module Sorry.Core.GameState

open FSharp.Core.Extensions
open FSharp.Core.Extensions.Result
open Sorry.Core

/// The Sorry Game State consists
/// @TODO - finish


// Queries
let wrap max n = (n + max) % max
let nColors = 4
let incrementColor increment color =
    let wrap max n = (n + max) % max
    (color |> int) + increment |> wrap nColors |> enum
    
let positionAheadOfCurrentBy moveIncrement localColor currentPosition =
    let nSpacePerColor = 15
    
    let toLocal boardPosition =
       match boardPosition with
       | Home ->
           65
       | Safety(safetySquare) ->
           (safetySquare |> int) + 59
       | Outer(color, coord) ->
           // calculate how many colors away from the local color we are
           // Example: Colors are in order Green->Red->Blue->Yellow
           let colorDist = (color |> int) - (localColor |> int) |> wrap nColors
           match ((colorDist * nSpacePerColor) + (coord |> int)) with
           | 60 -> 0 // square 60 is safety 1, so we convert square 1 behind start to 0
           | local -> local
           
       | Start ->
           failwith "Start square is special square and can not be used with to local"

    let toBoardPosition localPosition =
        match localPosition with
        | _ when localPosition >= 1 && localPosition < 60 ->
            let colorDiff = localPosition / nSpacePerColor
            let outerCoord = localPosition - (colorDiff * nSpacePerColor)
            let color = ((localColor |> int) + colorDiff) % nColors |> enum<Color>
            Some(Outer(color, outerCoord |> enum))
        // This occurs when you are at or near the opening square and move backward
        | _ when localPosition <= 0 && localPosition >= -3 ->
            let color = ((localColor |> int) - 1) |> wrap nColors
            let color = color |> enum
            Some(Outer(color, nSpacePerColor + localPosition |> enum))
        | _ when localPosition >= 60 && localPosition <= 64 ->
            Some(Safety((localPosition - 59) |> enum))
        | _ when localPosition = 65 -> Some(Home)
        | _ -> None
       
    match currentPosition with 
    | Start ->
        assert(moveIncrement = 1)
        Some(Outer(localColor, OuterCoordinate.One))
    | position -> ((position |> toLocal) + moveIncrement) |> toBoardPosition
        
// @TODO convert to Option instead of result
let getTokenPositions game = 
    match game with
    | DrawingCard(gameState) -> gameState.TokenPositions
    | ChoosingAction(gameState) -> gameState.BoardState.TokenPositions
    | GameOver _ -> failwith "Not Implemented"
    
let getPlayers game = 
    match game with
    | DrawingCard(gameState) -> gameState.Players
    | ChoosingAction(gameState) -> gameState.BoardState.Players
    | GameOver _ -> failwith "Not Implemented"
    
let getActivePlayer game = 
    match game with
    | DrawingCard(gameState) -> gameState.ActivePlayer
    | ChoosingAction(gameState) -> gameState.BoardState.ActivePlayer
    | GameOver(gameState) -> gameState.Winner
    
let getAvailableActions game =
    match game with
    | GameOver _ -> []
    | DrawingCard _ -> [Action.DrawCard]
    | ChoosingAction(game) ->
        let activeColor = game.BoardState.ActivePlayer.Color
        let boardPositions = game.BoardState.TokenPositions
       
        let isOuterSquare position =
            match position with
            | Outer _ -> true
            | _ -> false
        
        let isValidMove moveIncrement (pawn:Pawn, position) =
            let ownPawnIsNotOnMoveToSquare (pawn:Pawn, moveToSquare) =
                let pieceIsOnMoveToSquare =
                    boardPositions |> Map.tryFindKey (fun pawn position -> pawn.Color = activeColor && position = moveToSquare)
                match pieceIsOnMoveToSquare with
                | Some _ -> false
                | None -> true
            let moveToSquare = position |> positionAheadOfCurrentBy moveIncrement pawn.Color
            match moveToSquare with
            | None -> false
            | Some(moveToSquare) when moveToSquare = Home -> true
            | Some(moveToSquare) -> ownPawnIsNotOnMoveToSquare (pawn, moveToSquare)
            
        let canMoveAnyPiece predicate moveIncrement =
            boardPositions
            |> Map.toList
            |> List.filter (fun (pawn, position) -> pawn.Color = activeColor && position |> predicate)
            |> List.filter (isValidMove moveIncrement)
            |> List.map (fun (pawn, _) -> Action.MovePawn(pawn, moveIncrement))
            
        let canMoveAnyPieceOutOfStart = canMoveAnyPiece (fun position -> position = Start) 1
                                            
        let canMoveAnyPieceNotOnStartOrHome = canMoveAnyPiece (fun position -> position <> Start && position <> Home)
        
        let canSwitchPlacesWithOpponentNotOnStartHomeOrSafety =
            
            let activePieces = boardPositions |> Map.filter (fun pawn _ -> pawn.Color = activeColor)
            let opponentPieces = boardPositions |> Map.filter (fun pawn _ -> pawn.Color <> activeColor)
            
            let ableToSwitch boardPositions =
                boardPositions
                |> Map.toList
                |> List.filter (fun (_Pawn, position) -> position |> isOuterSquare)
                |> List.map fst
                
            let activePiecesAbleToSwitch = activePieces |> ableToSwitch
            let opponentPieceAbleToSwitch = opponentPieces |> ableToSwitch 
            
            (activePiecesAbleToSwitch, opponentPieceAbleToSwitch)
            ||> List.allPairs
            |> List.map Action.SwitchPawns

        let canMoveAnyPieceOnStartToBumpAnyPieceNotOnStartHomeOrSafety =
            
            let activePiecesOnStart = boardPositions
                                      |> Map.toList
                                      |> List.filter (fun (pawn, position) -> pawn.Color = activeColor
                                                                              && position = Start)
                                      |> List.map fst
                                      
            let opponentPiecesNotOnStartHomeOrSafety = boardPositions
                                                       |> Map.toList
                                                       |> List.filter (fun (pawn, position) -> pawn.Color <> activeColor && position |> isOuterSquare)
                                                       |> List.map fst
            
            (activePiecesOnStart, opponentPiecesNotOnStartHomeOrSafety)
            ||> List.allPairs
            |> List.map Action.Sorry
            
        let canSplitMove7WithAny2PiecesNotOnStartOrHome =
            let splits = [(1,6);(2,5);(3,4);(4,3);(5,2);(6,1)]
            
            let pawnsEligibleToMove = boardPositions
                                       |> Map.filter (fun pawn position -> pawn.Color = activeColor && position <> Start && position <> Home)
                                       |> Map.toList
                                       |> List.map fst
            
            // Since known max pawns is 3 simpler to hard code all possibilities then come up with generic implementation
            let pieceSplits =
                match pawnsEligibleToMove with
                | [pawn1; pawn2; pawn3] -> [(pawn1, pawn2);(pawn1, pawn3);(pawn2, pawn3)]
                | [pawn1; pawn2] -> [(pawn1, pawn2)]
                | _ -> []
                
            (pieceSplits, splits)
            ||> List.allPairs
            |> List.map (fun ((pawn1, pawn2),(move1, move2)) -> SplitMove7((pawn1,move1),(pawn2,move2)))
            |> List.filter (fun split ->
                match split with
                | SplitMove7((pawn1, move1), (pawn2, move2)) ->
                    let pos1 = boardPositions.[pawn1]
                    let pos2 = boardPositions.[pawn2]
                    (isValidMove move1 (pawn1, pos1)) && (isValidMove move2 (pawn2, pos2))
                | _ -> failwith "Unexpected move")
                       
        let actions = match game.DrawnCard with
                      | Card.One ->canMoveAnyPieceOutOfStart@(canMoveAnyPieceNotOnStartOrHome 1)
                      | Card.Two ->canMoveAnyPieceOutOfStart@(canMoveAnyPieceNotOnStartOrHome 2)
                      | Card.Three -> canMoveAnyPieceNotOnStartOrHome 3
                      | Card.Four -> canMoveAnyPieceNotOnStartOrHome -4
                      | Card.Five -> canMoveAnyPieceNotOnStartOrHome 5
                      | Card.Seven -> (canMoveAnyPieceNotOnStartOrHome 7)@canSplitMove7WithAny2PiecesNotOnStartOrHome
                      | Card.Eight -> canMoveAnyPieceNotOnStartOrHome 8
                      | Card.Ten -> (canMoveAnyPieceNotOnStartOrHome 10)@(canMoveAnyPieceNotOnStartOrHome -1)
                      | Card.Eleven -> (canMoveAnyPieceNotOnStartOrHome 11)@canSwitchPlacesWithOpponentNotOnStartHomeOrSafety
                      | Card.Twelve -> canMoveAnyPieceNotOnStartOrHome 12
                      | Card.Sorry -> canMoveAnyPieceOnStartToBumpAnyPieceNotOnStartHomeOrSafety
                      
        let switchIsOnlyValidMove actions =
            actions |> List.forall (fun action -> match action with | SwitchPawns _ -> true | _ -> false)
            
        match actions with
        | [] -> [Action.PassTurn]
        | switch when actions |> switchIsOnlyValidMove -> switch@[Action.PassTurn]
        | actions -> actions
        
    
let getDrawnCard game = 
    match game with
    | ChoosingAction(game) -> Some(game.DrawnCard)
    | _ -> None
    
let getNumCardsLeft game = 
    match game with
    | ChoosingAction actionState -> actionState.BoardState.Deck.Length
    | DrawingCard boardState -> boardState.Deck.Length
    | GameOver boardState -> 0 // @TODO
    
let tryDrawCard random game =
    match game with
    | DrawingCard(gameState) ->
        // for simplicity sake, rather than shuffle deck, draw random card each time
        let drawIndex = random() % gameState.Deck.Length
        let topCut, bottomCut = gameState.Deck |> List.splitAt drawIndex
        let drawnCard = bottomCut.Head
        let newDeck =
            match topCut @ bottomCut.Tail with
            | [] -> newDeck
            | deck -> deck
        Ok(ChoosingAction({BoardState={gameState with Deck=newDeck};DrawnCard=drawnCard}))
    | _ -> Error(game, "Can only draw a card when game is in draw state")

let tryChooseAction random logger action game =
    
    logger game action  
    
    let updateActivePlayer gameState =
       assert(gameState.Players |> List.contains gameState.ActivePlayer)
       let currentIndex = gameState.Players |> List.findIndex (fun player -> player = gameState.ActivePlayer)
       let nextIndex = (currentIndex + 1) % gameState.Players.Length
       {gameState with ActivePlayer=gameState.Players.[nextIndex]}
                         
    let sendPawnBackToStartIfOnPosition (position:BoardPosition) (tokenPositions:TokenPositions) =
        let opponentToBump = tokenPositions |> Map.tryFindKey (fun _ p ->
            match p with
            | Home -> false
            | Safety _ -> false 
            | p when p = position -> true
            | _ -> false)
                                
        match opponentToBump with
        | Some(pawn) -> tokenPositions |> Map.add pawn Start
        | None -> tokenPositions
        
    let slideIfPawnLandsOnSlideSquare pawnToMove (tokenPositions: TokenPositions) =
          
        let bumpAnyPawnInSlideRegion (slideSquares:BoardPosition list) (tokenPositions:TokenPositions) =
            tokenPositions |> List.foldBack sendPawnBackToStartIfOnPosition slideSquares
              
        match tokenPositions.[pawnToMove] with
        | Outer(color, OuterCoordinate.Six) ->
            let slideSquares =  [ for i in 6..10 -> Outer(color, i |> enum) ]
            if color <> pawnToMove.Color then
                tokenPositions
                |> bumpAnyPawnInSlideRegion slideSquares
                |> Map.add pawnToMove (Outer(color, OuterCoordinate.Ten))
            else
                tokenPositions 
        | Outer(color, OuterCoordinate.Thirteen) ->
            let slideEnd = Outer(color |> incrementColor 1, OuterCoordinate.One)
            let slideSquares =  [ for i in 13..15 -> Outer(color, i |> enum) ] @ [slideEnd]
              
            // your own slide 3 is actually behind starting square so in previous color outer region
            let ownSlideColor = pawnToMove.Color |> incrementColor -1
            if color <> ownSlideColor then
                tokenPositions
                |> bumpAnyPawnInSlideRegion slideSquares
                |> Map.add pawnToMove slideEnd
            else
                tokenPositions 
        | _ -> tokenPositions
          
    // try move pawn or do we assume its legal
    // as we already found available moves???
    let movePawn (pawnToMove:Pawn) moveIncrement gameState =
       
       let currentPosition = gameState.TokenPositions.[pawnToMove]
       
       let newPosition = currentPosition |> positionAheadOfCurrentBy moveIncrement pawnToMove.Color
          
       match newPosition with
       | Some(newPosition) ->
           let newBoardState =
               gameState.TokenPositions
               |> sendPawnBackToStartIfOnPosition newPosition
               |> Map.add pawnToMove newPosition
               |> slideIfPawnLandsOnSlideSquare pawnToMove
           
           {gameState with TokenPositions=newBoardState}
        | None -> failwith "Invalid move"
       
    let switchPawns pawn1 pawn2 (gameState:BoardState) =
        let pawn1Pos = gameState.TokenPositions.[pawn1]
        let pawn2Pos = gameState.TokenPositions.[pawn2]
        let newTokenPositions = gameState.TokenPositions
                                |> Map.add pawn1 pawn2Pos
                                |> Map.add pawn2 pawn1Pos
                                |> slideIfPawnLandsOnSlideSquare pawn1
                                |> slideIfPawnLandsOnSlideSquare pawn2
                                
        {gameState with TokenPositions=newTokenPositions}
       
    let sorry pawnOnStart pawnToBump (gameState:BoardState) =
        assert(gameState.TokenPositions.[pawnOnStart] = Start)
        let pawnToBumpPos = gameState.TokenPositions.[pawnToBump]
        let newTokenPositions = gameState.TokenPositions
                                |> Map.add pawnOnStart pawnToBumpPos
                                |> Map.add pawnToBump Start
        
        {gameState with TokenPositions=newTokenPositions}
        
    result {
        if game |> getAvailableActions |> List.contains action then
            
            // Since we have validated that it is an available action we can
            // now assume all actions are valid below and just update without worry
            // of it putting game in invalid state
            return! match game with
                    | DrawingCard(_) ->
                        match action with
                        | DrawCard -> game |> tryDrawCard random
                        | _ -> Error(game, "Can only draw card when game is in draw state")
                    | ChoosingAction(gameState) ->
                        let checkWinner (boardState:BoardState) =
                            boardState.TokenPositions
                            |> Map.toSeq
                            |> Seq.groupBy (fun (pawn, position) -> pawn.Color)
                            |> Seq.tryFind (fun (color, positions) -> positions |> Seq.forall (fun (_, pos) -> pos = Home))
                            |> Option.bind (fun (color, _) -> boardState.Players |> List.tryFind (fun player -> player.Color = color))
                           
                        match action with
                        | MovePawn(pawn,moveIncrement) ->
                            let newBoardState = gameState.BoardState |> movePawn pawn moveIncrement
                            match newBoardState |> checkWinner with
                            | Some(winner) -> Ok(GameOver{Winner=winner})
                            | None ->
                                match gameState.DrawnCard with
                                | Card.Two -> Ok(DrawingCard(newBoardState))
                                | _ -> Ok(DrawingCard(newBoardState |> updateActivePlayer))
                        | SplitMove7((pawn1, move1),(pawn2, move2)) ->
                            let newBoardState = gameState.BoardState
                                                |> movePawn pawn1 move1
                                                |> movePawn pawn2 move2
                            Ok(DrawingCard(newBoardState |> updateActivePlayer))
                        | SwitchPawns(pawn1,pawn2) ->
                            let newBoardState = gameState.BoardState |> switchPawns pawn1 pawn2
                            Ok(DrawingCard(newBoardState |> updateActivePlayer))
                        | Sorry(pawn1, pawn2) ->
                            let newBoardState = gameState.BoardState |> sorry pawn1 pawn2
                            Ok(DrawingCard(newBoardState |> updateActivePlayer))
                        | PassTurn -> Ok(DrawingCard(gameState.BoardState |> updateActivePlayer))
                        | _ -> Error(game, "Unimplemented")
                    | _ -> Error(game, "Unimplemented")
        else            
            return! Error(game, "Choosing Invalid Action")
    }
