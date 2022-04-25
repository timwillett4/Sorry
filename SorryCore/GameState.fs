module Sorry.Core.GameState

open FSharp.Core.Extensions
open FSharp.Core.Extensions.Result
open FSharp.Core.Extensions.Validation
open Sorry.Core

/// The Sorry Game State consists
/// @TODO - finish


/// <summary>
/// New came starts in setup state with no players
/// Players can be added with <see cref="addPlayers">addPlayers</see>
/// </summary>
let newGame = SettingUp({Players=[]})

// Queries

/// getChosenColors returns a list of the colors that have already been chosen
let private getChosenColors (game:SetupState) = game.Players |> List.map (fun player -> player.Color)

/// Converts board position to 1 first square out of start to 65(home) representing
/// local coordinates for a particular color

let wrap max n = (n + max) % max
let nColors = 4
let incrementColor increment color =
    let wrap max n = (n + max) % max
    (color |> int) + increment |> wrap nColors |> enum
    
let positionAheadOfCurrentBy moveIncrement localColor currentPosition =
    let nSpacePerColor = 15
    
    let toLocal boardPosition =
       match boardPosition with
       | Home(color)->
           assert(color = localColor)
           65
       | Safety(color, safetySquare) ->
           assert(color = localColor)
           (safetySquare |> int) + 59
       | Outer(color, coord) ->
           // calculate how many colors away from the local color we are
           // Example: Colors are in order Green->Red->Blue->Yellow
           let colorDist = (color |> int) - (localColor |> int) |> wrap nColors
           (colorDist * nSpacePerColor) + (coord |> int)
       | Start(color) ->
           assert(color = localColor)
           failwith $"Start square is special square and can not be used with to local"

    let toBoardPosition localPosition =
        match localPosition with
        | _ when localPosition >= 1 && localPosition <= 60 ->
            let colorDiff = localPosition / nSpacePerColor
            let outerCoord = localPosition - (colorDiff * nSpacePerColor)
            let color = ((localColor |> int) + colorDiff) % nColors |> enum<Color>
            Outer(color, outerCoord |> enum)
        // This occurs when you are at or near the opening square and move backward
        | _ when localPosition <= 0 && localPosition >= -3 ->
            let color = ((localColor |> int) - 1) |> wrap nColors
            let color = color |> enum
            Outer(color, nSpacePerColor + localPosition |> enum)
        | _ when localPosition >= 60 && localPosition <= 64 ->
            Safety(localColor, (localPosition - 59) |> enum)
        | _ when localPosition = 65 -> Home(localColor)
        | _ -> failwith $"Invalid board position"
       
    match currentPosition with 
    | Start(color) ->
        assert(moveIncrement = 1)
        Outer(color, OuterCoordinate.One)
    | position -> ((position |> toLocal) + moveIncrement) |> toBoardPosition
        
/// getAvailableColors returns the available colors left to choose from
/// this query is only valid before a game has been started via calling 'startGame'
let getAvailableColors game =
    match game with
    | SettingUp(game) ->
        let allColors = FSharpType.getAllEnumsValues<Color>()
        let chosenColors = game |> getChosenColors
        List.distinct allColors chosenColors
    | _ -> []

let getTokenPositions game = 
    match game with
    | Drawing(gameState) -> Ok(gameState.TokenPositions)
    | SettingUp _ -> Error(game, "Game is still in setup state")
    | _ -> Error(game, "Not implemented")
    
let getPlayers game = 
    match game with
    | Drawing(gameState) -> Ok(gameState.Players)
    | SettingUp _ -> Error(game, "Game is still in setup state")
    | _ -> Error(game, "Not implemented")
    
let getActivePlayer game = 
    match game with
    | Drawing(gameState) -> Ok(gameState.ActivePlayer)
    | SettingUp _ -> Error(game, "Game is still in setup state")
    | _ -> Error(game, "Not implemented")
    
let getAvailableActions game =
    match game with
    | Drawing _ -> Ok([Action.DrawCard])
    | ChoosingAction(game) ->
        let activeColor = game.BoardState.ActivePlayer.Color
        let boardPositions = game.BoardState.TokenPositions
       
        let isOuterSquare position =
            match position with
            | Outer _ -> true
            | _ -> false
        
        let canMoveAnyPiece predicate moveIncrement =
            let ownPawnIsNotOnMoveToSquare (pawn:Pawn, position) =
               let newPosition = position |> positionAheadOfCurrentBy moveIncrement pawn.Color
               let pieceOnMoveToSquare = boardPositions |> Map.tryFindKey (fun pawn position -> pawn.Color = activeColor && position = newPosition)
               match pieceOnMoveToSquare with
               | Some _ -> false
               | None -> true
                   
            boardPositions
            |> Map.toList
            |> List.filter (fun (pawn, position) -> pawn.Color = activeColor && position |> predicate)
            |> List.filter ownPawnIsNotOnMoveToSquare 
            |> List.map (fun (pawn, _) -> Action.MovePawn(pawn, moveIncrement))
            
        let canMoveAnyPieceOutOfStart = canMoveAnyPiece (fun position -> position = Start(activeColor)) 1
                                            
        let canMoveAnyPieceNotOnStart = canMoveAnyPiece (fun position -> position <> Start(activeColor))
        
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
                                                                              && position = Start(pawn.Color))
                                      |> List.map fst
                                      
            let opponentPiecesNotOnStartHomeOrSafety = boardPositions
                                                       |> Map.toList
                                                       |> List.filter (fun (pawn, position) -> pawn.Color <> activeColor && position |> isOuterSquare)
                                                       |> List.map fst
            
            (activePiecesOnStart, opponentPiecesNotOnStartHomeOrSafety)
            ||> List.allPairs
            |> List.map Action.Sorry
            
        let canSplitMove7WithAny2PiecesNotOnStart =
            let splits = [(1,6);(2,5);(3,4);(4,3);(5,2);(6,1)]
            
            let pawnsEligibleToMove = boardPositions
                                       |> Map.filter (fun pawn position -> pawn.Color = activeColor && position <> Start(pawn.Color))
                                       |> Map.toList
                                       |> List.map fst
            
            // Since known max pawns is 3 simpler to hard code all possibilities then come up with generic implementation
            let pieceSplits =
                match pawnsEligibleToMove with
                | [pawn1; pawn2; pawn3] -> [(pawn1, pawn2);(pawn2, pawn3)]
                | [pawn1; pawn2] -> [(pawn1, pawn2)]
                | _ -> []
                
            (pieceSplits, splits)
            ||> List.allPairs
            |> List.map (fun ((pawn1, pawn2),(move1, move2)) -> SplitMove7((pawn1,move1),(pawn2,move2)))

        let actions = match game.DrawnCard with
                      | Card.One ->canMoveAnyPieceOutOfStart@(canMoveAnyPieceNotOnStart 1)
                      | Card.Two ->canMoveAnyPieceOutOfStart@(canMoveAnyPieceNotOnStart 2)
                      | Card.Three -> canMoveAnyPieceNotOnStart 3
                      | Card.Four -> canMoveAnyPieceNotOnStart -4
                      | Card.Five -> canMoveAnyPieceNotOnStart 5
                      | Card.Seven -> (canMoveAnyPieceNotOnStart 7)@canSplitMove7WithAny2PiecesNotOnStart
                      | Card.Eight -> canMoveAnyPieceNotOnStart 8
                      | Card.Ten -> (canMoveAnyPieceNotOnStart 10)@(canMoveAnyPieceNotOnStart -1)
                      | Card.Eleven -> (canMoveAnyPieceNotOnStart 11)@canSwitchPlacesWithOpponentNotOnStartHomeOrSafety
                      | Card.Twelve -> canMoveAnyPieceNotOnStart 12
                      | Card.Sorry -> canMoveAnyPieceOnStartToBumpAnyPieceNotOnStartHomeOrSafety
                      
        match actions with
        | [] -> Ok([Action.PassTurn])
        | actions -> Ok(actions)
        
    | SettingUp _ -> Error(game, "Game is still in setup state")
    | _ -> Error(game, "Not implemented")
    
let getDrawnCard game = 
    match game with
    | ChoosingAction(game) -> Some(game.DrawnCard)
    | _ -> None
    
// Commands
let tryAddPlayer name color game =
    match game with
    | SettingUp(setupState) ->
        let addPlayerRules : ValidationRule<SetupState * Player> list =
            [ fun (setupState, player) ->
                let chosenColors = setupState |> getChosenColors
                not <| (chosenColors |> List.contains player.Color), "Can't choose a color that has already been chosen"]
            
        let addPlayerValidator = buildValidator addPlayerRules
        
        match (setupState, {Name=name;Color=color}) |> addPlayerValidator with
        | true, _ -> Ok(SettingUp({ Players = setupState.Players @ [{Name=name; Color=color}]}))
        | false, error -> Error(game, error)
    | _ -> Error(game, "Can only add players when game is in setup state")
    
/// <summary>
/// <para>
/// try startGame should be called when you are done adding players and configuring settings
/// and ready to begin the game.
/// </para>
/// <para>
/// It will return an error if the setup criteria has not been met
/// </para>
/// </summary>
let tryStartGame random game =
    match game with
    | SettingUp(setupState) ->
        let startGameRules : ValidationRule<SetupState> list =
            [ fun setupState -> setupState.Players.Length >=2, "Must have at least 2 players to start a game"]
            
        let startGameValidator = buildValidator startGameRules
        
        match setupState |> startGameValidator with
        | true, _ ->
            let activePlayer = random() % setupState.Players.Length
                
            let initializeTokenPositions (players:Player list) =
                players
                |> List.map (fun player ->
                    [{Color=player.Color;ID=PawnID.One}, BoardPosition.Start(player.Color)
                     {Color=player.Color;ID=PawnID.Two}, BoardPosition.Start(player.Color)
                     {Color=player.Color;ID=PawnID.Three}, BoardPosition.Start(player.Color)])
                |> List.reduce (fun colors1 colors2 -> colors1 @ colors2)
                |> Map.ofList
                
            let tokenPositions = initializeTokenPositions setupState.Players
            
            Ok(Drawing({Deck=newDeck;RandomNumberGenerator=random;Players=setupState.Players;TokenPositions=tokenPositions;ActivePlayer=setupState.Players.[activePlayer]}))
            
        | false, error -> Error(game, error)
    | _ -> Error(game, "Can only start a game that is still in setup state")
                 
let tryDrawCard game =
    match game with
    | Drawing(gameState) ->
        // for simplicity sake, rather than shuffle deck, draw random card each time
        let drawIndex = gameState.RandomNumberGenerator() % gameState.Deck.Length
        let topCut, bottomCut = gameState.Deck |> List.splitAt drawIndex
        let drawnCard = bottomCut.Head
        let newDeck = topCut @ bottomCut.Tail
        Ok(ChoosingAction({BoardState={gameState with Deck=newDeck};DrawnCard=drawnCard}))
    | _ -> Error(game, "Can only draw a card when game is in draw state")

let tryChooseAction action game =
    
    let updateActivePlayer gameState =
       assert(gameState.Players |> List.contains gameState.ActivePlayer)
       let currentIndex = gameState.Players |> List.findIndex (fun player -> player = gameState.ActivePlayer)
       let nextIndex = currentIndex + 1 % gameState.Players.Length
       {gameState with ActivePlayer=gameState.Players.[nextIndex]}
       
    // try move pawn or do we assume its legal
    // as we already found available moves???
    let movePawn (pawnToMove:Pawn) moveIncrement gameState =
       
       let currentPosition = gameState.TokenPositions.[pawnToMove]
       
       let newPosition = currentPosition |> positionAheadOfCurrentBy moveIncrement pawnToMove.Color
                         
       let sendOpponentBackToStartIfYouLandOnHim tokenPositions =
           let opponentToBump = tokenPositions |> Map.tryFindKey (fun _ position -> position = newPosition)
                                
           match opponentToBump with
           | Some(pawn) -> gameState.TokenPositions.Add(pawn, Start(pawn.Color))
           | None -> gameState.TokenPositions
           
       let slideIfPawnLandsOnSlideSquare boardPosition =
          match newPosition with
          | Outer(color, OuterCoordinate.Six) -> boardPosition
                                                 |> Map.add pawnToMove (Outer(color, OuterCoordinate.Ten))
                                                 // @TODO bump any pawn on slide region
          | Outer(color, OuterCoordinate.Thirteen) -> boardPosition
                                                      |> Map.add pawnToMove (Outer(color |> incrementColor 1, OuterCoordinate.One))
                                                      // @TODO bump any pawn on slide regioGreenGreenYellowkjkYou should not slide when you land on your own slide ou should not slide when you land on your own slide 3 4 Green, terCoordinate.Expected pawn to not slide and just move forward 1 SevenBluePawn2BluePawn2BlueYellowFourteen// @TODO - blue pawns should not be // your "own slide square is  3 quare is on the previous colors outer 13 pawn to not slide and just move forward one square to yell oe n
          | _ -> boardPosition
          
       let newBoardState =
           gameState.TokenPositions
           |> sendOpponentBackToStartIfYouLandOnHim
           |> Map.add pawnToMove newPosition
           |> slideIfPawnLandsOnSlideSquare
       
       {gameState with TokenPositions=newBoardState}
       
    let switchPawns pawn1 pawn2 (gameState:BoardState) =
        let pawn1Pos = gameState.TokenPositions.[pawn1]
        let pawn2Pos = gameState.TokenPositions.[pawn2]
        let newTokenPositions = gameState.TokenPositions
                                |> Map.add pawn1 pawn2Pos
                                |> Map.add pawn2 pawn1Pos
                                
        {gameState with TokenPositions=newTokenPositions}
       
    let sorry pawnOnStart pawnToBump (gameState:BoardState) =
        assert(gameState.TokenPositions.[pawnOnStart] = Start(pawnOnStart.Color))
        let pawnToBumpPos = gameState.TokenPositions.[pawnToBump]
        let newTokenPositions = gameState.TokenPositions
                                |> Map.add pawnOnStart pawnToBumpPos
                                |> Map.add pawnToBump (Start(pawnToBump.Color))
        
        {gameState with TokenPositions=newTokenPositions}
        
    result {
        let! availableActions = game |> getAvailableActions
        
        if availableActions |> List.contains action then
            // Since we have validated that it is an available action we can
            // now assume all actions are valid below and just update without worry
            // of it putting game in invalid state
            return! match game with
                    | Drawing(_) ->
                        match action with
                        | DrawCard -> game |> tryDrawCard
                        | _ -> Error(game, "Can only draw card when game is in draw state")
                    | ChoosingAction(gameState) ->
                        match action with
                        | MovePawn(pawn,moveIncrement) ->
                            let newBoardState = gameState.BoardState |> movePawn pawn moveIncrement
                            match gameState.DrawnCard with
                            | Card.Two -> Ok(Drawing(newBoardState))
                            | _ -> Ok(Drawing(newBoardState |> updateActivePlayer))
                        | SplitMove7((pawn1, move1),(pawn2, move2)) ->
                            let newBoardState = gameState.BoardState
                                                |> movePawn pawn1 move1
                                                |> movePawn pawn2 move2
                            Ok(Drawing(newBoardState |> updateActivePlayer))
                        | SwitchPawns(pawn1,pawn2) ->
                            let newBoardState = gameState.BoardState |> switchPawns pawn1 pawn2
                            Ok(Drawing(newBoardState |> updateActivePlayer))
                        | Sorry(pawn1, pawn2) ->
                            let newBoardState = gameState.BoardState |> sorry pawn1 pawn2
                            Ok(Drawing(newBoardState |> updateActivePlayer))
                        | PassTurn -> Ok(Drawing(gameState.BoardState |> updateActivePlayer))
                        | _ -> Error(game, "Unimplemented")
                    | _ -> Error(game, "Unimplemented")
        else            
            return! Error(game, "Choosing Invalid Action")
    }
