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
    let canMoveAnyPiece predicate activeColor boardPositions spaces =
        boardPositions
        |> Map.toList
        |> List.filter (fun ((color, _), position) -> (color, position) ||> predicate)
        |> List.map (fun ((color, pawnID), _) -> Action.MovePawn(color, pawnID, spaces))
       
    match game with
    | Drawing _ -> Ok([Action.DrawCard])
    | ChoosingAction(game) ->
        let activeColor = game.GameState.ActivePlayer.Color
        let boardPositions = game.GameState.TokenPositions
       
        let canMoveAnyPieceOutOfStart = canMoveAnyPiece
                                            (fun color position -> color = activeColor && position = Start(activeColor))
                                            activeColor
                                            boardPositions
                                            1
                                            
        let canMoveAnyPieceNotOnStart = canMoveAnyPiece
                                            (fun color position -> color = activeColor && position <> Start(activeColor))
                                            activeColor
                                            boardPositions
                                           
        
        let actions = match game.DrawnCard with
                      | Card.One ->canMoveAnyPieceOutOfStart@(canMoveAnyPieceNotOnStart 1)
                      | Card.Two ->canMoveAnyPieceOutOfStart@(canMoveAnyPieceNotOnStart 2)
                      | Card.Three -> canMoveAnyPieceNotOnStart 3
                      | Card.Four -> canMoveAnyPieceNotOnStart -4
                      | Card.Five -> canMoveAnyPieceNotOnStart 5
                      | _ -> []
                      
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
                    [(player.Color, PawnID.One), BoardPosition.Start(player.Color)
                     (player.Color, PawnID.Two), BoardPosition.Start(player.Color)
                     (player.Color, PawnID.Three), BoardPosition.Start(player.Color)])
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
        Ok(ChoosingAction({GameState={gameState with Deck=newDeck};DrawnCard=drawnCard}))
    | _ -> Error(game, "Can only draw a card when game is in draw state")

let tryChooseAction action game =
    
    let updateActivePlayer gameState =
       assert(gameState.Players |> List.contains gameState.ActivePlayer)
       let currentIndex = gameState.Players |> List.findIndex (fun player -> player = gameState.ActivePlayer)
       let nextIndex = currentIndex + 1 % gameState.Players.Length
       {gameState with ActivePlayer=gameState.Players.[nextIndex]}
       
    // try move pawn or do we assume its legal
    // as we already found available moves???
    let movePawn color pawnID moveIncrement gameState =
       
       // @TODO - add to core extensions math
       let wrap max n = (n + max) % max
       let nColors = 4
       let nSpacePerColor = 15
       
       // Converts to a 0(start) to 66(home) representing
       // local coordinates for particular color
       let toLocal localColor boardPosition =
           match boardPosition with
           | Start(color) when color = localColor -> 0
           | Home(color) when color = localColor -> 66
           | Safety(color, safetySquare) when color = localColor ->
               (safetySquare |> int) + 60
           | Outer(color, coord) ->
               // calculate how many colors away from the local color we are
               // Example: Colors are in order Green->Red->Blue->Yellow
               let colorDist = (color |> int) - (localColor |> int) |> wrap nColors
               (colorDist * nSpacePerColor) + (coord |> int)
           | _ ->
               failwith $"Local Position must be between 0 - 66"

       let toBoardPosition localColor localPosition =
           match localPosition with
           | start when localPosition = 0 -> Start(localColor)
           | outer when localPosition >= 1 && localPosition <= 60 ->
               let colorDiff = localPosition / nSpacePerColor
               let outerCoord = localPosition - (colorDiff * nSpacePerColor)
               let color = ((color |> int) + colorDiff) % nColors |> enum<Color>
               Outer(color, outerCoord |> enum)
           // This occurs when you get backward and are near opening sqaure
           | outer when localPosition < 0 && localPosition >= -3 ->
               let colorInt = ((localColor |> int) - 1) |> wrap nColors
               let color = colorInt |> enum
               Outer(color, nSpacePerColor + localPosition |> enum)
           | safety when localPosition >= 61 && localPosition <= 65 ->
               Safety(localColor, (localPosition - 60) |> enum)
           | home when localPosition = 66 -> Home(color)
           | _ -> failwith $"Invalid board position"
               
       let currentPosition = gameState.TokenPositions.[color,pawnID]
       let newPosition =
           match currentPosition with 
           | Start(color) ->
               assert(moveIncrement = 1)
               Outer(color, OuterCoordinate.One)
           | position ->
               let local = position |> toLocal color
               let newLocal = local + moveIncrement
               let newBoardPosition = newLocal |> toBoardPosition color
               ((position |> toLocal color) + moveIncrement) |> toBoardPosition color
       let newBoardState = gameState.TokenPositions.Add ((color, pawnID), newPosition)
       {gameState with TokenPositions=newBoardState}
       
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
                        | MovePawn(color,pawnID,moveIncrement) ->
                            let newGameState = gameState.GameState |> movePawn color pawnID moveIncrement
                            match gameState.DrawnCard with
                            | Card.Two -> Ok(Drawing(newGameState))
                            | _ -> Ok(Drawing(newGameState |> updateActivePlayer))
                        | PassTurn ->
                        Ok(Drawing(gameState.GameState |> updateActivePlayer))
                        | _ -> Error(game, "Unimplemented")
                    | _ -> Error(game, "Unimplemented")
        else            
            return! Error(game, "Choosing Invalid Action")
    }
