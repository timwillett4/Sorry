module Sorry.Core.GameState

open FSharp.Core.Extensions
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
    let canMoveAnyPieceOutOfStart activeColor boardPositions =
       boardPositions
       |> Map.toList
       |> List.filter (fun ((color, _), position) -> color = activeColor && position = BoardPosition.Start(color))
       |> List.map (fun ((color, pawnID), _) -> Action.MovePawn(color, pawnID, 1))
       
    match game with
    | Drawing _ -> Ok([Action.DrawCard])
    | ChoosingAction(game) ->
        let activeColor = game.GameState.ActivePlayer.Color
        let boardPositions = game.GameState.TokenPositions
       
        match game.DrawnCard with
        | Card.One ->
            // @TODO - find more elequent way to build rules
            Ok(canMoveAnyPieceOutOfStart activeColor boardPositions)
        | Card.Two ->
            Ok(canMoveAnyPieceOutOfStart activeColor boardPositions)
        | _ ->
            Ok([Action.PassTurn])
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
    match game with
    | Drawing(gameState) ->
        match action with
        | DrawCard -> game |> tryDrawCard
        | _ -> Error(game, "Illegal action")
    | _ -> Error(game, "Unimplemented")
