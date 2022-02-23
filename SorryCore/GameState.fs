module Sorry.Core.GameState

open FSharp.Core.Extensions
open FSharp.Core.Extensions.Validation
open Sorry.Core

/// The Sorry Game State consists


/// <summary>
/// New came starts in setup state with no players
/// Players can be added with <see cref="addPlayers">addPlayers</see>
/// </summary>
let newGame = SettingUp({Players=[]})

// Querries

/// getChosenColors returns a list of the colors that have already been chosen
let private getChosenColors (game:SetupState) = game.Players |> List.map (fun player -> player.Color)

/// getAvailableColors returns the available colors left to choose from
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
    | SettingUp(_) -> Error(game, "Game is still in setup state")
    | _ -> Error(game, "Not implemented")
    
let getPlayers game = 
    match game with
    | Drawing(gameState) -> Ok(gameState.Players)
    | SettingUp(_) -> Error(game, "Game is still in setup state")
    | _ -> Error(game, "Not implemented")
    
let getActivePlayer game = 
    match game with
    | Drawing(gameState) -> Ok(gameState.ActivePlayer)
    | SettingUp(_) -> Error(game, "Game is still in setup state")
    | _ -> Error(game, "Not implemented")
    
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
    
/// startGame should be called when you are done adding players and configuring settings
/// and ready to begin the game.
let startGame random game =
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
                 
                 