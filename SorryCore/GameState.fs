﻿module Sorry.Core.GameState

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
let getChosenColors game = game.Players |> List.map (fun player -> player.Color)

/// getAvailableColors returns the available colors left to choose from
let getAvailableColors game =
    match game with
    | SettingUp(game) ->
        let allColors = FSharpType.getAllEnumsValues<Color>()
        let chosenColors = game |> getChosenColors
        List.distinct allColors chosenColors
    | _ -> []
    
// Commands
let tryAddPlayer name color game =
    match game with
    | SettingUp(setupState) ->
        let addPlayerRules : ValidationRule<SetupState * Player> list =
            [ fun (setupState, player) ->
                let chosenColors = setupState |> getChosenColors
                not <| (chosenColors |> List.contains player.Color), sprintf"%A has already been chosen" <| color]
            
        let addPlayerValidator = buildValidator addPlayerRules
        
        match (setupState, {Name=name;Color=color}) |> addPlayerValidator with
        | true, _ -> Ok(SettingUp({ Players = setupState.Players @ [{Name=name; Color=color}]}))
        | false, error -> Error(game, error)
    | _ -> Error(game, "Can only add players when game is in setup state")
    