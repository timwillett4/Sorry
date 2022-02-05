module Sorry.Core.GameState

open FSharp.Core.Extensions
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
        let chosenColors = setupState |> getChosenColors
        // @TODO create validation rules builder
        match chosenColors |> List.contains color with
        | false -> Ok(SettingUp({ Players = setupState.Players @ [{Name=name; Color=color}]}))
        | true -> Error(game, sprintf"%A has already been chosen" <| color)
    | _ -> Error(game, "Can only add players when game is in setup state") 
