module Sorry.Core.GameState

open FSharp.Core.Extensions
open Sorry.Core

/// The Sorry Game State consists


/// <summary>
/// New came starts in setup state with no players
/// Players can be added with <see cref="addPlayers">addPlayers</see>
/// </summary>
let newGame = SettingUp({Players=[]})

// Commands
let addPlayer name color game =
    match game with
    | SettingUp(game) -> SettingUp({ Players = game.Players @ [{Name=name; Color=color}]})
    | _ -> game // @TODO - return result error here, eventually need to catch invalid adds in setup state as well
     

// Querries
/// getAvailableColors returns the available colors left to choose from
let getAvailableColors game =
    match game with
    | SettingUp(game) ->
        let chosenColors = game.Players |> List.map (fun player -> player.Color)
        let allColors = FSharpType.getAllEnumsValues<Color>()
        List.distinct allColors chosenColors
    | _ -> []