module Sorry.Core.GameState

open Sorry.Core.BoardPosition
open FSharp.Core.Extensions.Result

/// The Sorry Game State consists


/// <summary>New came starts in setup state with no players
/// Players can be added with <see 
let newGame = SettingUp({Players=[]})

//let addPlayer game name color = 
     
//let getAvailableColors