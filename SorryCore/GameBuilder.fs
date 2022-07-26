module Sorry.Core.GameBuilder

open FSharp.Core.Extensions
open FSharp.Core.Extensions.Validation

type T = {
   Players: Player list    
}

/// Initializes a new game builder with no players added
let newGame = {Players = []}

/////////////////////////////////////////////////////////////////////////////////////
// Queries
/////////////////////////////////////////////////////////////////////////////////////

/// getChosenColors returns a list of the colors that have already been chosen
let getChosenColors builder = builder.Players |> List.map (fun player -> player.Color)

/// getAvailableColors returns the available colors left to choose from
let getAvailableColors builder =
    let allColors = FSharpType.getAllEnumsValues<Color>()
    let chosenColors = builder |> getChosenColors
    List.distinct allColors chosenColors


/////////////////////////////////////////////////////////////////////////////////////
// Commands
/////////////////////////////////////////////////////////////////////////////////////

let tryAddPlayer name color builder =
    let addPlayerRules : ValidationRule<T * Player> list =
        [ fun (setupState, player) ->
            let chosenColors = setupState |> getChosenColors
            not <| (chosenColors |> List.contains player.Color), "Can't choose a color that has already been chosen" ]
        
    let addPlayerValidator = buildValidator addPlayerRules
    
    let newPlayer = {Name=name;Color=color}
        
    match (builder, newPlayer) |> addPlayerValidator with
    | true, _ -> Ok({ Players = newPlayer::builder.Players})
    | false, error -> Error(builder, error)
        

/// <summary>
/// <para>
/// tryStartGame should be called when you are done adding players and configuring settings
/// and ready to begin the game.
/// </para>
/// <para>
/// It will return an error if there are not between 2 to 4 players
/// </para>
/// <param name="random">A function with no input that returns a new random integer each time it is invoked
/// (for things like shuffling deck or selecting starting player)</param>
/// </summary>
let tryStartGame random builder =
    let startGameRules : ValidationRule<T> list =
        [ fun setupState -> setupState.Players.Length >=2, "Must have at least 2 players to start a game"]
        
    let startGameValidator = buildValidator startGameRules
    
    match builder |> startGameValidator with
    | true, _ ->
        let initializeTokenPositions (players:Player list) =
            
            let getStartingTokenPositions player = 
                FSharpType.getAllUnionCases<PawnID>()
                |> List.map (fun pawnID -> {Color=player.Color;ID=pawnID}, BoardPosition.Start)
                
            players
            |> List.collect getStartingTokenPositions
            |> Map.ofList
            
        let tokenPositions = initializeTokenPositions builder.Players
        let activePlayer = random() % builder.Players.Length
        
        Ok(DrawingCard({Deck=newDeck;Players=builder.Players;TokenPositions=tokenPositions;ActivePlayer=builder.Players.[activePlayer]}))
        
    | false, error -> Error(builder, error)
