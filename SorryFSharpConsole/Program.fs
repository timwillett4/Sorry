// Learn more about F# at http://fsharp.org
open System
open Sorry.Core
open FSharp.Core.Extensions.Result
open Sorry.Core.Display

[<EntryPoint>]
let main argv =
    let deleteChar() = Console.Write("\b \b")
    let readChar() =
        let char = Console.ReadKey().KeyChar
        Console.WriteLine()
        char
        
    let readLine() = Console.ReadLine()
    
    let game = GameBuilder.newGame
    
    let rec getNumberOfPlayers readChar =
        match readChar() with
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | _ ->
            deleteChar()
            getNumberOfPlayers readChar
            
    printf "Select number of players (2-4): "
    let nPlayers = getNumberOfPlayers readChar
    
    let rec chooseColor readChar =
        match readChar() |> Char.ToUpper with
        | 'R' -> Color.Red
        | 'Y' -> Color.Yellow
        | 'B' -> Color.Blue
        | 'G' -> Color.Green
        | _ ->
            deleteChar()
            chooseColor readChar
        
    let rec addPlayer game playerIndex =
        printf "Enter name for player %i:" <| playerIndex
        let playerName = readLine()
        
        printf "Choose color %i:" <| playerIndex
        let color = chooseColor readChar
        printfn ""
    
        match game |> GameBuilder.tryAddPlayer playerName color with
        | Ok(game) -> game
        | Error(gameSate, error:string) ->
            printfn "%A" <|error
            addPlayer game playerIndex
        
    let rec getActionChoice readChar (maxActions:int) =
        // @TODO - this is not going to work when there are more than 10 options
        printf "Choose Action: "
        match (readChar() |> int) - ('0' |> int) |> int with
        | actionChoice when actionChoice >= 0 && actionChoice < maxActions -> actionChoice
        | _ ->
            deleteChar()
            getActionChoice readChar maxActions
        
    let game = [ 1..nPlayers ] |> List.fold addPlayer game
    
    let random = fun () -> Random(DateTime.Now.Millisecond).Next()
    
    let game = game |> GameBuilder.tryStartGame random
    
    let rec gameLoop game = 
        result {
            
            let availableActions = game |> GameState.getAvailableActions
            (printBoardState game) |> printf "%s"
            let actionChoice = getActionChoice readChar availableActions.Length
           
            let log game action = () 
            let! game = game |> GameState.tryChooseAction random log availableActions.[actionChoice]
            // @TODO - check win condition (match game state end on game over recurse on others?)
            
            return! gameLoop game
    }
    
    match game with
    | Ok(game) -> game |> gameLoop |> ignore
    | Error(_, error) -> printf "%s" <| error
    
    0 // return an integer exit code 