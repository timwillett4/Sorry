// Learn more about F# at http://fsharp.org
open System
open Sorry.Core
open FSharp.Core.Extensions.Result
open SorryFSharpConsole.Display

[<EntryPoint>]
let main argv =
    let deleteChar() = Console.Write("\b \b")
    let readChar() =
        let char = Console.ReadKey().KeyChar
        Console.WriteLine()
        char
        
    let readLine() = Console.ReadLine()
    
    let game = GameState.newGame
    
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
    
        match game |> GameState.tryAddPlayer playerName color with
        | Ok(game) -> game
        | Error(gameSate, error:string) ->
            printfn "%A" <|error
            addPlayer game playerIndex
        
    let game = seq { for i in 1..nPlayers -> i } |> Seq.fold addPlayer game
    
    let r = result {
        let! game = game |> GameState.tryStartGame (fun () -> Random(DateTime.Now.Millisecond).Next())
        let! tokenPositions = game |> GameState.getTokenPositions
        
        tokenPositions |> drawBoard
        
        return game
    }
    
    0 // return an integer exit code 