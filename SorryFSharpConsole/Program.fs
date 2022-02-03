// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let deleteChar() = Console.Write("\b \b")
    let readChar() = Console.ReadKey().KeyChar
    let readLine() = Console.ReadLine()
    
    let rec getNumberOfPlayers readChar =
        match readChar() with
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | _ ->
            deleteChar()
            getNumberOfPlayers readChar
            
    Console.Write "Select number of players (2-4): "
    let nPlayers = getNumberOfPlayers readChar
    Console.Clear()
    
    //let getPlayerInfo player = readLine
        
    let playerNames =
        seq { for i in 1..nPlayers -> i }
        |> Seq.map (fun player ->
            Console.Write(sprintf "Enter name for player %i:" <| player)
            let playerName = readLine()
            Console.Write(sprintf "Choose color %i:" <| player)
            Console.Clear()
            playerName
        )
        |> Seq.toList
       
    0 // return an integer exit code 