#r "../../../FSharp.Core.Extensions/bin/Release/netstandard2.0/FSharp.Core.Extensions.dll"
#r "../bin/Release/netstandard2.0/SorryCore.dll"
#r "../../packages/Expecto/lib/netstandard2.0/Expecto.dll"

open Sorry.Core

open Expecto


[<Tests>]
let createGameTests =

    testList "Create New Game Tests" [

        testList "Must have between 2 to 4 players to create a game" [

            test "Creating a game with 0 players returns an error" {
                let game = [] |> Map.ofList |> GameState.newGame
                Expect.isError game "no players"}

            test "Creating a game with one player returns an error" {
                 let game = [Color.Yellow, "Tim"] |> Map.ofList |> GameState.newGame
                 Expect.isError game ""}

            test "Creating a game with 2 players returns a new game state" {
                let game = [Color.Blue,"Corbin"; Color.Green,"Levi"] |> Map.ofList |> GameState.newGame
                Expect.isOk game ""}

            test "Creating a game with 3 players returns a new game state" {
                let game = [Color.Blue,"Corbin"; Color.Green,"Levi"; Color.Yellow,"Micah"] |> Map.ofList |> GameState.newGame
                Expect.isOk game ""}

            test "Creating a game with 4 players returns a new game state" {
                let game = [Color.Blue,"Corbin"; Color.Green,"Levi"; Color.Yellow, "Micah"; Color.Red,"BabyB"] |> Map.ofList |> GameState.newGame
                Expect.isOk game ""}]

    
        testList "All players should start on their home space" [

            // @TODO - bad test as empty positions is passing
            // ok to keep but should write test that fails when positions are empty first
            //test "Creating a game with 4 players returns a new game state" {
            //    let game = [Color.Blue,"Corbin"; Color.Green,"Levi"; Color.Yellow, "Micah"; Color.Red,"BabyB"] |> Map.ofList |> GameState.newGame    
            //    let allOnHomeSquare =
            //        match game with
            //        | Ok game -> 
            //            game |> Map.forall (fun color tokens -> tokens |> Map.forall (fun _ pos -> pos = Home color))
            //        | Error e -> 
            //            false
            //    Expect.isTrue allOnHomeSquare ""
            //}
        ]
    ]

createGameTests |> runTests defaultConfig