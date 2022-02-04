module Sorry.Core.Tests.CreateGameTests

open Sorry.Core
open FSharp.Core.Extensions
open Expecto

[<Tests>]
let createGameTests =

    let allColors = FSharpType.getAllEnumsValues<Color>()
    let tokenIDs = FSharpType.getAllUnionCases<PawnID>()

    testList "Create New Game Tests" [

        test "New game should initialize in setting up state with no players added" {
            let game = GameState.newGame
            match game with
            | SettingUp setupState -> Expect.isEmpty setupState.Players "Expected there to be no players on initialization"
            | _ -> failwith "Expected game to be in setting up state"
        }
        
        test "All colors should be available when no players have been added" {
            let colors = GameState.newGame |> GameState.getAvailableColors 
            Expect.equal colors.Length 4 "Expect all four colors to be available"
        }
        
        test "After the first player chooses red there should be 3 colors available" {
            let colors =
                GameState.newGame
                |> GameState.addPlayer "Levi" Color.Red
                |> GameState.getAvailableColors
            Expect.equal colors.Length 3 "Expect 3 colors to be available"
        }
    ]