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
    ]