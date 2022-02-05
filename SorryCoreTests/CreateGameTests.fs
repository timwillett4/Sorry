module Sorry.Core.Tests.CreateGameTests

open Sorry.Core
open FSharp.Core.Extensions
open FSharp.Core.Extensions.Result
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
            match GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red with
            | Ok(game) ->
                let availableColors = game |> GameState.getAvailableColors
                Expect.equal availableColors.Length 3 "Expect 3 colors to be available"
            | Error(_) -> failtest "Unexpected error adding player"
        }
        
        test "After the first player chooses red, red should no longer be available" {
            match GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red with
            | Ok(game) ->
                let availableColors = game |> GameState.getAvailableColors
                Expect.equal (availableColors |> List.contains Color.Red) false "Expect red to not be one of available colors"
            | Error(_) -> failtest "Unexpected error adding player"
        }
        
        test "Choosing a color that was already take should return an error" {
            let gameState = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Corbin" Color.Red
                return game
            }
                
            Expect.isError gameState "Expect error to be returned when a color is chosen that was already taken"
        }
        
        test "Adding a player when game is not in setup state should return an error" {
            let gameState = result {
                let game = GameOver({Winner={Name="Levi";Color=Color.Red}})
                let! game = game |> GameState.tryAddPlayer "Corbin" Color.Red
                return game
            }
                
            Expect.isError gameState "Expect error to be returned when game is not in setting up state"
        }
        // @TODO - test that error is reported if name doesn't match validation rules (use validation rule builder)
    ]