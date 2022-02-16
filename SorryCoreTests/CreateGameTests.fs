module Sorry.Core.Tests.CreateGameTests

open Sorry.Core
open FSharp.Core.Extensions
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let addPlayerTests =

    testList "Add new player tests" [

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
                
            Expect.isError gameState "Expect error to be returned when try to add a player to a game that is not in setting up state"
        }
        // @TODO - test that error is reported if name doesn't match validation rules (use validation rule builder)
    ]

[<Tests>]
let startGameTests =  
    testList "Start Game Tests" [
        test "Starting a game when game is not in setup state should return an error" {
            let gameState = result {
                let game = GameOver({Winner={Name="Levi";Color=Color.Red}})
                let! game = game |> GameState.startGame
                return game
            }
                
            Expect.isError gameState "Expect error to be returned starting a game that is not in setting up state"
        }
        
        test "Starting a game with only 1 player should return an error" {
            let gameState = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.startGame
                return game
            }
                
            Expect.isError gameState "Expect error to be returned when a game is started with only 1 player"
        }
        
        test "Starting a game with 2 player should transition game to draw state" {
            let gameState = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.startGame
                
                return game
            }
                
            match gameState with
            | Ok(Drawing _) -> ()
            | _ -> failtest "Expected game to transition to drawing state"
        }
        
        // @TODO - add tests for verifying board setup
        test "A new game should have 45 cards in the deck" {
            let gameState = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.startGame
                
                return game
            }
                
            match gameState with
            | Ok(Drawing gameState) -> Expect.equal gameState.Deck.Length 45 "Expected game to contain 45 cards"
            | _ -> failtest "Expected game to transition to draw state"
        }
    ]
    