module Sorry.Core.Tests.CreateGameTests

open Sorry.Core
open Sorry.Core.GameBuilder
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let startGameTests =
    
    let levi = {Color=Color.Green; Name = "Levi"}
    let corbin = {Color=Color.Yellow; Name = "Corbin"}
    let micah = {Color=Color.Blue; Name = "Micah"}
    
    let random1 = fun () -> 1
    let tryStartGame = GameBuilder.tryStartGame random1
    
    testList "Setup Tests" [
        
        test "A new game should have no players" {
            let game = GameBuilder.newGame
            
            Expect.equal game.Players [] "Expected player list to be empty"
        }
        
        test "All players must choose different colors" {
            let gameState = result {
                let! game = newGame |> tryAddPlayer "Levi" Color.Red
                let! game = game |> GameBuilder.tryAddPlayer "Corbin" Color.Red
                return game
            }
                
            Expect.isError gameState "Expect error to be returned when a color is chosen that was already taken"
        }
        
        testList "getAvailableColors should return the colors that have not yet been chosen" [

            test "All colors should be available when no players have been added" {
                let colors = GameBuilder.newGame |> GameBuilder.getAvailableColors 
                Expect.equal colors.Length 4 "Expect all four colors to be available"
            }
                
            test "After the first player chooses red there should be 3 colors available" {
                match GameBuilder.newGame |> GameBuilder.tryAddPlayer "Levi" Color.Red with
                | Ok(game) ->
                    let availableColors = game |> GameBuilder.getAvailableColors
                    Expect.equal availableColors.Length 3 "Expect 3 colors to be available"
                | Error(_) -> failtest "Unexpected error adding player"
            }
                
            test "After the first player chooses red, red should no longer be available" {
                  match GameBuilder.newGame |> GameBuilder.tryAddPlayer "Levi" Color.Red with
                  | Ok(game) ->
                      let availableColors = game |> GameBuilder.getAvailableColors
                      Expect.equal (availableColors |> List.contains Color.Red) false "Expect red to not be one of available colors"
                  | Error(_) -> failtest "Unexpected error adding player"
            }
        ]
        
        testList "To start a game there must be between 2 to 4 players" [
            
            test "Starting a game with no player should return an error" {
                
                let gameState = {Players = [levi]} |> tryStartGame
                    
                Expect.isError gameState "Expect error to be returned when a game is started with only 1 player"
            }
            
            test "Starting a game with only 1 player should return an error" {
                
                let gameState = {Players = [levi]} |> tryStartGame
                    
                Expect.isError gameState "Expect error to be returned when a game is started with only 1 player"
            }
            
            test "Starting a game with 2 players should succeed" {
                let gameState = {Players = [levi;corbin]} |> tryStartGame
                
                Expect.isOk gameState "Expected start game to succeed with 4 players"
            }
        ]            
        
        test "A new game should start with 45 cards in the deck" {
            let numCardsInDeck = result {
                let! gameState = {Players = [levi;corbin]} |> tryStartGame
                return gameState |> GameState.getNumCardsLeft
            }
            
            Expect.equal numCardsInDeck (Ok(45)) "Expected new game to contain 45 cards"
        }
        
        test "There should be 4 tokens of each color" {
            let are4ofEachColor = result {
                let! gameState = {Players = [levi;corbin]} |> tryStartGame
                let tokens = gameState |> GameState.getTokenPositions
                
                let countColor color game =
                    game
                    |> Map.filter (fun (pawn:Pawn) _ -> pawn.Color = color)
                    |> Map.count 
                
                return (tokens |> countColor Color.Green) = 4 && (tokens |> countColor Color.Yellow) = 4
            }
            
            Expect.equal are4ofEachColor (Ok(true)) "Expected 4 of each color"
        }
        
        test "All pieces should start on their start square" {
            let allPiecesOnHomeSquare = result {
                let! gameState = {Players = [levi;corbin]} |> tryStartGame
                let tokens = gameState |> GameState.getTokenPositions
                    
                return tokens |> Map.forall (fun _ position -> position = BoardPosition.Start)
            }
            
            Expect.equal allPiecesOnHomeSquare (Ok(true)) "Expected all pieces to start on their start square"
        } 
        
        test "When a game is started, the players list should match those added in setup state" {
            let players = result {
                let! gameState = {Players = [levi;corbin]} |> tryStartGame
                
                // @TODO - convert getPlayers to not return result
                return gameState |> GameState.getPlayers
            }
            
            Expect.equal players (Ok([levi;corbin])) "Expected players list to match those added in setup state"
        }
        
        test "When a game is started, the active player should be chosen according to the random number method" {
            let fakeRandomNumberGenerator0() = 0
            let fakeRandomNumberGenerator1() = 1
            let fakeRandomNumberGenerator2() = 2
            
            let activePlayers = result {
                let gameSetup = {Players = [levi;corbin;micah]}
                
                let! game1 = gameSetup |> GameBuilder.tryStartGame fakeRandomNumberGenerator0
                let! game2 = gameSetup |> GameBuilder.tryStartGame fakeRandomNumberGenerator1
                let! game3 = gameSetup |> GameBuilder.tryStartGame fakeRandomNumberGenerator2
                
                let activePlayer1 = game1 |> GameState.getActivePlayer
                let activePlayer2 = game2 |> GameState.getActivePlayer
                let activePlayer3 = game3 |> GameState.getActivePlayer
                
                return [activePlayer1;activePlayer2;activePlayer3]
            }
            
            Expect.equal activePlayers (Ok([levi;corbin;micah])) "Expected active player to be chosen by random number"
        }
    ]
