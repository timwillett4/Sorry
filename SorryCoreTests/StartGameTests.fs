module Sorry.Core.Tests.CreateGameTests

open Sorry.Core
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let startGameTests =
    
    let levi = {Color=Color.Green; Name = "Levi"}
    let corbin = {Color=Color.Yellow; Name = "Corbin"}
    let micah = {Color=Color.Blue; Name = "Micah"}
    
    let random1 = fun () -> 1
    let tryStartGame = GameState.tryStartGame random1
    
    testList "Start Game Tests" [
        
        test "Starting a game when a game is in the game over state should return an error" {
            let gameState = GameOver({Winner=levi}) |> tryStartGame
            
            Expect.isError gameState "Expect error to be returned starting a game that is not in setting up state"
        }
        
        test "Starting a game with only 1 player should return an error" {
            
            let gameState = SettingUp({Players = [levi]}) |> tryStartGame
                
            Expect.isError gameState "Expect error to be returned when a game is started with only 1 player"
        }
        
        test "Starting a game with 2 or more players should transition the game to draw state" {
            let gameState = SettingUp({Players = [levi;corbin]}) |> tryStartGame
            
            match gameState with
            | Ok(Drawing _) -> ()
            | _ -> failtest "Expected game to transition to drawing state"
        }
        
        test "A new game should have 45 cards in the deck" {
            let numCardsInDeck = result {
                let! gameState = SettingUp({Players = [levi;corbin]}) |> tryStartGame
                
                return! match gameState with
                        | Drawing gameState -> Ok gameState.Deck.Length
                        | _ -> Error (gameState, "Expected game to transition to draw state")
            }
            
            Expect.equal numCardsInDeck (Ok(45)) "Expected game to contain 45 cards"
        }
        
        test "There should be 3 tokens of each color" {
            let are3ofEachColor = result {
                let! gameState = SettingUp({Players = [levi;corbin]}) |> tryStartGame
                let! tokens = gameState |> GameState.getTokenPositions
                
                let countColor color game =
                    game
                    |> Map.filter (fun (pawn:Pawn) _ -> pawn.Color = color)
                    |> Map.count 
                
                return (tokens |> countColor Color.Green) = 3 && (tokens |> countColor Color.Yellow) = 3
            }
            
            Expect.equal are3ofEachColor (Ok(true)) "Expected 3 of each color"
        }
        
        test "All pieces should start on their start square" {
            let allPiecesOnHomeSquare = result {
                let! gameState = SettingUp({Players = [levi;corbin]}) |> tryStartGame
                let! tokens = gameState |> GameState.getTokenPositions
                    
                return tokens |> Map.forall (fun _ position -> position = BoardPosition.Start)
            }
            
            Expect.equal allPiecesOnHomeSquare (Ok(true)) "Expected all pieces to start on their start square"
        } 
        
        test "When a game is started, the players list should match those added in setup state" {
            let players = result {
                let! gameState = SettingUp({Players = [levi;corbin]}) |> tryStartGame
                
                return! gameState |> GameState.getPlayers
            }
            
            Expect.equal players (Ok([levi;corbin])) "Expected players list to match those added in setup state"
        }
        
        test "When a game is started, the active player should be chosen according to the random number method" {
            let fakeRandomNumberGenerator0() = 0
            let fakeRandomNumberGenerator1() = 1
            let fakeRandomNumberGenerator2() = 2
            
            let activePlayers = result {
                let gameSetup = SettingUp({Players = [levi;corbin;micah]})
                
                let! game1 = gameSetup |> GameState.tryStartGame fakeRandomNumberGenerator0
                let! game2 = gameSetup |> GameState.tryStartGame fakeRandomNumberGenerator1
                let! game3 = gameSetup |> GameState.tryStartGame fakeRandomNumberGenerator2
                
                let! activePlayer1 = game1 |> GameState.getActivePlayer
                let! activePlayer2 = game2 |> GameState.getActivePlayer
                let! activePlayer3 = game3 |> GameState.getActivePlayer
                
                return [activePlayer1;activePlayer2;activePlayer3]
            }
            
            Expect.equal activePlayers (Ok([levi;corbin;micah])) "Expected active player to be chosen by random number"
        }
    ]
    