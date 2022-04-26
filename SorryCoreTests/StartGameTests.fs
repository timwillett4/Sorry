module Sorry.Core.Tests.CreateGameTests

open Sorry.Core
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let startGameTests =  
    testList "Start Game Tests" [
        
        test "Starting a game when a game is in the game over state should return an error" {
            let gameState = result {
                let game = GameOver({Winner={Name="Levi";Color=Color.Red}})
                let! game = game |> GameState.tryStartGame (fun () -> 1)
                return game
            }
                
            Expect.isError gameState "Expect error to be returned starting a game that is not in setting up state"
        }
        
        test "Starting a game with only 1 player should return an error" {
            
            let gameState = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryStartGame (fun () -> 1)
                return game
            }
                
            Expect.isError gameState "Expect error to be returned when a game is started with only 1 player"
        }
        
        
        test "Starting a game with 2 or more players should transition the game to draw state" {
            let gameState = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.tryStartGame (fun () -> 1)
                
                return game
            }
            
            match gameState with
            | Ok(Drawing _) -> ()
            | _ -> failtest "Expected game to transition to drawing state"
        }
        
        test "A new game should have 45 cards in the deck" {
            let numCardsInDeck = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.tryStartGame (fun () -> 1)
                
                let numCards =
                    match game with
                    | Drawing gameState -> Ok gameState.Deck.Length
                    | _ -> Error (game, "Expected game to transition to draw state")
                    
                return! numCards
            }
            
            match numCardsInDeck with
            | Ok(numCardsInDeck) -> Expect.equal numCardsInDeck 45 "Expected game to contain 45 cards"
            | Error(e, _) -> failtest $"Unexpected error: {e}"
        }
        
        test "There should be 3 tokens of each color" {
            let num = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.tryStartGame (fun () -> 1)
                let! tokens = game |> GameState.getTokenPositions
                
                let countColor color game =
                    game
                    |> Map.filter (fun (pawn:Pawn) position -> pawn.Color = color)
                    |> Map.count 
                
                return (tokens |> countColor Color.Red), (tokens |> countColor Color.Yellow)
            }
            
            match num with
            | Ok(numberOfRed, numberOfYellow) -> Expect.equal (numberOfRed, numberOfYellow) (3, 3) "Expected 3 of each color"
            | Error(e, _) -> failtest $"Unexpected error: {e}"
        }
        
        test "All pieces should start on their start square" {
            let allPiecesOnHomeSquare = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.tryStartGame (fun () -> 1)
                let! tokens = game |> GameState.getTokenPositions
                    
                return tokens |> Map.forall (fun _ position -> position = BoardPosition.Start)
            }
            
            match allPiecesOnHomeSquare with
            | Ok(onHomeSquare) -> Expect.isTrue onHomeSquare "Expected all pieces to start on their start square"
            | Error(e, _) -> failtest $"Unexpected error: {e}"
        }
        
        test "When a game is started, the players list should match those added in setup state" {
            let players = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.tryStartGame (fun () -> 1)
                
                return! game |> GameState.getPlayers
            }
            
            match players with
            | Ok(players) -> Expect.equal players [{Name="Levi";Color=Color.Red};{Name="Tim";Color=Color.Yellow}] "Expected players list to match those added in setup state"
            | Error(e, _) -> failtest $"Unexpected error: {e}"
        }
        
        test "When a game is started, the active player should be chosen according to the random number method" {
            let fakeRandomNumberGenerator0() = 0
            let fakeRandomNumberGenerator1() = 1
            let fakeRandomNumberGenerator2() = 2
            
            let activePlayers = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game1 = game |> GameState.tryStartGame fakeRandomNumberGenerator0
                let! game2 = game |> GameState.tryStartGame fakeRandomNumberGenerator1
                let! game3 = game |> GameState.tryStartGame fakeRandomNumberGenerator2
                
                let! activePlayer1 = game1 |> GameState.getActivePlayer
                let! activePlayer2 = game2 |> GameState.getActivePlayer
                let! activePlayer3 = game3 |> GameState.getActivePlayer
                
                return [activePlayer1;activePlayer2;activePlayer3]
            }
            
            let levi = {Name="Levi";Color=Color.Red}
            let tim = {Name="Tim";Color=Color.Yellow}
            
            match activePlayers with
            | Ok(activePlayers) -> Expect.equal activePlayers [levi;tim;levi] "Expected active player to be chosen by random number"
            | Error(e, _) -> failtest $"Unexpected error: {e}"
        }
        
        test "The only action for a newly created game should be 'draw card'" {
            let availableActions = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.tryStartGame (fun () -> 0)
                
                return! game |> GameState.getAvailableActions
            }
            
            match availableActions with
            | Ok(actions) -> Expect.equal actions [Action.DrawCard] "Expected available action to be single action of draw card"
            | Error(e, _) -> failtest $"Unexpected error: {e}"
        }
    ]
    