module SorryCoreTests.GamePlayTests

open Sorry.Core
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let drawCardTests =
    testList "Draw card tests" [

        test "Get current card should return none when a card has not been drawn yet" {
            let drawnCard = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.tryStartGame (fun () -> 0)
                
                return game |> GameState.getDrawnCard
            }
            
            match drawnCard with
            | Ok(drawnCard) -> Expect.isNone drawnCard "Expected current card to be none when no cards had been drawn"
            | Error(e, _) -> failtest $"Unexpected error: {e}"
        }
        
        test "The next card chosen should be determined by the random number generator" {
            let random0() =  0
                    
            let drawnCard = result {
                let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameState.tryStartGame random0
                let! game = game |> GameState.tryDrawCard random0
                
                return game |> GameState.getDrawnCard
            }
            
            match drawnCard with
            | Ok(Some(currentCard)) -> Expect.equal currentCard Card.One "Expected first card in deck to be drawn"
            | Ok(None) -> failtest "Expected card to be drawn"
            | Error(e, _) -> failtest $"Unexpected error: {e}"
        }
        
        // test try draw card twice in a row should raise error?
    ]