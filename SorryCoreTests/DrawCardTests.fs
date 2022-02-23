module SorryCoreTests.GamePlayTests

open Sorry.Core
open FSharp.Core.Extensions
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let drawCardTests =
    test "Get current card should return none when card has not been drawn yet" {
        
        let currentCard = result {
            let! game = GameState.newGame |> GameState.tryAddPlayer "Levi" Color.Red
            let! game = game |> GameState.tryAddPlayer "Tim" Color.Yellow
            let! game = game |> GameState.startGame (fun () -> 0)
            
            return! game |> GameState.getCurrentCard
        }
        
        match currentCard with
        | Ok(currentCard) -> Expect.isNone currentCard "Expected current card to be none when no cards had been drawn"
        | Error(e, _) -> failtest $"Unexpected error: {e}"
    }
