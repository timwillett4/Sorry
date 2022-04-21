module SorryCoreTests.ChooseActionTests

open Sorry.Core
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let chooseActionTests =
    
    // @TODO - these might be good candidates for property based tests
    testList "ChooseActionTests" [
        let levi = {Name="Levi"; Color=Color.Green}
        let dad = {Name="Dad"; Color=Color.Blue}
        
        testList "InitialGameState Tests" [
            let initialGameState = Drawing{
                   Deck = newDeck
                   RandomNumberGenerator = fun () -> 0
                   Players = [levi;dad]
                   TokenPositions = [
                       (Color.Green, PawnID.One), BoardPosition.Start(Color.Green)
                       (Color.Green, PawnID.Two), BoardPosition.Start(Color.Green)
                       (Color.Green, PawnID.Three), BoardPosition.Start(Color.Green)
                       
                       (Color.Blue, PawnID.One), BoardPosition.Start(Color.Blue)
                       (Color.Blue, PawnID.Two), BoardPosition.Start(Color.Blue)
                       (Color.Blue, PawnID.Three), BoardPosition.Start(Color.Blue)
                   ] |> Map.ofList
                   ActivePlayer = levi
                }
            
            test "Initial actions for a newly created game should be {DrawCard}" {
                
                let availableActions = initialGameState |> GameState.getAvailableActions
                
                match availableActions with
                | Ok(actions) -> Expect.equal actions [Action.DrawCard] "Expected single available action of draw card"
                | Error _ -> failtest "Unexpected error"
            }
            
            test "Choosing a valid action should not return an error" {
                let newGameState = initialGameState |> GameState.tryChooseAction Action.DrawCard
                Expect.isOk newGameState "Expect drawing card to be a valid action"
            }
            
            test "Choosing an unavailable action should return an error" {
                let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(Color.Blue,PawnID.One,3))
                Expect.isError newGameState "Expect move pawn to be an invalid action"
            }
        ]
        
    ]
