module SorryCoreTests.ChooseActionTests

open Sorry.Core
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let chooseActionTests =
    
    // @TODO - generalize test into property based tests?
    testList "Get Available Action Tests" [
        let levi = {Name="Levi"; Color=Color.Green}
        let dad = {Name="Dad"; Color=Color.Blue}
        
        let initialGameState = {
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

        testList "InitialGameState Tests" [
            
            let gameState = Drawing(initialGameState)
            
            test "Initial actions for a newly created game should be {DrawCard}" {
                
                let availableActions = gameState |> GameState.getAvailableActions
                
                match availableActions with
                | Ok(actions) -> Expect.equal actions [Action.DrawCard] "Expected single available action of draw card"
                | Error _ -> failtest "Unexpected error"
            }
            
            test "Choosing draw card action for an initially created game should be a valid action" {
                let newGameState = gameState |> GameState.tryChooseAction Action.DrawCard
                Expect.isOk newGameState "Expect drawing card to be a valid action"
            }
            
            test "Choosing move pawn action for an initially created game should be a invalid action" {
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Blue,PawnID.One,3))
                Expect.isError newGameState "Expect move pawn to be an invalid action"
            }
        ]
        
        let canMoveFromStart card = test $"Drawing a %A{card} should allow you to move any piece from the start square" {
            let initialGameState = ChoosingAction{GameState = initialGameState; DrawnCard = card }
                
            let expectedActions = [
                    Action.MovePawn(Color.Green, PawnID.One, 1)
                    Action.MovePawn(Color.Green, PawnID.Two, 1)
                    Action.MovePawn(Color.Green, PawnID.Three, 1)
            ]
                
            let availableActions = initialGameState |> GameState.getAvailableActions
                
            match availableActions with
            | Ok(actions) -> Expect.equal actions expectedActions "Expected to be able to move any piece from the starting position"
            | Error _ -> failtest "Unexpected error"
        }
        
        let canNotMoveFromStart card = test $"When all pieces are in home squares and a %A{card} is drawn, the player should only be able to pass turn" {
            let initialGameState = ChoosingAction{GameState = initialGameState; DrawnCard = card }
            
            let availableActions = initialGameState |> GameState.getAvailableActions
            
            match availableActions with
            | Ok(actions) -> Expect.equal actions [Action.PassTurn] "Expected pass turn to be only action"
            | Error _ -> failtest "Unexpected error"
        }
        
        [Card.One; Card.Two] |> List.map canMoveFromStart |> testList "Can only move from start with one or two"
        
        [Card.Three
         Card.Four
         Card.Five
         Card.Seven
         Card.Eight
         Card.Ten
         Card.Eleven
         Card.Twelve
         Card.Sorry]
        |> List.map canNotMoveFromStart |> testList "Can not move from start with any card other than one or two"
    ]