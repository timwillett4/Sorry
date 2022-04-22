module SorryCoreTests.GetAvailableActions

open Sorry.Core
open Expecto

[<Tests>]
let getAvailableActionTests =
    
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
            
            test "Choosing move pawn action for an initially created game should be a invalid action" {
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Blue,PawnID.One,3))
                Expect.isError newGameState "Expect move pawn to be an invalid action"
            }
            
            let newGameState = gameState |> GameState.tryChooseAction Action.DrawCard
            
            test "Choosing draw card action for an initially created game should be a valid action" {
                Expect.isOk newGameState "Expect drawing card to be a valid action"
            }
        ]
        
        let canMoveFromStart card =
            let initialGameState = ChoosingAction{GameState = initialGameState; DrawnCard = card }
            Seq.toList (testParam initialGameState [
                $"When a %A{card} card is drawn, you should be allowed to move any piece from the start square",
                fun initialGameState () ->
                    let expectedActions = [
                            Action.MovePawn(Color.Green, PawnID.One, 1)
                            Action.MovePawn(Color.Green, PawnID.Two, 1)
                            Action.MovePawn(Color.Green, PawnID.Three, 1)
                    ]
                        
                    let availableActions = initialGameState |> GameState.getAvailableActions
                        
                    match availableActions with
                    | Ok(actions) -> Expect.equal actions expectedActions "Expected to be able to move any piece from the starting position"
                    | Error _ -> failtest "Unexpected error"
                    
                $"When a %A{card} card is drawn, moving Pawn 1 by 1 should be a valid move",
                fun initialGameState () ->
                    let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, 1))
                    Expect.isOk newGameState "Expected moving pawn one 1 space to be a valid action"
                    
                $"When a %A{card} card is drawn, moving Pawn 1 by 1 should update the board positions",
                fun initialGameState () ->
                    let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, 1))
                    match newGameState with
                    | Ok(Drawing(gameState)) ->
                        Expect.equal
                            gameState.TokenPositions.[Color.Green, PawnID.One]
                            (BoardPosition.Outer(Color.Green, OuterCoordinate.Zero))
                            "Expected token one to be moved out of start space"
                    | _ -> failtest "Expected game to transition to drawState"
                    
                $"When a %A{card} card is drawn, PassTurn should be an invalid move",
                fun initialGameState () ->
                    let newGameState = initialGameState |> GameState.tryChooseAction Action.PassTurn
                    Expect.isError newGameState "Expected pass turn to be an invalid move"
            ]
        )
            
        [Card.One; Card.Two]|> List.collect canMoveFromStart |> testList "Can only move from start with one or two"
        
        test "When one is drawn moving from start should change active player" {
            let initialGameState = ChoosingAction{GameState = initialGameState; DrawnCard = Card.One }
            let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, 1))
            match newGameState with
            | Ok(Drawing(gameState)) ->Expect.equal gameState.ActivePlayer dad "Expected turn to move to next player"
            | _ -> failtest "Expected game to transition to drawState"         
        }
        
        test "When two is drawn moving from start should not change active player (player gets to draw again)" {
            let initialGameState = ChoosingAction{GameState = initialGameState; DrawnCard = Card.Two }
            let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, 1))
            match newGameState with
            | Ok(Drawing(gameState)) ->Expect.equal gameState.ActivePlayer levi "Expected active player to get another turn"
            | _ -> failtest "Expected game to transition to drawState"         
        }
        
        let canNotMoveFromStart card =
            let initialGameState = ChoosingAction{GameState = initialGameState; DrawnCard = card }
            Seq.toList (testParam initialGameState [
                $"When all pieces are in home squares and a %A{card} is drawn, the player should only be able to pass turn",
                 fun initialGameState () ->
                    let availableActions = initialGameState |> GameState.getAvailableActions
                    
                    match availableActions with
                    | Ok(actions) -> Expect.equal actions [Action.PassTurn] "Expected pass turn to be only action"
                    | Error _ -> failtest "Unexpected error"
                    
                    
                 $"When all pieces are in home squares and a %A{card} is drawn, choosing pass turn action should change the active player",
                 fun initialGameState () ->
                        let newGameState = initialGameState |> GameState.tryChooseAction Action.PassTurn
                        match newGameState with
                        | Ok(Drawing(gameState)) ->
                            Expect.equal gameState.ActivePlayer dad "Expected turn to move to next player"
                        | _ -> failtest "Expected game to transition to drawState"
                        
                 $"When all pieces are in home squares and a %A{card} is drawn, choosing pass turn action should not change the token positions",
                 fun initialGameState () ->
                        let newGameState = initialGameState |> GameState.tryChooseAction Action.PassTurn
                        match (newGameState,initialGameState) with
                        | Ok(Drawing(newGameState)),ChoosingAction(initialGameState) ->
                            Expect.equal initialGameState.GameState.TokenPositions newGameState.TokenPositions "Expected token positions to not change"
                        | _ -> failtest "Expected game to transition to drawState"
                        
                 $"When all pieces are in home squares and a %A{card} is drawn, moving a pawn should be an invalid move",
                 fun initialGameState () ->
                    let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, 1))
                    Expect.isError newGameState "Expected moving pawn to be an invalid move"
            ]        
        )
        
        [Card.Three
         Card.Four
         Card.Five
         Card.Seven
         Card.Eight
         Card.Ten
         Card.Eleven
         Card.Twelve
         Card.Sorry]
        |> List.collect canNotMoveFromStart |> testList "Can not move from start with any card other than one or two"
    ]