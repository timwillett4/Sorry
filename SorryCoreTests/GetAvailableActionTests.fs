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

        testList "Initial State Tests" [
            
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
                            (BoardPosition.Outer(Color.Green, OuterCoordinate.One))
                            "Expected token one to be moved out of start space"
                    | _ -> failtest "Expected game to transition to drawState"
                    
                $"When a %A{card} card is drawn, PassTurn should be an invalid move",
                fun initialGameState () ->
                    let newGameState = initialGameState |> GameState.tryChooseAction Action.PassTurn
                    Expect.isError newGameState "Expected pass turn to be an invalid move"
            ]
        )
            
        [Card.One; Card.Two]|> List.collect canMoveFromStart |> testList "Can only move from start with one or two tests"
        
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
        |> List.collect canNotMoveFromStart |> testList "Can not move from start with any card other than one or two test"
        
        testList "Basic Movement Tests" [
            let levi = {Name="Levi"; Color=Color.Green}
            let dad = {Name="Dad"; Color=Color.Blue}
            
            let gameState = {
                   Deck = newDeck
                   RandomNumberGenerator = fun () -> 0
                   Players = [levi;dad]
                   TokenPositions = [
                       (Color.Green, PawnID.One), BoardPosition.Outer(Color.Green, OuterCoordinate.One)
                       (Color.Green, PawnID.Two), BoardPosition.Outer(Color.Yellow, OuterCoordinate.One)
                       (Color.Green, PawnID.Three), BoardPosition.Outer(Color.Red, OuterCoordinate.One)
                       
                       (Color.Blue, PawnID.One), BoardPosition.Outer(Color.Blue, OuterCoordinate.One)
                       (Color.Blue, PawnID.Two), BoardPosition.Start(Color.Blue)
                       (Color.Blue, PawnID.Three), BoardPosition.Start(Color.Blue)
                   ] |> Map.ofList
                   ActivePlayer = levi
                }
            
            // tests for cards that you just move the number of squares it shows on card
            let basicMovementCardsTests (card:Card) (expectedMove:int) (expectedMoveSquare:BoardPosition) =
                let gameState = ChoosingAction{GameState = gameState; DrawnCard = card }
                Seq.toList (testParam gameState [
                    $"When %A{card} is draw player should be able to move pawn %A{card} space",
                     fun gameState () ->
                        let availableActions = gameState |> GameState.getAvailableActions
                        let expectedActions = [
                            Action.MovePawn(Color.Green, PawnID.One, expectedMove)
                            Action.MovePawn(Color.Green, PawnID.Two, expectedMove)
                            Action.MovePawn(Color.Green, PawnID.Three, expectedMove)
                            ]
                        match availableActions with
                        | Ok(actions) -> Expect.containsAll actions expectedActions $"Expected to be able to move any piece by %A{card}"
                        | Error _ -> failtest "Unexpected Error"
                    $"Expect Token Positions to be updated when moving by %A{card}",
                    fun gameState () ->
                        let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, expectedMove))
                        match newGameState with
                        | Ok(Drawing(gameState)) ->
                            Expect.equal gameState.TokenPositions.[Color.Green, PawnID.One] expectedMoveSquare $"Expected pawn 1 to be moved %i{expectedMove} space to %A{expectedMoveSquare}"
                        | _ -> failtest "Expected game to transition to draw state"
                    $"Expect active player to be updated",
                    fun gameState () ->
                        let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, expectedMove))
                        match newGameState with
                        | Ok(Drawing(gameState)) -> 
                            Expect.equal gameState.ActivePlayer dad "Expected active player to switch to dad"
                        | _ -> failtest "Expected game to transition to draw state" 
                ])        
              
            basicMovementCardsTests Card.One 1 (Outer(Color.Green, OuterCoordinate.Two)) |> testList "One Basic Movement Tests"
            basicMovementCardsTests Card.Three 3 (Outer(Color.Green, OuterCoordinate.Four)) |> testList "Three Basic Movement Tests"
            basicMovementCardsTests Card.Five 5 (Outer(Color.Green, OuterCoordinate.Six)) |> testList "Five Basic Movement Tests"
            basicMovementCardsTests Card.Eight 8 (Outer(Color.Green, OuterCoordinate.Nine)) |> testList "Eight Basic Movement Tests"
            basicMovementCardsTests Card.Twelve 12 (Outer(Color.Green, OuterCoordinate.Thirteen)) |> testList "Twelve Basic Movement Tests"
            
            // Two allow you to go again
            testList "Two Basic Movement Tests" [
                let gameState = ChoosingAction{GameState = gameState; DrawnCard = Card.Two }
                
                test $"When two is draw player should be able to move any pawn two space" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.MovePawn(Color.Green, PawnID.One, 2)
                        Action.MovePawn(Color.Green, PawnID.Two, 2)
                        Action.MovePawn(Color.Green, PawnID.Three, 2)
                        ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece by 2"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, 2))
                
                test $"Expect Token Positions to be updated when moving by 2" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[Color.Green, PawnID.One] (Outer(Color.Green, OuterCoordinate.Three)) "Expected pawn 1 to be moved 2 space to green 3"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect active player to draw again" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer levi "Expected active player to draw again"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
            ]
            
            testList "Four Basic Movement Tests" [
                let gameState = ChoosingAction{GameState = gameState; DrawnCard = Card.Four }
                
                test $"When four is draw player should be able to move any pawn backwards four spaces" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.MovePawn(Color.Green, PawnID.One, -4)
                        Action.MovePawn(Color.Green, PawnID.Two, -4)
                        Action.MovePawn(Color.Green, PawnID.Three, -4)
                        ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece backwards 4"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.Two, -4))
                
                test $"Expect Token Positions to be updated when moving backwards 4" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[Color.Green, PawnID.Two] (Outer(Color.Blue, OuterCoordinate.Twelve))
                            "Expected pawn 1 to be moved backwards 4 spaces to yellow 12"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect turn to move to next player" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer dad "Expect turn to move to next player"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, -4))
                
                test $"Expect token to wrap behind first square moving backwards 4 from opening square" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[Color.Green, PawnID.One] (Outer(Color.Yellow, OuterCoordinate.Twelve))
                            "Expected pawn 1 to be moved backwards 4 spaces to yellow 12"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
            ]
            
            // @TODO - 7
            
            testList "10 Basic Movement Tests" [
                let gameState = ChoosingAction{GameState = gameState; DrawnCard = Card.Ten }
                
                test $"When ten our is draw player should be able to move any pawn backwards 10 spaces or back 1 space" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.MovePawn(Color.Green, PawnID.One, 10)
                        Action.MovePawn(Color.Green, PawnID.Two, 10)
                        Action.MovePawn(Color.Green, PawnID.Three, 10)
                        Action.MovePawn(Color.Green, PawnID.One, -1)
                        Action.MovePawn(Color.Green, PawnID.Two, -1)
                        Action.MovePawn(Color.Green, PawnID.Three, -1)
                        ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece forwards 10 or backwards 1"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.Two, 10))
                
                test $"Expect Token Positions to be updated when moving forwards 10" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[Color.Green, PawnID.Two] (Outer(Color.Yellow, OuterCoordinate.Eleven))
                            "Expected pawn 1 to be moved forwards 10 spaces to yellow 11"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect turn to move to next player" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer dad "Expect turn to move to next player"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(Color.Green, PawnID.One, -1))
                
                test $"Expect token to wrap behind first square moving backwards 1 space from opening square" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[Color.Green, PawnID.One] (Outer(Color.Yellow, OuterCoordinate.Fifteen))
                            "Expected pawn 1 to be moved backwards 1 spaces to yellow 15"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
            ]
            
            // @TODO - 11
            
            // @TODO - Sorry
        ]
        
        // @TODO - test for landing on opponent piece
        // @TODO - test for landing on boost squares
        // @TODO - test for can't land on your own space
        // @TODO - test for can't move
        // @TODO - test for getting home
        // @TODO - test for backwards from safety
    ]