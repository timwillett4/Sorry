module SorryCoreTests.MoveActionsTests

open Sorry.Core
open Expecto

[<Tests>]
let getAvailableActionTests =
    
    testList "Choose action tests" [
        let levi = {Name="Levi"; Color=Color.Green}
        let dad = {Name="Dad"; Color=Color.Blue}
        
        let initialBoardState = {
               Deck = newDeck
               RandomNumberGenerator = fun () -> 0
               Players = [levi;dad]
               TokenPositions = [
                   GreenPawn1, Start(Color.Green)
                   GreenPawn2, Start(Color.Green)
                   GreenPawn3, Start(Color.Green)
                   
                   BluePawn1, BoardPosition.Start(Color.Blue)
                   BluePawn2, BoardPosition.Start(Color.Blue)
                   BluePawn3, BoardPosition.Start(Color.Blue)
               ] |> Map.ofList
               ActivePlayer = levi
            }

        testList "Initial state tests" [
            
            let gameState = Drawing(initialBoardState)
            
            test "Initial actions for a newly created game should be [DrawCard]" {
                
                let availableActions = gameState |> GameState.getAvailableActions
                
                match availableActions with
                | Ok(actions) -> Expect.equal actions [Action.DrawCard] "Expected single available action of draw card"
                | Error _ -> failtest "Unexpected error"
            }
            
            test "Choosing move pawn action for an initially created game should be a invalid action" {
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(BluePawn1, 3))
                Expect.isError newGameState "Expect move pawn to be an invalid action"
            }
            
            let newGameState = gameState |> GameState.tryChooseAction Action.DrawCard
            
            test "Choosing draw card action for an initially created game should be a valid action" {
                Expect.isOk newGameState "Expect drawing card to be a valid action"
            }
        ]
        
        let canMoveFromStart card =
            let initialGameState = ChoosingAction{BoardState = initialBoardState; DrawnCard = card }
            Seq.toList (testParam initialGameState [
                $"When a %A{card} card is drawn, you should be allowed to move any piece from the start square",
                fun initialGameState () ->
                    let expectedActions = [
                            Action.MovePawn(GreenPawn1, 1)
                            Action.MovePawn(GreenPawn2, 1)
                            Action.MovePawn(GreenPawn3, 1)
                    ]
                        
                    let availableActions = initialGameState |> GameState.getAvailableActions
                        
                    match availableActions with
                    | Ok(actions) -> Expect.equal actions expectedActions "Expected to be able to move any piece from the starting position"
                    | Error _ -> failtest "Unexpected error"
                    
                $"When a %A{card} card is drawn, moving Pawn 1 by 1 should be a valid move",
                fun initialGameState () ->
                    let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, 1))
                    Expect.isOk newGameState "Expected moving pawn one 1 space to be a valid action"
                    
                $"When a %A{card} card is drawn, moving Pawn 1 by 1 should update the board positions",
                fun initialGameState () ->
                    let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, 1))
                    match newGameState with
                    | Ok(Drawing(gameState)) ->
                        Expect.equal
                            gameState.TokenPositions.[GreenPawn1]
                            (BoardPosition.Outer(Color.Green, OuterCoordinate.One))
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
            let initialGameState = ChoosingAction{BoardState = initialBoardState; DrawnCard = Card.One }
            let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, 1))
            match newGameState with
            | Ok(Drawing(gameState)) ->Expect.equal gameState.ActivePlayer dad "Expected turn to move to next player"
            | _ -> failtest "Expected game to transition to drawState"         
        }
        
        test "When two is drawn moving from start should not change active player (player gets to draw again)" {
            let initialGameState = ChoosingAction{BoardState = initialBoardState; DrawnCard = Card.Two }
            let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, 1))
            match newGameState with
            | Ok(Drawing(gameState)) ->Expect.equal gameState.ActivePlayer levi "Expected active player to get another turn"
            | _ -> failtest "Expected game to transition to drawState"         
        }
        
        let canNotMoveFromStart card =
            let initialGameState = ChoosingAction{BoardState = initialBoardState; DrawnCard = card }
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
                            Expect.equal initialGameState.BoardState.TokenPositions newGameState.TokenPositions "Expected token positions to not change"
                        | _ -> failtest "Expected game to transition to drawState"
                        
                 $"When all pieces are in home squares and a %A{card} is drawn, moving a pawn should be an invalid move",
                 fun initialGameState () ->
                    let newGameState = initialGameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, 1))
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
        
        testList "Move token tests" [
            let levi = {Name="Levi"; Color=Color.Green}
            let dad = {Name="Dad"; Color=Color.Blue}
            
            // tests for cards that you just move the number of squares it shows on card
            let basicMovementCardsTests (card:Card) (expectedMove:int) (expectedMoveSquare:BoardPosition) =
                
                let boardState = {
                       Deck = newDeck
                       RandomNumberGenerator = fun () -> 0
                       Players = [levi;dad]
                       TokenPositions = [
                           GreenPawn1, BoardPosition.Outer(Color.Green, OuterCoordinate.One)
                           GreenPawn2, BoardPosition.Outer(Color.Yellow, OuterCoordinate.One)
                           GreenPawn3, BoardPosition.Outer(Color.Red, OuterCoordinate.One)
                           
                           BluePawn1, BoardPosition.Outer(Color.Blue, OuterCoordinate.One)
                           BluePawn2, BoardPosition.Start(Color.Blue)
                           BluePawn3, BoardPosition.Start(Color.Blue)
                       ] |> Map.ofList
                       ActivePlayer = levi
                }
                
                let gameState = ChoosingAction{BoardState = boardState; DrawnCard = card }
                
                Seq.toList (testParam gameState [
                    $"When %A{card} is drawn, active player should be able to move pawn %A{card} space",
                     fun gameState () ->
                        let availableActions = gameState |> GameState.getAvailableActions
                        let expectedActions = [
                            Action.MovePawn(GreenPawn1, expectedMove)
                            Action.MovePawn(GreenPawn2, expectedMove)
                            Action.MovePawn(GreenPawn3, expectedMove)
                            ]
                        match availableActions with
                        | Ok(actions) -> Expect.containsAll actions expectedActions $"Expected to be able to move any piece by %A{card}"
                        | Error _ -> failtest "Unexpected Error"
                    $"Expect token position to be updated when moving by %A{card}",
                    fun gameState () ->
                        let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, expectedMove))
                        match newGameState with
                        | Ok(Drawing(gameState)) ->
                            Expect.equal gameState.TokenPositions.[GreenPawn1] expectedMoveSquare $"Expected pawn 1 to be moved %i{expectedMove} space to %A{expectedMoveSquare}"
                        | _ -> failtest "Expected game to transition to draw state"
                    $"Expect active player to be updated",
                    fun gameState () ->
                        let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, expectedMove))
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
            
            testList "Card 2 choose action tests" [
                
                let boardState = {
                       Deck = newDeck
                       RandomNumberGenerator = fun () -> 0
                       Players = [levi;dad]
                       TokenPositions = [
                           GreenPawn1, BoardPosition.Outer(Color.Green, OuterCoordinate.One)
                           GreenPawn2, BoardPosition.Outer(Color.Yellow, OuterCoordinate.One)
                           GreenPawn3, BoardPosition.Outer(Color.Red, OuterCoordinate.One)
                           
                           BluePawn1, BoardPosition.Outer(Color.Blue, OuterCoordinate.One)
                           BluePawn2, BoardPosition.Start(Color.Blue)
                           BluePawn3, BoardPosition.Start(Color.Blue)
                       ] |> Map.ofList
                       ActivePlayer = levi
                }
                
                let gameState = ChoosingAction{BoardState = boardState; DrawnCard = Card.Two }
                
                test $"When a two card is drawn, the active player should be able to move any pawn not on start two space" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.MovePawn(GreenPawn1, 2)
                        Action.MovePawn(GreenPawn2, 2)
                        Action.MovePawn(GreenPawn3, 2)
                        ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece by 2"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, 2))
                
                test $"Expect token positions to be updated when moving by 2" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn1] (Outer(Color.Green, OuterCoordinate.Three)) "Expected pawn 1 to be moved 2 space to green 3"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect active player to draw again when 2 is drawn" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer levi "Expected active player to draw again"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
            ]
            
            testList "Card 4 movement tests" [
                
                let boardState = {
                       Deck = newDeck
                       RandomNumberGenerator = fun () -> 0
                       Players = [levi;dad]
                       TokenPositions = [
                           GreenPawn1, BoardPosition.Outer(Color.Green, OuterCoordinate.One)
                           GreenPawn2, BoardPosition.Outer(Color.Yellow, OuterCoordinate.One)
                           GreenPawn3, BoardPosition.Outer(Color.Red, OuterCoordinate.One)
                           
                           BluePawn1, BoardPosition.Outer(Color.Blue, OuterCoordinate.One)
                           BluePawn2, BoardPosition.Start(Color.Blue)
                           BluePawn3, BoardPosition.Start(Color.Blue)
                       ] |> Map.ofList
                       ActivePlayer = levi
                }
                
                let gameState = ChoosingAction{BoardState = boardState; DrawnCard = Card.Four }
                
                test $"When a four card is drawn, the active player should be able to move any pawn backwards four spaces" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.MovePawn(GreenPawn1, -4)
                        Action.MovePawn(GreenPawn2, -4)
                        Action.MovePawn(GreenPawn3, -4)
                        ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece backwards 4"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn2, -4))
                
                test $"Expect token position to be updated when moving backwards 4" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn2] (Outer(Color.Blue, OuterCoordinate.Twelve))
                            "Expected pawn 1 to be moved backwards 4 spaces to yellow 12"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect turn to move to next player" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer dad "Expect turn to move to next player"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, -4))
                
                test $"Expect token to wrap behind first square moving backwards 4 from opening square" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn1] (Outer(Color.Yellow, OuterCoordinate.Twelve))
                            "Expected pawn 1 to be moved backwards 4 spaces to yellow 12"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
            ]
            
            testList "Card 7 movement tests" [
                
                let boardState = {
                       Deck = newDeck
                       RandomNumberGenerator = fun () -> 0
                       Players = [levi;dad]
                       TokenPositions = [
                           GreenPawn1, BoardPosition.Outer(Color.Green, OuterCoordinate.One)
                           GreenPawn2, BoardPosition.Outer(Color.Red, OuterCoordinate.One)
                           GreenPawn3, BoardPosition.Start(Color.Green)
                           
                           BluePawn1, BoardPosition.Start(Color.Blue)
                           BluePawn2, BoardPosition.Start(Color.Blue)
                           BluePawn3, BoardPosition.Start(Color.Blue)
                       ] |> Map.ofList
                       ActivePlayer = levi
                }
                
                let gameState = ChoosingAction{BoardState = boardState; DrawnCard = Card.Seven }
                
                test $"When a 7 card is drawn, the active player should be able to move any pawn not on start forward 7 spaces or split between 2 pieces" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.MovePawn(GreenPawn1, 7)
                        Action.MovePawn(GreenPawn2, 7)
                        Action.SplitMove7((GreenPawn1, 1), (GreenPawn2, 6))
                        Action.SplitMove7((GreenPawn1, 2), (GreenPawn2, 5))
                        Action.SplitMove7((GreenPawn1, 3), (GreenPawn2, 4))
                        Action.SplitMove7((GreenPawn1, 4), (GreenPawn2, 3))
                        Action.SplitMove7((GreenPawn1, 5), (GreenPawn2, 2))
                        Action.SplitMove7((GreenPawn1, 6), (GreenPawn2, 1))
                    ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece backwards 4"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, 7))
                
                test $"Expect token position to be updated when moving pawn forward 7" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn1] (Outer(Color.Green, OuterCoordinate.Eight))
                            "Expected pawn 1 to be moved forward 7 spaces to green 8"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect turn to move to next player" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer dad "Expect turn to move to next player"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.SplitMove7((GreenPawn1, 1),(GreenPawn2,6)))
                
                test $"Expect token 1 to be move forward 1 when splitting move 7 and moving pawn 1 forward 1" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn1] (Outer(Color.Green, OuterCoordinate.Two))
                            "Expected pawn 1 to be moved backwards 1 spaces to green 1"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect token 2 to be move forward 6 when splitting move 7 and moving pawn 2 forward 6" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn2] (Outer(Color.Red, OuterCoordinate.Seven))
                            "Expected pawn 2 to be moved forward 6 spaces to red 7"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
            ]
            
            testList "Card 10 movement Tests" [
                
                let boardState = {
                       Deck = newDeck
                       RandomNumberGenerator = fun () -> 0
                       Players = [levi;dad]
                       TokenPositions = [
                           GreenPawn1, BoardPosition.Outer(Color.Green, OuterCoordinate.One)
                           GreenPawn2, BoardPosition.Outer(Color.Yellow, OuterCoordinate.One)
                           GreenPawn3, BoardPosition.Outer(Color.Red, OuterCoordinate.One)
                           
                           BluePawn1, BoardPosition.Outer(Color.Blue, OuterCoordinate.One)
                           BluePawn2, BoardPosition.Start(Color.Blue)
                           BluePawn3, BoardPosition.Start(Color.Blue)
                       ] |> Map.ofList
                       ActivePlayer = levi
                }
                
                let gameState = ChoosingAction{BoardState = boardState; DrawnCard = Card.Ten }
                
                test $"When a ten card is drawn, the active player should be able to move any pawn forwards 10 spaces or back 1 space" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.MovePawn(GreenPawn1, 10)
                        Action.MovePawn(GreenPawn2, 10)
                        Action.MovePawn(GreenPawn3, 10)
                        Action.MovePawn(GreenPawn1, -1)
                        Action.MovePawn(GreenPawn2, -1)
                        Action.MovePawn(GreenPawn3, -1)
                        ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece forwards 10 or backwards 1"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn2, 10))
                
                test $"Expect token position to be updated when moving forwards 10" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn2] (Outer(Color.Yellow, OuterCoordinate.Eleven))
                            "Expected pawn 1 to be moved forwards 10 spaces to yellow 11"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect turn to move to next player" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer dad "Expect turn to move to next player"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, -1))
                
                test $"Expect token to wrap behind first square moving backwards 1 space from opening square" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn1] (Outer(Color.Yellow, OuterCoordinate.Fifteen))
                            "Expected pawn 1 to be moved backwards 1 spaces to yellow 15"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
            ]
            
            testList "Card 11 movement tests" [
                
                let boardState = {
                       Deck = newDeck
                       RandomNumberGenerator = fun () -> 0
                       Players = [levi;dad]
                       TokenPositions = [
                           GreenPawn1, BoardPosition.Outer(Color.Green, OuterCoordinate.One)
                           GreenPawn2, BoardPosition.Outer(Color.Yellow, OuterCoordinate.One)
                           GreenPawn3, BoardPosition.Outer(Color.Red, OuterCoordinate.One)
                           
                           BluePawn1, BoardPosition.Outer(Color.Blue, OuterCoordinate.One)
                           BluePawn2, BoardPosition.Start(Color.Blue)
                           BluePawn3, BoardPosition.Start(Color.Blue)
                       ] |> Map.ofList
                       ActivePlayer = levi
                }
                
                let gameState = ChoosingAction{BoardState = boardState; DrawnCard = Card.Eleven }
                
                test $"When an 11 is drawn, the active player should be able to move any pawn forwards 11 spaces or switch with an opponents piece" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.MovePawn(GreenPawn1, 11)
                        Action.MovePawn(GreenPawn2, 11)
                        Action.MovePawn(GreenPawn3, 11)
                        Action.SwitchPawns(GreenPawn1, BluePawn1)
                        Action.SwitchPawns(GreenPawn2, BluePawn1)
                        Action.SwitchPawns(GreenPawn3, BluePawn1)
                        ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece forwards 11 or switch with an opponents piece"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.MovePawn(GreenPawn1, 11))
                
                test $"Expect token position to be updated when moving forwards 11" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn1] (Outer(Color.Green, OuterCoordinate.Twelve))
                            "Expected pawn 1 to be moved forwards 11 spaces to yellow 12"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect turn to move to next player" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer dad "Expect turn to move to next player"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.SwitchPawns(GreenPawn1, BluePawn1))
                
                test $"Expect green pawn 1 to move to position where blue pawn 1 was" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn1] (Outer(Color.Blue, OuterCoordinate.One))
                            "Expected green pawn 1 to move to position where blue pawn 1 was"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect blue pawn 1 to move to position where green pawn 1 was" {
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[BluePawn1] (Outer(Color.Green, OuterCoordinate.One))
                            "Expected green pawn 1 to move to position where blue pawn 1 was"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
    
                // @TODO - 11 Special Rules:
                // @TODO -     Allowed to pass instead of use switch if you can't move 11
                // @TODO -     If you switch with a player on boost square you get to boost
                // @TODO -     Can't switch with a pawn on start/home/or safety
            ]
            
            testList "Card 'Sorry' movement tests" [
                let levi = {Name="Levi"; Color=Color.Green}
                let dad = {Name="Dad"; Color=Color.Blue}
                
                let boardState = {
                       Deck = newDeck
                       RandomNumberGenerator = fun () -> 0
                       Players = [levi;dad]
                       TokenPositions = [
                           GreenPawn1, BoardPosition.Start(Color.Green)
                           GreenPawn2, BoardPosition.Start(Color.Green)
                           GreenPawn3, BoardPosition.Outer(Color.Red, OuterCoordinate.One)
                           
                           BluePawn1, BoardPosition.Outer(Color.Blue, OuterCoordinate.One)
                           BluePawn2, BoardPosition.Outer(Color.Blue, OuterCoordinate.Two)
                           BluePawn3, BoardPosition.Start(Color.Blue)
                       ] |> Map.ofList
                       ActivePlayer = levi
                    }
                
                let gameState = ChoosingAction{BoardState = boardState; DrawnCard = Card.Sorry }
                
                test $"When Sorry card is drawn, the active player should be able to move any piece out of start to a square occupied by an opponent pawn and send that pawn back to start" {
                    let availableActions = gameState |> GameState.getAvailableActions
                    let expectedActions = [
                        Action.Sorry(GreenPawn1, BluePawn1)
                        Action.Sorry(GreenPawn2, BluePawn2)
                        Action.Sorry(GreenPawn2, BluePawn1)
                        Action.Sorry(GreenPawn2, BluePawn2)
                        ]
                    match availableActions with
                    | Ok(actions) -> Expect.containsAll actions expectedActions "Expected to be able to move any piece on start to bump opponents pawn on outer square"
                    | Error _ -> failtest "Unexpected Error" 
                }
                
                let newGameState = gameState |> GameState.tryChooseAction (Action.Sorry(GreenPawn1, BluePawn1))
                
                test $"Expect green pawn 1 to move to square where blue pawn 1 was" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[GreenPawn1] (Outer(Color.Blue, OuterCoordinate.One))
                            "Expected green pawn1 to move to square where blue pawn 2 was"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect blue pawn 1 to move back to start" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.TokenPositions.[BluePawn1] (Start(Color.Blue))
                            "Expected blue pawn 1 to go back to start"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                test $"Expect turn to move to next player" {
                    
                    match newGameState with
                    | Ok(Drawing(gameState)) -> 
                        Expect.equal gameState.ActivePlayer dad "Expect turn to move to next player"
                    | _ -> failtest "Expected game to transition to draw state" 
                }
                
                // @TODO - test for can't bump piece on safety or home
            ]
        ]
        
        // @TODO - test for landing on opponent piece
        // @TODO - test for landing on boost squares
        // @TODO - test for can't land on your own space
        // @TODO - test for can't move
        // @TODO - test for getting home
        // @TODO - test for backwards from safety
    ]