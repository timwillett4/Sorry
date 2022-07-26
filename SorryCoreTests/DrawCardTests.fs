module SorryCoreTests.GamePlayTests

open Sorry.Core
open FSharp.Core.Extensions.Result
open Expecto

[<Tests>]
let drawCardTests =
    testList "Draw card tests" [

        let levi = {Name="Levi"; Color=Color.Green}
        let dad = {Name="Dad"; Color=Color.Blue}
        
        let GreenPawn1 = {Color=Color.Green;ID=PawnID.One}
        let GreenPawn2 = {Color=Color.Green;ID=PawnID.Two}
        let GreenPawn3 = {Color=Color.Green;ID=PawnID.Three}
        let GreenPawn3 = {Color=Color.Green;ID=PawnID.Three}
        let GreenPawn4 = {Color=Color.Green;ID=PawnID.Three}
        
        let BluePawn1 = {Color=Color.Blue;ID=PawnID.One}
        let BluePawn2 = {Color=Color.Blue;ID=PawnID.Two}
        let BluePawn3 = {Color=Color.Blue;ID=PawnID.Three}
        let BluePawn4 = {Color=Color.Blue;ID=PawnID.Three}
        
        let newGame = {
            Deck = newDeck
            Players = [levi;dad]
            TokenPositions = [
                // Yellow 14 = 1 away from safety square
                GreenPawn1, BoardPosition.Start
                GreenPawn2, BoardPosition.Start
                GreenPawn3, BoardPosition.Start
                GreenPawn4, BoardPosition.Start
                
                BluePawn1, BoardPosition.Start
                BluePawn2, BoardPosition.Start
                BluePawn3, BoardPosition.Start
                BluePawn4, BoardPosition.Start
            ] |> Map.ofList
            ActivePlayer = levi
        }
        
        let newGame = DrawingCard(newGame)
        
        test "Get current card should return none when a card has not been drawn yet" {
            let drawnCard = result {
                let! game = GameBuilder.newGame |> GameBuilder.tryAddPlayer "Levi" Color.Red
                let! game = game |> GameBuilder.tryAddPlayer "Tim" Color.Yellow
                let! game = game |> GameBuilder.tryStartGame (fun () -> 0)
                
                return game |> GameState.getDrawnCard
            }
            
            Expect.equal drawnCard (Ok(None)) "Expected getDrawnCard to return none when still in draw state"
        }
        
        test "The next card chosen should be determined by the random number generator" {
            let random0() =  0
                    
            let drawnCard = result {
                let! game = newGame |> GameState.tryDrawCard random0
                
                return game |> GameState.getDrawnCard
            }
            
            Expect.equal drawnCard (Ok(Some(Card.One))) "Expected first card in deck to be drawn"
        }
        
        // test try draw card twice in a row should raise error?
    ]