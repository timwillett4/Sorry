module SorryCoreTests.PresentationTests

open Expecto
open Sorry.Core

[<Tests>]
let startGameTests =
    
    testList "toBoardPos Tests" [
        
        let GreenPawn1 = {Color = Color.Green; ID = PawnID.One}
        
        testList "Outer Green Tests" [
            test "Expect outer green 1 to be screen coords (11, 15)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Green, OuterCoordinate.One)) GreenPawn1
                Expect.equal screenPos (11, 15) "Expected Green Outer 1 to equal (11, 15)"
            }
            
            test "Expect outer green 12 to be screen coords (0, 15)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Green, OuterCoordinate.Twelve)) GreenPawn1
                Expect.equal screenPos (0, 15) "Expected Green Outer 1 to equal (0, 15)"
            }
            
            test "Expect outer green 13 to be screen coords (0, 14)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Green, OuterCoordinate.Thirteen)) GreenPawn1
                Expect.equal screenPos (0, 14) "Expected Green Outer 1 to equal (0, 14)"
            }
            
            test "Expect outer green 15 to be screen coords (0, 12)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Green, OuterCoordinate.Fifteen)) GreenPawn1
                Expect.equal screenPos (0, 12) "Expected Green Outer 15 to equal (0, 12)"
            }
        ]
        
        testList "Outer Red Tests" [
            test "Expect outer Red 1 to be screen coords (0, 11)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Red, OuterCoordinate.One)) GreenPawn1
                Expect.equal screenPos (0, 11) "Expected Red Outer 1 to equal (0, 11)"
            }
            
            test "Expect outer Red 12 to be screen coords (0, 0)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Red, OuterCoordinate.Twelve)) GreenPawn1
                Expect.equal screenPos (0, 0) "Expected Red Outer 1 to equal (0, 0)"
            }
            
            test "Expect outer Red 13 to be screen coords (1, 0)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Red, OuterCoordinate.Thirteen)) GreenPawn1
                Expect.equal screenPos (1, 0) "Expected Red Outer 1 to equal (1, 0)"
            }
            
            test "Expect outer Red 15 to be screen coords (3, 0)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Red, OuterCoordinate.Fifteen)) GreenPawn1
                Expect.equal screenPos (3, 0) "Expected Red Outer 15 to equal (3, 0)"
            }
        ]
        
        testList "Outer Blue Tests" [
            test "Expect outer Blue 1 to be screen coords (4, 0)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Blue, OuterCoordinate.One)) GreenPawn1
                Expect.equal screenPos (4, 0) "Expected Blue Outer 1 to equal (4, 0)"
            }
            
            test "Expect outer Blue 12 to be screen coords (15, 0)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Blue, OuterCoordinate.Twelve)) GreenPawn1
                Expect.equal screenPos (15, 0) "Expected Blue Outer 1 to equal (15, 0)"
            }
            
            test "Expect outer Blue 13 to be screen coords (15, 1)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Blue, OuterCoordinate.Thirteen)) GreenPawn1
                Expect.equal screenPos (15, 1) "Expected Blue Outer 1 to equal (15, 1)"
            }
            
            test "Expect outer Blue 15 to be screen coords (15, 3)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Blue, OuterCoordinate.Fifteen)) GreenPawn1
                Expect.equal screenPos (15, 3) "Expected Blue Outer 15 to equal (15, 3)"
            }
        ]
        
        testList "Outer Yellow Tests" [
            test "Expect outer Yellow 1 to be screen coords (15, 4)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Yellow, OuterCoordinate.One)) GreenPawn1
                Expect.equal screenPos (15, 4) "Expected Yellow Outer 1 to equal (15, 4)"
            }
            
            test "Expect outer Yellow 12 to be screen coords (15, 15)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Yellow, OuterCoordinate.Twelve)) GreenPawn1
                Expect.equal screenPos (15, 15) "Expected Yellow Outer 1 to equal (15, 15)"
            }
            
            test "Expect outer Yellow 13 to be screen coords (14, 15)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Yellow, OuterCoordinate.Thirteen)) GreenPawn1
                Expect.equal screenPos (14, 15) "Expected Yellow Outer 1 to equal (14, 15)"
            }
            
            test "Expect outer Yellow 15 to be screen coords (12, 15)" {
                let screenPos = Presentation.toScreenCoords (Outer(Color.Yellow, OuterCoordinate.Fifteen)) GreenPawn1
                Expect.equal screenPos (12, 15) "Expected Yellow Outer 15 to equal (12, 15)"
            }
        ]
    ]