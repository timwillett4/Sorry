#r "../../submodules/FSharp.Core.Extensions/bin/Release/netstandard2.0/FSharp.Core.Extensions.dll"
#r "../bin/Release/netstandard2.0/SorryCore.dll"

// setup
"sample console output"
"How Many Players?"

"Enter Player 1 name:"
"Select Player 1 color"

"Enter Player 2 name:"
"Select Player 2 color"

let newGame = createNewGame(Red "Levi", Yellow "Tim")
// game loop

let rec gameLoop gameState =
    printGameInfo(gameState)
    let actions = getAvailableActions(newGame)
    let newGameState = selectAction(actions.[1])
    // if not game over
    //    gameLoop newGameState
...
"[Game Info]"
"BoardState"
"Levi's turn (Red)"
"Available Action: [D]raw"

"Drew an 8"
"Available Action: [1]Move Token1 1"
"Available Action: [2]Move Token1 2"
...

// over
"Levi wins"

"New Game?"
