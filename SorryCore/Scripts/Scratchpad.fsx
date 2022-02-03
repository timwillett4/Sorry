#load "CardDeck.fsx"

// rules only one pawn can be in a square (exception home and start)
// if you land on another player of different color you bump them back to start
// can't land on space your color is on
// slider
// 

// outer = 0-14 for each color


// R14 B0  B1  B2  B3  ... B13 B14
// R13                     Y1
// ..                      ...
// R2                      Y12
// R1                      Y13
// R0 G14 G13  G12 G11 ... G0
//                          

// 1 = slider of 4 (0 to 3) (all but color)
// 2 = home
// 4 = start
// 9 = slider of 5 (9 to 13)
//type Color =
//    | Green
//    | Red
//    | Blue
//    | Yellow

//type Position = 
//    | Outer of Color * int // 0 -14
//    | Start of Color
//    | HomeRow of Color * int // 0 - 5
//    | Home of Color



// gameboard should have map of squares and optionally pawn who is occupied?
// ensure pawn can only be on one square
// only home and start can have multiple pawns
// home, start, and home row can only be occupied by correct color

// State    
// Initial - State none - Action = Setup(2player,3player,4player) -> Transition Playing
// Playing -> State
                  // Drawing
// Finished - State winner -> Action = Restart -> Transition Initial

type BoardState = {
    Deck : Deck
    CurrentPlayer : Color
    TokenPositions : Map<Color, Position * Position * Position>
}

type DrawingState = {
    BoardState : BoardState
}

type ChooseActionState = {
    DrawnCard : Card
    BoardState : BoardState
}

type GameOverState = {
    BoardState : BoardState
    Winner : Color
}

let drawCard (state:DrawingState) =
    let topCard,remaining = state.BoardState.Deck |> drawCard
    {DrawnCard = topCard; BoardState = {state.BoardState with Deck=remaining}}

type TokenID =
    | One
    | Two
    | Three
    | Four

type Token = Color * TokenID

// @TODO - get this down
// card has turn action that can update board
type TurnAction =
    | Move of int
    | Sorry
    | Swap
    // @TODO add special action (ie red next move home, slide, etc)

let getCardAction card =

    let cardActions =
        [(Card.One, [Move1])
         (Card.Two, [Move2])
         (One, [Move3])
         (One, [Move4])
         (One, [Move1])
         (One, [Move1])
        ]

type ValidMove = ValidMove of TurnAction

let moveRedOne5 = Move (5, (Color.Red, TokenID.One))

let getAvailableActions (state:ChooseActionState) =
    [ValidMove moveRedOne5]

// @TODO use state machine ?
    // initial
        // choose number of players -> advance to playing
    // playing
        // draw card = updates deck, drawn, available actions
        // choose action -> updates players positions
        // won ? -> advanced to finished : current player advance
    // finished
        // winner


let drawCard game =
    let card,newDeck = game.Deck |> drawCard
    card, {game with Deck = newDeck}

let getValidTurnActions game : list<ValidMoves> =
    game.Deck

let playTurn validAction game =


//////////////////////////////////
// basic game loop
let card = game |> drawCard
let actions = game |> getValidActions card // returns pass if only valid option
let game = game |> playTurn action[0]
game |> printState
//////////////////////////////////

// need a function that takes a board, a card, and gives possible actions
// then a function that takes a board, and
//let getN
// cards
// 5 - 1s - move from start, move forward 1
// 4 - 2s - move from start, move forward 2
// 4 - 3s - move forward 3
// 4 - 4s - move backwards 4
// 4 - 5s - move forwards 5
// 4 - 7s - move forwards 7, split between 2
// 4 - 8s - move forward 8
// 4 - 10s - move forward 10 or backwards 1
// 4 - 11s - move forward 11 or switch places with another piece
// 4 - 12s - move forward 12
// 4 - sorry cards - move piece from home to where opp is and send there to home, or move 4

// deck
// 

// board
// 14 x 4 + 4 corners + 4 homes + 4 + homerow5

// pieces
// 3 pawns or green, red, plue, yellow

// player
// to 4

// gamestates

// 1)setup

// 2)playing
// deck
// board
// currentTurn

// 3)over

// turn sequence
// draw card -> if(empty)shuffle, draw
// choose action
// isgameover ? declarewinner : advanceturn

// win condition
// all of players pieces in home