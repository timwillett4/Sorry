[<AutoOpen>]
module Sorry.Core.DomainTypes

/// The color enum represent the four possible colors players can choose from
type Color =
    | Green = 0
    | Red = 1
    | Blue = 2
    | Yellow = 3

type Card =
    | One
    | Two
    | Three
    | Four
    | Five
    | Seven
    | Eight
    | Ten
    | Eleven
    | Twelve
    | Sorry

type Deck = Card list

type PawnID = 
    | One
    | Two
    | Three


type Pawn = {
    Color : Color
    ID : PawnID
}

type Action =
    | DrawCard
    | MovePawn of pawn:Pawn*spaces:int
    | SplitMove7 of move1:(Pawn*int)*move2:(Pawn*int)
    | SwitchPawns of pawn1:Pawn*pawn2:Pawn
    | Sorry of pawnOnStart:Pawn*pawnToBump:Pawn
    | PassTurn 

let newDeck = [
    // 5 ones, 4 of every other card
    Card.One
    Card.One
    Card.One
    Card.One
    Card.One
    
    Card.Two
    Card.Two
    Card.Two
    Card.Two
   
    Card.Three
    Card.Three
    Card.Three
    Card.Three
    
    Card.Four
    Card.Four
    Card.Four
    Card.Four
    
    Card.Five
    Card.Five
    Card.Five
    Card.Five
    
    Card.Seven
    Card.Seven
    Card.Seven
    Card.Seven
   
    Card.Eight
    Card.Eight
    Card.Eight
    Card.Eight
    
    Card.Ten
    Card.Ten
    Card.Ten
    Card.Ten
    
    Card.Eleven
    Card.Eleven
    Card.Eleven
    Card.Eleven
    
    Card.Twelve
    Card.Twelve
    Card.Twelve
    Card.Twelve
    
    Card.Sorry
    Card.Sorry
    Card.Sorry
    Card.Sorry
]

type Player = {
    Name : string    
    Color : Color
}
    
/// BoardPosition Represents all of the possible positions on a sorry board.
/// There are 60 squares on the outside of the board, a start square for each color
/// 5 safety squares and home square
/// 
/// The numbering system for outer squares is as follows:
///     The first square that a color moves to outside of it's start is square Color * 0.
///     The next square it moves to is 1, etc.
///     When you reach the next players opening square it resets to NextColor * 0
///
/// <example>
///         R14/B0
///          | |
///        _ _ _ _ _ _ _ _ _ _ _ _ _ 
///        _   _                   _ <- B14
///        _   _       _ _ _ _ _ _ _ <- Y0
///        _   _                   _
///        _   _                   _
///        _   _                   _
///        _   _                   _
///        _                       _
///        _                       _
///        _                       _
///        _                   _   _
///        _                   _   _
///        _                   _   _
///        _                   _   _
/// R0->   _ _ _ _ _ _ _       _   _
/// G14->  _                   _   _
///        _ _ _ _ _ _ _ _ _ _ _ _ _
///                            | |
///                           G0/Y14
/// </example>

type OuterCoordinate =
    | One = 1
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6
    | Seven = 7
    | Eight = 8
    | Nine = 9
    | Ten = 10
    | Eleven = 11
    | Twelve = 12
    | Thirteen = 13
    | Fourteen = 14
    | Fifteen = 15
    
type SafetySquare =
    | One = 1
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    
type BoardPosition = 
    | Outer of Color * OuterCoordinate
    | Safety of SafetySquare
    | Start 
    | Home 

/// Board state is a map storing where each pawn is on board 
type TokenPositions = Map<Pawn, BoardPosition>

/// The setup state indicates players are still being added and the game has not yet started
type SetupState = {
   Players: Player list    
}

/// The DrawState is when a player is currently up to draw a card

type BoardState = {
    Deck : Deck
    Players : Player list
    TokenPositions : TokenPositions
    ActivePlayer : Player
}

/// The ChooseActionState is when a player has just drawn a card and now much choose what action to take
/// (For example which token to move, or what move to make for cards that have options such as 7 or 10)
type ChooseActionState = {
    BoardState : BoardState
    DrawnCard : Card
}

/// The GameOverState is when the game is over and a player has won
type GameOverState = {
    Winner : Player
}

/// GameState is a discriminate union storing the current state of the game
type GameState = 
   | SettingUp of SetupState 
   | Drawing of BoardState
   | ChoosingAction of ChooseActionState
   | GameOver of GameOverState
