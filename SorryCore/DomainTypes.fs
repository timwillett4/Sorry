[<AutoOpen>]
module Sorry.Core.DomainTypes

open Sorry.Core

/// The color enum represent the four possible colors players can choose from
type Color =
    | Green = 0
    | Red = 1
    | Blue = 2
    | Yellow = 3

/// Card type is a discrimenent union representing the 12 different types of cards 
// @TODO - add desckriptions (and how many of each type there should be?)
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

type Player = {
    Name : string    
    Color : Color
    // Keep it simpler for now
    //Age : Int0to100.T
    //Name : Name.T
}
    
/// BoardPosition Represents all of the possible positions on a sorry board.
/// There are 60 squares on the outside of the board, a start square for each color
/// 5 safety squares and home square
/// 
/// The numbering system for outer squares is as follows:
///     The first square that a color moves to outside of it's home is square 0.
///     The next square it moves to is 1, etc.
///
/// <example>
///     R0 1 2 3 4 5 6 . . . . . . R13
/// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
/// _   ^                         _ R14
/// _                           > _ B0
/// ...
/// </example>
type BoardPosition = 
    | Outer of Color * Int0to14.T
    | Safety of Color * Int0to4.T
    | Start of Color   
    | Home of Color 

/// Board state is a map storing where each pawn is on board for each color
type BoardState = Map<Color, Map<PawnID, BoardPosition>>

/// The setup state indicates players are still being added and the game has not yet started
type SetupState = {
   Players: Player list    
}

/// The DrawState is when a player is currently up to draw a card
type DrawState = {
    Deck : Deck
    TokenPositions : BoardState
    ActivePlayer : Player
}

/// The ChooseActionState is when a player has just drawn a card and not much choose what action to take
/// (For example which token to move, or what move to make for cards that have options such as 7 or 10)
type ChooseActionState = {
    GameState : DrawState
    DrawnCard : Card
}

/// The GameOverState is when the game is over and a player has won
type GameOverState = {
    Winner : Player
}

/// GameState is a discriminate union storing the current state of the game
type GameState = 
   | SettingUp of SetupState 
   | Drawing of DrawState
   | ChoosingAction of ChooseActionState
   | GameOverGameOverState
