

module Blackjack where

-- Import necessary modules
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Task A1 --

hand2 :: Hand 
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

sizeSteps :: [Int]
sizeSteps = [ size hand2 
            , size (Card (Numeric 2) Hearts : [Card Jack Spades])
            , 1 + size [Card Jack Spades]
            , 1 + (1 + size [])
            , 1 + (1 + 0)
            , 1 + 1
            , 2
            ]

-- Task A2 --
-- returns a string of a hand with cards on separate lines
display :: Hand -> String
display hand = unlines (map showCard hand)
  where
    showCard (Card r s)  = " " ++ showRank r ++ " of " ++ showSuit s ++ " "
    showRank (Numeric n) = show n
    showRank Jack        = "Jack"
    showRank Queen       = "Queen"
    showRank King        = "King"
    showRank Ace         = "Ace"

    showSuit Hearts      = "Hearts"  
    showSuit Spades      = "Spades"   
    showSuit Diamonds    = "Diamonds" 
    showSuit Clubs       = "Clubs"  

-- Task A3 --

valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank Jack        = 10
valueRank Queen       = 10
valueRank King        = 10
valueRank Ace         = 11

valueCard :: Card -> Int
valueCard (Card r _) = valueRank r

numberOfAces :: Hand -> Int
numberOfAces hand = length [ () | Card Ace _ <- hand ]

--returns the value of a hand in Blackjack
value :: Hand -> Int
value hand 
  | raw > 21 && aces > 0 = raw - 10 * aces    -- if sum exceeds 21 and there are aces, count all aces as 1 instead of 11
  | otherwise            = raw                -- otherwise return the raw value
  where 
    raw = sum (map valueCard hand)            -- total value counting all aces as 11
    aces = numberOfAces hand                  -- number of aces on the hand 

-- Task A4 --
-- returns true if the hand value exceeds 21
gameOver :: Hand -> Bool
gameOver h = value h > 21

--returns the winner of the game 
winner :: Hand -> Hand -> Player 
winner guest bank 
  | gameOver guest            = Bank 
  | gameOver bank             = Guest
  | value guest > value bank  = Guest 
  | otherwise                 = Bank 

--------------------------------------------------------------------------------
-- Part B
---------------------------------------------------------------------------------

-- Task B1 --
fullDeck :: Deck
fullDeck = [Card r s | r <- allRanks, s <- allSuits]
  where
    allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]
    allSuits = [Hearts, Spades, Diamonds, Clubs]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2 --

draw :: Deck -> Hand -> (Deck, Hand)
draw [] hand = error "draw: The deck is empty." -- error if the deck is empty
draw (card:deck) hand = (deck, card:hand) -- draw the top card from the deck and add it to the hand

-- Task B3 --

playBank :: Deck -> Hand
playBank deck = playBank' deck []      -- start with an empty hand

playBank' :: Deck -> Hand -> Hand      -- draw cards until the bank's hand value is >= 16
playBank' deck bankHand
  | value bankHand >= 16 = bankHand                  -- stop drawing if the bank's is >= 16
  | otherwise           = playBank' deck' bankHand'  -- continue drawing
  where 
    (deck', bankHand') = draw deck bankHand

-- Task B4 --

pick :: Double -> Deck -> Card
pick _ []   = error "pick: The deck is empty." -- error if the deck is empty
pick r deck = deck !! index                    -- pick the card at the calculated index
  where 
    n = length deck                    -- number of cards in the deck
    index = min (n - 1) (floor (r * fromIntegral n)) -- calculate the index based on the random number

removeCard :: Card -> Deck -> Deck    
removeCard _ [] = []                    -- if the deck is empty return an empty deck
removeCard c (x:xs)                     -- look for the card to remove
      | x == c    = xs                  -- remove the card when it occurs 
      | otherwise = x : removeCard c xs -- otherwise keep looking

shuffle :: [Double] -> Deck -> Deck
shuffle _ []          = []             -- if the deck is empty return an empty deck 
shuffle (r:rs) deck =
    chosen : shuffle rs remaining 
  where 
    chosen = pick r deck               -- pick a card based on the random number
    remaining = removeCard chosen deck -- remove the picked card from the deck 

runShuffle :: IO Deck
runShuffle = do
  Rand ds <- generate arbitrary
  return (shuffle ds fullDeck)

-- Task B5 --

belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) = 
  card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = 
  length (shuffle randomlist deck) == length deck 

-- Task B6 --
implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation   -- follow the instructions on Canvas

