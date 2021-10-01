{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Hearts.Card (Card (..), Suit (..), Value (..), score, allCards) where

import qualified Data.Aeson as Aeson
import Data.Monoid (Sum)
import GHC.Generics (Generic)

data Card = Card Suit Value
  deriving (Eq, Generic)

instance Show Card where
  show = \case
    Card Clubs Two -> "🃒"
    Card Clubs Three -> "🃓"
    Card Clubs Four -> "🃔"
    Card Clubs Five -> "🃕"
    Card Clubs Six -> "🃖"
    Card Clubs Seven -> "🃗"
    Card Clubs Eight -> "🃘"
    Card Clubs Nine -> "🃙"
    Card Clubs Ten -> "🃚"
    Card Clubs Jack -> "🃛"
    Card Clubs Queen -> "🃝"
    Card Clubs King -> "🃞"
    Card Clubs Ace -> "🃑"
    Card Diamonds Two -> "🃂"
    Card Diamonds Three -> "🃃"
    Card Diamonds Four -> "🃄"
    Card Diamonds Five -> "🃅"
    Card Diamonds Six -> "🃆"
    Card Diamonds Seven -> "🃇"
    Card Diamonds Eight -> "🃈"
    Card Diamonds Nine -> "🃉"
    Card Diamonds Ten -> "🃊"
    Card Diamonds Jack -> "🃋"
    Card Diamonds Queen -> "🃍"
    Card Diamonds King -> "🃎"
    Card Diamonds Ace -> "🃁"
    Card Spades Two -> "🂢"
    Card Spades Three -> "🂣"
    Card Spades Four -> "🂤"
    Card Spades Five -> "🂥"
    Card Spades Six -> "🂦"
    Card Spades Seven -> "🂧"
    Card Spades Eight -> "🂨"
    Card Spades Nine -> "🂩"
    Card Spades Ten -> "🂪"
    Card Spades Jack -> "🂫"
    Card Spades Queen -> "🂭"
    Card Spades King -> "🂮"
    Card Spades Ace -> "🂡"
    Card Hearts Two -> "🂲"
    Card Hearts Three -> "🂳"
    Card Hearts Four -> "🂴"
    Card Hearts Five -> "🂵"
    Card Hearts Six -> "🂶"
    Card Hearts Seven -> "🂷"
    Card Hearts Eight -> "🂸"
    Card Hearts Nine -> "🂹"
    Card Hearts Ten -> "🂺"
    Card Hearts Jack -> "🂻"
    Card Hearts Queen -> "🂽"
    Card Hearts King -> "🂾"
    Card Hearts Ace -> "🂱"

instance Aeson.ToJSON Card

data Suit
  = Clubs
  | Diamonds
  | Spades
  | Hearts
  deriving (Eq, Bounded, Enum, Generic)

instance Show Suit where
  show = \case
    Clubs -> "♣"
    Diamonds -> "♦"
    Spades -> "♠"
    Hearts -> "♥"

instance Aeson.ToJSON Suit

data Value
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Bounded, Enum, Generic)

instance Show Value where
  show = \case
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

instance Aeson.ToJSON Value

-- | The score taken for taking this card in a trick.
score :: Card -> Sum Integer
score = \case
  Card Spades Queen -> 13
  Card Hearts _ -> 1
  _ -> 0

allCards :: [Card]
allCards =
  Card
    <$> [minBound .. maxBound]
    <*> [minBound .. maxBound]
