{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hearts.Card (
  Card (..),
  Suit (..),
  Value (..),
  score,
  allCards,
  sortCards,
  cardHtml,
  faceDownCard,
) where

import qualified Data.Aeson as Aeson
import Data.List (sortOn)
import Data.Monoid (Sum)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Lucid (HtmlT, ToHtml (..), class_, span_)
import Servant (FromHttpApiData, ToHttpApiData, toUrlPiece)
import Servant.API (parseUrlPiece)

data Card = Card {suit :: Suit, value :: Value}
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
instance Aeson.FromJSON Card

instance ToHtml Card where
  toHtml c@(Card s _) =
    let cardClass =
          case s of
            Clubs -> "clubs"
            Spades -> "spades"
            Diamonds -> "diamonds"
            Hearts -> "hearts"
     in cardHtml (Just cardClass) (Text.pack (show c))
  toHtmlRaw = toHtml

faceDownCard :: Text
faceDownCard = "🂠"

cardHtml :: Monad m => Maybe Text -> Text -> HtmlT m ()
cardHtml extraClass c =
  span_ [class_ ("card" <> maybe "" (" " <>) extraClass)] (toHtml c)

instance FromHttpApiData Card where
  parseUrlPiece p =
    let (suitText, valueText) = Text.splitAt 1 p
        s = case suitText of
          "C" -> Right Clubs
          "D" -> Right Diamonds
          "S" -> Right Spades
          "H" -> Right Hearts
          _ -> Left "Unrecognised suit"
        v = case valueText of
          "2" -> Right Two
          "3" -> Right Three
          "4" -> Right Four
          "5" -> Right Five
          "6" -> Right Six
          "7" -> Right Seven
          "8" -> Right Eight
          "9" -> Right Nine
          "10" -> Right Ten
          "J" -> Right Jack
          "Q" -> Right Queen
          "K" -> Right King
          "A" -> Right Ace
          _ -> Left "Unrecognised value"
     in Card <$> s <*> v

instance ToHttpApiData Card where
  toUrlPiece Card{suit, value} =
    let suitText = case suit of
          Clubs -> "C"
          Diamonds -> "D"
          Spades -> "S"
          Hearts -> "H"
        valueText = case value of
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
     in suitText <> valueText

data Suit
  = Clubs
  | Diamonds
  | Spades
  | Hearts
  deriving (Eq, Bounded, Enum, Ord, Generic)

instance Show Suit where
  show = \case
    Clubs -> "♣"
    Diamonds -> "♦"
    Spades -> "♠"
    Hearts -> "♥"

instance Aeson.ToJSON Suit
instance Aeson.FromJSON Suit

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
  deriving (Eq, Bounded, Enum, Ord, Generic)

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
instance Aeson.FromJSON Value

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

sortCards :: [Card] -> [Card]
sortCards = sortOn suit . sortOn value
