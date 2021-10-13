{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Hearts.Game.Event (
  Event (..),
  StartEvent (..),
  DealEvent (..),
  PlayEvent (..),
  Deck (..),
  shuffledDeck,
) where

import qualified Data.Aeson as Aeson
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffleM)

import Hearts.Card (Card)
import qualified Hearts.Card as Card
import Hearts.Player (FourPlayers)
import qualified Hearts.Player as Player

data Event
  = -- | Start a game.
    Start StartEvent
  | -- | Deal a new hand.
    Deal DealEvent
  | -- | Play the next card in the current trick.
    Play PlayEvent
  deriving (Show, Generic)

instance Aeson.ToJSON Event

newtype StartEvent = -- | Start a game with these players.
  --
  -- The order of the players in the vector is the play order.
  StartEvent {players :: FourPlayers Player.Id}
  deriving (Show, Generic)

instance Aeson.ToJSON StartEvent

newtype DealEvent = -- | Deal a new hand with this deck.
  DealEvent {deck :: Deck}
  deriving (Show, Generic)

instance Aeson.ToJSON DealEvent

newtype PlayEvent = -- | Play this card next in the current trick.
  PlayEvent {card :: Card}
  deriving (Show, Generic)

instance Aeson.ToJSON PlayEvent

newtype Deck = Deck (Vector Card)
  deriving (Show, Generic)

instance Aeson.ToJSON Deck

shuffledDeck :: IO Deck
shuffledDeck = Deck . Vector.fromList <$> shuffleM Card.allCards
