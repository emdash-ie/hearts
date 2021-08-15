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

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Random.Shuffle (shuffleM)

import Hearts.Card (Card)
import qualified Hearts.Card as Card
import Hearts.Player (FourPlayers, Player)
import qualified Hearts.Player as Player

data Event
  = -- | Start a game.
    Start StartEvent
  | -- | Deal a new hand.
    Deal DealEvent
  | -- | Play the next card in the current trick.
    Play PlayEvent
  deriving (Show)

newtype StartEvent = -- | Start a game with these players.
  --
  -- The order of the players in the vector is the play order.
  StartEvent {players :: FourPlayers Player.Id}
  deriving (Show)

newtype DealEvent = -- | Deal a new hand with this deck.
  DealEvent {deck :: Deck}
  deriving (Show)

newtype PlayEvent = -- | Play this card next in the current trick.
  PlayEvent {card :: Card}
  deriving (Show)

newtype Deck = Deck (Vector Card)
  deriving (Show)

shuffledDeck :: IO Deck
shuffledDeck = Deck . Vector.fromList <$> shuffleM Card.allCards
