-- | Whereas the events in Hearts.Game.Event represent a total view of the game,
-- these events represent a player's view of the game, with limited information.
module Hearts.Game.Player.Event (
  Event (..),
  StartEvent (..),
  DealEvent (..),
  PlayEvent (..),
) where

import Data.Vector (Vector)

import Hearts.Card (Card)
import qualified Hearts.Player as Player

data Event
  = Start StartEvent
  | Deal DealEvent
  | Play PlayEvent

newtype StartEvent = StartEvent {players :: Vector Player.Id}

newtype DealEvent = -- | Deal a new hand. 'hand' is the cards this player receives.
  DealEvent {hand :: Vector Card}

data PlayEvent = PlayEvent {player :: Player.Id, card :: Card}
