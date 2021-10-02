{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Whereas the events in Hearts.Game.Event represent a total view of the game,
-- these events represent a player's view of the game, with limited information.
module Hearts.Player.Event (
  Event (..),
  StartEvent (..),
  DealEvent (..),
  PlayEvent (..),
  fromGameEvent,
  fromGameStartEvent,
  fromGameDealEvent,
  fromGamePlayEvent,
) where

import qualified Data.Aeson as Aeson
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Hearts.Card (Card)
import Hearts.Game (Game (..))
import qualified Hearts.Game as Game
import qualified Hearts.Game.Event as Game
import Hearts.Player (FourPlayers)
import qualified Hearts.Player as Player

data Event
  = Start StartEvent
  | Deal DealEvent
  | Play PlayEvent
  deriving (Generic)

fromGameEvent :: Maybe Game -> Player.PlayerIndex -> Game.Event -> Maybe Event
fromGameEvent game playerIndex event = case (game, event) of
  (_, Game.Start startEvent) ->
    Just (Start (fromGameStartEvent startEvent))
  (Just _, Game.Deal dealEvent) ->
    Just (Deal (fromGameDealEvent playerIndex dealEvent))
  (Just Game{players}, Game.Play playEvent) ->
    Just (Play (fromGamePlayEvent playerIndex players playEvent))
  _ -> Nothing

fromGameStartEvent :: Game.StartEvent -> StartEvent
fromGameStartEvent Game.StartEvent{..} = StartEvent{..}

fromGameDealEvent :: Player.PlayerIndex -> Game.DealEvent -> DealEvent
fromGameDealEvent playerIndex Game.DealEvent{deck = Game.Deck deck} =
  let hand =
        Player.getPlayerData playerIndex (Game.sortHand <$> Game.dealAmong4 deck)
   in DealEvent{hand}

fromGamePlayEvent ::
  Player.PlayerIndex ->
  FourPlayers Player.Id ->
  Game.PlayEvent ->
  PlayEvent
fromGamePlayEvent playerIndex players Game.PlayEvent{card} =
  let player = Player.getPlayerData playerIndex players
   in PlayEvent{player, card}

newtype StartEvent = StartEvent
  {players :: FourPlayers Player.Id}
  deriving (Generic)

instance Aeson.ToJSON StartEvent

newtype DealEvent = -- | Deal a new hand. 'hand' is the cards this player receives.
  DealEvent {hand :: Vector Card}
  deriving (Generic)

instance Aeson.ToJSON DealEvent

data PlayEvent = PlayEvent
  {player :: Player.Id, card :: Card}
  deriving (Generic)

instance Aeson.ToJSON PlayEvent
