{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hearts.Room (
  Room (..),
  Event (..),
  FoldError (..),
  foldEvents,
  processEvent,
) where

import Control.Lens (over)
import qualified Data.Aeson as Aeson
import Data.Foldable (foldl')
import Data.Generics.Product (field)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

import qualified Hearts.Game as Game
import Hearts.Player (Player (Player))
import qualified Hearts.Player as Player

data Room = Room
  { name :: RoomName
  , players :: Vector Player
  , games :: Vector Game.ID
  }
  deriving (Generic)

instance Aeson.ToJSON Room

data Event
  = Create RoomName
  | Join Player.Id Username
  | Leave Player.Id
  | StartGame Game.ID
  deriving (Generic)

type Username = Text
type RoomName = Text

instance Aeson.ToJSON Event
instance Aeson.FromJSON Event

foldEvents ::
  Foldable f =>
  Maybe Room ->
  (Event, f Event) ->
  Either FoldError Room
foldEvents room (event, events) =
  foldl' fold (processEvent room event) events
  where
    fold ::
      Either FoldError Room ->
      Event ->
      Either FoldError Room
    fold = either (const . Left) (processEvent . Just)

processEvent :: Maybe Room -> Event -> Either FoldError Room
processEvent = curry \case
  (Nothing, Create name) ->
    Right Room{name, players = Vector.empty, games = Vector.empty}
  (Just _, Create _) ->
    Left RoomAlreadyCreated
  (Nothing, _) ->
    Left RoomNotCreated
  (Just Room{..}, Join newId username) ->
    if newId `elem` (Player.id <$> players)
      then Left IdAlreadyTaken
      else Right (Room{players = Vector.snoc players (Player newId username), ..})
  (Just Room{..}, Leave player) ->
    if player `elem` (Player.id <$> players)
      then Right (Room{players = Vector.filter ((/= player) . Player.id) players, ..})
      else Left IdNotInRoom
  (Just room, StartGame gameId) ->
    Right (over (field @"games") (`Vector.snoc` gameId) room)

data FoldError
  = RoomNotCreated
  | RoomAlreadyCreated
  | IdNotInRoom
  | IdAlreadyTaken
  deriving (Show, Generic)

instance Aeson.ToJSON FoldError
