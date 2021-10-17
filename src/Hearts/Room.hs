{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

import Hearts.Player (Player (Player))
import qualified Hearts.Player as Player

data Room = Room
  { players :: Vector Player
  , games :: Vector UUID
  }
  deriving (Generic)

instance Aeson.ToJSON Room

data Event
  = Join Player.Id Username
  | Leave Player.Id
  | StartGame UUID

type Username = Text

foldEvents :: Foldable f => Maybe Room -> f Event -> Either FoldError Room
foldEvents room = foldl' fold (Right (fromMaybe (Room Vector.empty Vector.empty) room))
  where
    fold ::
      Either FoldError Room ->
      Event ->
      Either FoldError Room
    fold = either (const . Left) processEvent

processEvent :: Room -> Event -> Either FoldError Room
processEvent room@Room{..} = \case
  Join newId username ->
    if newId `elem` (Player.id <$> players)
      then Left IdAlreadyTaken
      else Right (Room{players = Vector.snoc players (Player newId username), ..})
  Leave player ->
    if player `elem` (Player.id <$> players)
      then Right (Room{players = Vector.filter ((/= player) . Player.id) players, ..})
      else Left IdNotInRoom
  StartGame gameId ->
    Right (over (field @"games") (`Vector.snoc` gameId) room)

data FoldError
  = IdNotInRoom
  | IdAlreadyTaken
  deriving (Show, Generic)

instance Aeson.ToJSON FoldError
