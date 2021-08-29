{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Hearts.Room (
  Room (..),
  Event (..),
  FoldError (..),
  foldEvents,
  processEvent,
) where

import qualified Hearts.Player as Player

import qualified Data.Aeson as Aeson
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

newtype Room = Room
  {players :: Vector Player.Id}
  deriving (Generic)

instance Aeson.ToJSON Room

data Event
  = Join Player.Id
  | Leave Player.Id

foldEvents :: Foldable f => Maybe Room -> f Event -> Either FoldError Room
foldEvents room = foldl' fold (Right (fromMaybe (Room Vector.empty) room))
  where
    fold ::
      Either FoldError Room ->
      Event ->
      Either FoldError Room
    fold (Right r) event = processEvent r event
    fold e@(Left _) _ = e

processEvent :: Room -> Event -> Either FoldError Room
processEvent Room{..} = \case
  Join newPlayer ->
    if newPlayer `elem` players
      then Left IdAlreadyTaken
      else Right (Room{players = Vector.snoc players newPlayer})
  Leave player ->
    if player `elem` players
      then Right (Room{players = Vector.filter (/= player) players})
      else Left IdNotInRoom

data FoldError
  = IdNotInRoom
  | IdAlreadyTaken
  deriving (Show, Generic)

instance Aeson.ToJSON FoldError
