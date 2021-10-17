{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hearts.Player (
  Player (..),
  Id (..),
  Game (..),
  FourPlayers (..),
  fourWith,
  ThreePlayers (..),
  PlayerData (..),
  PlayerIndex (..),
  getPlayerData,
  takeFour,
  findIndex,
) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Functor (($>))
import Data.Monoid (Sum (getSum))
import Data.Text (Text)
import Data.Vector (Vector, (!?))
import GHC.Generics (Generic)

import Hearts.Card (Card)
import Hearts.Player.Id

data Game = Game
  { players :: FourPlayers Id
  , scores :: FourPlayers (Sum Integer)
  , hand :: Maybe (Vector Card)
  , trick :: Maybe (FourPlayers (Maybe Card))
  , tricks :: Maybe (FourPlayers (Vector Card))
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Game where
  toJSON Game{..} =
    Aeson.object
      [ "players" .= players
      , "scores" .= fmap getSum scores
      , "hand" .= hand
      , "trick" .= trick
      , "tricks" .= tricks
      ]

data Player = Player
  { id :: Id
  , username :: Username
  }
  deriving (Show, Eq, Generic)

type Username = Text

instance Aeson.ToJSON Player

data PlayerIndex
  = PlayerOne
  | PlayerTwo
  | PlayerThree
  | PlayerFour

findIndex :: (a -> Bool) -> FourPlayers a -> Maybe PlayerIndex
findIndex p FourPlayers{..} =
  if
      | p one -> Just PlayerOne
      | p two -> Just PlayerTwo
      | p three -> Just PlayerThree
      | p four -> Just PlayerFour
      | otherwise -> Nothing

getPlayerData :: PlayerIndex -> FourPlayers a -> a
getPlayerData = \case
  PlayerOne -> one
  PlayerTwo -> two
  PlayerThree -> three
  PlayerFour -> four

data PlayerData a where
  Four :: FourPlayers a -> PlayerData a
  Three :: ThreePlayers a -> PlayerData a

takeFour :: Vector a -> Maybe (FourPlayers a)
takeFour xs = do
  one <- xs !? 0
  two <- xs !? 1
  three <- xs !? 2
  four <- xs !? 3
  pure FourPlayers{..}

data FourPlayers a = FourPlayers
  { one :: a
  , two :: a
  , three :: a
  , four :: a
  }
  deriving (Show, Eq, Functor, Foldable, Generic)

instance Semigroup a => Semigroup (FourPlayers a) where
  FourPlayers{one = left1, two = left2, three = left3, four = left4}
    <> FourPlayers{one = right1, two = right2, three = right3, four = right4} =
      FourPlayers
        { one = left1 <> right1
        , two = left2 <> right2
        , three = left3 <> right3
        , four = left4 <> right4
        }

instance Monoid a => Monoid (FourPlayers a) where
  mempty = FourPlayers mempty mempty mempty mempty

instance Applicative FourPlayers where
  pure a = fourWith a
  FourPlayers{one = left1, two = left2, three = left3, four = left4}
    <*> FourPlayers{one = right1, two = right2, three = right3, four = right4} =
      FourPlayers
        { one = left1 right1
        , two = left2 right2
        , three = left3 right3
        , four = left4 right4
        }

instance Aeson.ToJSON a => Aeson.ToJSON (FourPlayers a)

data ThreePlayers a = ThreePlayers
  { one :: a
  , two :: a
  , three :: a
  }
  deriving (Show, Functor, Generic)

fourWith :: a -> FourPlayers a
fourWith a = mempty @(FourPlayers ()) $> a

instance Semigroup a => Semigroup (ThreePlayers a) where
  ThreePlayers{one = left1, two = left2, three = left3}
    <> ThreePlayers{one = right1, two = right2, three = right3} =
      ThreePlayers
        { one = left1 <> right1
        , two = left2 <> right2
        , three = left3 <> right3
        }

instance Monoid a => Monoid (ThreePlayers a) where
  mempty = ThreePlayers mempty mempty mempty
