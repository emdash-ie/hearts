{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hearts.Player (
  Player (..),
  Id,
  Game (..),
  Trick,
  FourPlayers (..),
  fourWith,
  ThreePlayers (..),
  PlayerData (..),
  PlayerIndex (..),
  getPlayerData,
  takeFour,
  findIndex,
  setPlayerData,
  playerData,
  playOrder,
  RoundInProgress (..),
  Round (..),
) where

import Control.Lens (Lens', lens)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Functor (($>))
import Data.Functor.Identity
import Data.Monoid (Sum (getSum))
import Data.Text (Text)
import Data.Vector (Vector, (!?))
import GHC.Generics (Generic)

import Hearts.Card (Card)
import Hearts.Player.Id

data Game = Game
  { players :: FourPlayers Id
  , scores :: FourPlayers (Sum Integer)
  , currentRound :: Maybe RoundInProgress
  , pastRounds :: Vector Round
  , finished :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Game where
  toJSON Game{..} =
    Aeson.object
      [ "players" .= players
      , "scores" .= fmap getSum scores
      , "currentRound" .= currentRound
      , "pastRounds" .= pastRounds
      , "finished" .= finished
      ]

data RoundInProgress = RoundInProgress
  { hand :: Maybe (Vector Card)
  , trick :: Maybe (Trick Maybe)
  , lastTrick :: Maybe (Trick Identity)
  , heartsBroken :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON RoundInProgress

data Round = Round
  { tricks :: Vector (Trick Identity)
  , scores :: FourPlayers (Sum Integer)
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Round where
  toJSON Round{..} =
    Aeson.object
      [ "tricks" .= tricks
      , "scores" .= fmap getSum scores
      ]

type Trick m =
  -- | (Starting player, cards)
  (PlayerIndex, FourPlayers (m Card))

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
  deriving (Show, Eq, Enum, Bounded, Generic)

instance Aeson.ToJSON PlayerIndex

findIndex :: (a -> Bool) -> FourPlayers a -> Maybe PlayerIndex
findIndex p FourPlayers{..} =
  if
      | p one -> Just PlayerOne
      | p two -> Just PlayerTwo
      | p three -> Just PlayerThree
      | p four -> Just PlayerFour
      | otherwise -> Nothing

playOrder :: PlayerIndex -> FourPlayers a -> [a]
playOrder index pd =
  let as = (`getPlayerData` pd) <$> cycle [minBound .. maxBound]
   in take
        (fromEnum (maxBound :: PlayerIndex) - fromEnum (minBound :: PlayerIndex) + 1)
        (drop (fromEnum index) as)

playerData :: PlayerIndex -> Lens' (FourPlayers a) a
playerData i = lens (getPlayerData i) (setPlayerData i)

getPlayerData :: PlayerIndex -> FourPlayers a -> a
getPlayerData = \case
  PlayerOne -> one
  PlayerTwo -> two
  PlayerThree -> three
  PlayerFour -> four

setPlayerData :: PlayerIndex -> FourPlayers a -> a -> FourPlayers a
setPlayerData i ps a = case i of
  PlayerOne -> ps{one = a}
  PlayerTwo -> ps{two = a}
  PlayerThree -> ps{three = a}
  PlayerFour -> ps{four = a}

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
instance Aeson.FromJSON a => Aeson.FromJSON (FourPlayers a)

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
