{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Hearts.Player (
  Player (..),
  Id (..),
  FourPlayers (..),
  fourWith,
  ThreePlayers (..),
  PlayerData (..),
) where

import Data.Functor (($>))
import GHC.Generics (Generic)
import Hearts.Player.Id

data Player = Player Id deriving (Show, Eq, Generic)

data PlayerData a where
  Four :: FourPlayers a -> PlayerData a
  Three :: ThreePlayers a -> PlayerData a

data FourPlayers a = FourPlayers
  { one :: a
  , two :: a
  , three :: a
  , four :: a
  }
  deriving (Show, Eq, Functor, Generic)

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
