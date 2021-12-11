{-# LANGUAGE DeriveGeneric #-}

module Hearts.Game.ID (ID, next, first, toNumeral) where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData, parseQueryParam, toQueryParam)

newtype ID = ID Integer
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON ID
instance Aeson.FromJSON ID

instance ToHttpApiData ID where
  toQueryParam (ID n) = Text.pack (show n)

instance FromHttpApiData ID where
  parseQueryParam t = bimap Text.pack (ID . fst) (decimal t)

next :: ID -> ID
next (ID n) = ID (n + 1)

first :: ID
first = ID 1

toNumeral :: ID -> Text
toNumeral (ID n) = Text.pack (show n)
