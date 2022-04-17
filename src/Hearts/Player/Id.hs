{-# LANGUAGE DeriveGeneric #-}

module Hearts.Player.Id (Id, first, next, toNumeral) where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData, parseQueryParam, toQueryParam)

newtype Id = Id Integer deriving (Show, Eq, Generic)

instance Aeson.ToJSON Id
instance Aeson.FromJSON Id

instance FromHttpApiData Id where
  parseQueryParam t = bimap Text.pack (Id . fst) (decimal t)

instance ToHttpApiData Id where
  toQueryParam (Id n) = Text.pack (show n)

first :: Id
first = Id 1

next :: Id -> Id
next (Id n) = Id (n + 1)

toNumeral :: Id -> Text
toNumeral (Id n) = Text.pack (show n)
