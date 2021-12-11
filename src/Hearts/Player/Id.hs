{-# LANGUAGE DeriveGeneric #-}

module Hearts.Player.Id (Id (..)) where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
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
