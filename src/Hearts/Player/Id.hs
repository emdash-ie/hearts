{-# LANGUAGE DeriveGeneric #-}

module Hearts.Player.Id (Id (..)) where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData, parseQueryParam, toQueryParam)
import Text.Read (readEither)

newtype Id = Id Integer deriving (Show, Eq, Generic)

instance Aeson.ToJSON Id
instance Aeson.FromJSON Id

instance FromHttpApiData Id where
  parseQueryParam t = bimap Text.pack Id (readEither (Text.unpack t))

instance ToHttpApiData Id where
  toQueryParam (Id n) = Text.pack (show n)
