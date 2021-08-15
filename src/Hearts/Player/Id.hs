{-# LANGUAGE DeriveGeneric #-}

module Hearts.Player.Id (Id (..)) where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

newtype Id = Id Integer deriving (Show, Eq, Generic)

instance Aeson.ToJSON Id
