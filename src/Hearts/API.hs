{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Hearts.API (
  HeartsAPI,
  JoinResponse (..),
  APIResponse (..),
  Result (..),
  JoinResult (..),
) where

import qualified Hearts.Player as Player
import Hearts.Room (Room)

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Servant

type HeartsAPI =
  "join" :> Post '[JSON] JoinResponse

newtype JoinResponse = JoinResponse APIResponse
  deriving (Generic)

instance Aeson.ToJSON JoinResponse

data APIResponse = APIResponse
  { result :: Result
  , actions :: Vector Action
  }
  deriving (Generic)

instance Aeson.ToJSON APIResponse

newtype Result
  = Join JoinResult
  deriving (Generic)

instance Aeson.ToJSON Result

data JoinResult = JoinResult
  { room :: Room
  , assignedId :: Player.Id
  }
  deriving (Generic)

instance Aeson.ToJSON JoinResult

data Action = Action
  { name :: Text
  , description :: Text
  , url :: Text
  }
  deriving (Generic)

instance Aeson.ToJSON Action
