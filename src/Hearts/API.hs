{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Hearts.API (
  HeartsAPI,
  APIResponse (..),
  JoinResult (..),
  JoinResponse,
  CreateResult (..),
  CreateResponse,
  Action (..),
) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Servant

import qualified Hearts.Player as Player
import Hearts.Player.Event (DealEvent, StartEvent)
import Hearts.Room (Room)

type HeartsAPI =
  "join" :> Post '[JSON] (APIResponse JoinResult)
    :<|> "game" :> QueryParam "id" Player.Id
      :> Post '[JSON] (APIResponse CreateResult)

data APIResponse result = APIResponse
  { result :: result
  , actions :: Vector Action
  }
  deriving (Generic)

instance Aeson.ToJSON result => Aeson.ToJSON (APIResponse result)

data Action = Action
  { name :: Text
  , description :: Text
  , url :: Text
  }
  deriving (Generic)

instance Aeson.ToJSON Action

type JoinResponse = APIResponse JoinResult

data JoinResult = JoinResult
  { room :: Room
  , assignedId :: Player.Id
  }
  deriving (Generic)

instance Aeson.ToJSON JoinResult

type CreateResponse = APIResponse CreateResult

data CreateResult = CreateResult
  { gameId :: UUID
  , startEvent :: StartEvent
  , dealEvent :: DealEvent
  }
  deriving (Generic)

instance Aeson.ToJSON CreateResult
