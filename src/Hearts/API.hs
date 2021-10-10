{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hearts.API (
  HeartsAPI,
  APIResponse (..),
  RootResult (..),
  JoinResult (..),
  JoinResponse,
  CreateResult (..),
  CreateResponse,
  Action (..),
  Method (..),
  WithLocation,
) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Lucid (ToHtml (..), action_, form_, input_, main_, method_, nav_, p_, type_, value_)
import Servant
import Servant.HTML.Lucid (HTML)

import qualified Hearts.Player as Player
import Hearts.Player.Event (DealEvent (..), StartEvent (..))
import Hearts.Room (Room (..))

type HeartsAPI =
  Get '[JSON, HTML] (APIResponse RootResult)
    :<|> "join"
      :> PostRedirectGet '[JSON, HTML] (APIResponse JoinResult)
    :<|> "room" :> QueryParam "playerId" Player.Id
      :> Get '[JSON, HTML] (APIResponse JoinResult)
    :<|> "game" :> QueryParam "playerId" Player.Id
      :> Post '[JSON, HTML] (APIResponse CreateResult)

type PostRedirectGet contentTypes a =
  Verb 'POST 303 contentTypes (WithLocation a)

type WithLocation a = Headers '[Header "Location" Text] a

data APIResponse result = APIResponse
  { result :: result
  , actions :: Vector Action
  }
  deriving (Generic)

instance Aeson.ToJSON result => Aeson.ToJSON (APIResponse result)

instance ToHtml result => ToHtml (APIResponse result) where
  toHtml APIResponse{actions, result} = do
    nav_ (foldMap toHtml actions)
    main_ (toHtml result)
  toHtmlRaw = toHtml

data Action = Action
  { name :: Text
  , description :: Text
  , url :: Text
  , method :: Method
  }
  deriving (Generic)

instance Aeson.ToJSON Action

instance ToHtml Action where
  toHtml Action{name, url, method} =
    form_
      [ action_ url
      , method_
          ( case method of
              Get -> "get"
              Post -> "post"
          )
      ]
      (input_ [type_ "submit", value_ name])
  toHtmlRaw = toHtml

data Method
  = Get
  | Post
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Method

newtype RootResult = RootResult ()
  deriving (Generic)

instance Aeson.ToJSON RootResult

instance ToHtml RootResult where
  toHtml _ = mempty
  toHtmlRaw = toHtml

type JoinResponse = APIResponse JoinResult

data JoinResult = JoinResult
  { room :: Room
  , assignedId :: Player.Id
  }
  deriving (Generic)

instance Aeson.ToJSON JoinResult

instance ToHtml JoinResult where
  toHtml JoinResult{room = Room{players, games}, assignedId} = do
    p_ ("Your assigned ID is: " <> toHtml (show assignedId))
    p_ ("The players in this room are: " <> toHtml (show players))
    p_ ("The games in this room so far are: " <> toHtml (show games))
  toHtmlRaw = toHtml

type CreateResponse = APIResponse CreateResult

data CreateResult = CreateResult
  { gameId :: UUID
  , startEvent :: StartEvent
  , dealEvent :: DealEvent
  }
  deriving (Generic)

instance Aeson.ToJSON CreateResult

instance ToHtml CreateResult where
  toHtml
    CreateResult
      { gameId
      , startEvent = StartEvent{players}
      , dealEvent = DealEvent{hand}
      } = do
      p_ ("Created a game with ID: " <> toHtml (show gameId))
      p_ ("The players in this game are: " <> toHtml (show players))
      p_ ("Your hand is: " <> toHtml (show hand))
  toHtmlRaw = toHtml
