{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  GameResult (..),
  Action (..),
  Method (..),
  WithLocation,
) where

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Lucid (
  ToHtml (..),
  a_,
  action_,
  form_,
  h1_,
  h2_,
  href_,
  input_,
  li_,
  main_,
  method_,
  name_,
  nav_,
  p_,
  type_,
  ul_,
  value_,
 )
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
    :<|> "game"
      :> ( QueryParam "playerId" Player.Id
            :> PostRedirectGet '[JSON, HTML] (APIResponse CreateResult)
            :<|> QueryParam "playerId" Player.Id
              :> Capture "gameId" UUID
              :> Get '[JSON, HTML] (APIResponse GameResult)
         )

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
  , parameters :: [(Text, Text)]
  }
  deriving (Generic)

instance Aeson.ToJSON Action

instance ToHtml Action where
  toHtml Action{name, url, method, parameters} =
    form_
      [ action_
          ( case method of
              Get -> url
              Post -> url <> "?" <> foldMap (\(k, v) -> k <> "=" <> v <> "&") parameters
          )
      , method_
          ( case method of
              Get -> "get"
              Post -> "post"
          )
      ]
      ( input_ [type_ "submit", value_ name]
          <> ( case method of
                Get -> foldMap (\(k, v) -> input_ [type_ "hidden", name_ k, value_ v]) parameters
                Post -> mempty
             )
      )
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
    p_ "The games in this room so far are:"
    ul_ do
      foldMap (\g -> li_ (a_ [href_ (gameHref g)] (toHtml (show g)))) games
    where
      gameHref :: UUID -> Text
      gameHref g =
        "game/" <> Text.pack (show g)
          <> "?playerId="
          <> Text.pack (show (coerce assignedId :: Integer))

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

data GameResult = GameResult
  { gameId :: UUID
  , game :: Player.Game
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON GameResult

instance ToHtml GameResult where
  toHtml
    GameResult
      { gameId
      , game = Player.Game{players, hand, scores}
      } = do
      h1_ ("Game " <> toHtml (show gameId))
      p_ ("The players in this game are: " <> toHtml (show players))
      p_ ("The scores are: " <> toHtml (show scores))
      h2_ "Your hand"
      foldMap (foldMap toHtml) hand
  toHtmlRaw = toHtml
