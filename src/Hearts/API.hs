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
  JoinRequest (..),
  JoinResult (..),
  JoinResponse,
  CreateResult (..),
  CreateResponse,
  GameResult (..),
  PlayResult (..),
  Action (..),
  Method (..),
  Input (..),
  WithLocation,
) where

import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Lucid (
  ToHtml (..),
  a_,
  action_,
  fieldset_,
  for_,
  form_,
  h1_,
  h2_,
  href_,
  id_,
  input_,
  label_,
  li_,
  link_,
  main_,
  method_,
  name_,
  nav_,
  p_,
  rel_,
  required_,
  table_,
  tbody_,
  td_,
  th_,
  thead_,
  tr_,
  type_,
  ul_,
  value_,
 )
import Servant hiding (Required)
import Servant.HTML.Lucid (HTML)
import Web.FormUrlEncoded (FromForm)

import Hearts.Card (Card)
import Hearts.Player (Player (Player))
import qualified Hearts.Player as Player
import Hearts.Player.Event (DealEvent (..), StartEvent (..))
import Hearts.Room (Room (..))

type HeartsAPI =
  Get '[JSON, HTML] (APIResponse RootResult)
    :<|> "static" :> Raw
    :<|> "join"
      :> ReqBody '[JSON, FormUrlEncoded] JoinRequest
      :> PostRedirectGet '[JSON, HTML] (APIResponse JoinResult)
    :<|> "room" :> QueryParam "playerId" Player.Id
      :> Get '[JSON, HTML] (APIResponse JoinResult)
    :<|> "game" :> GameAPI

type GameAPI =
  QueryParam "playerId" Player.Id
    :> PostRedirectGet '[JSON, HTML] (APIResponse CreateResult)
    :<|> QueryParam "playerId" Player.Id
      :> Capture "gameId" UUID
      :> Get '[JSON, HTML] (APIResponse GameResult)
    :<|> QueryParam "playerId" Player.Id
      :> Capture "gameId" UUID
      :> "play"
      :> Capture "card" Card
      :> PostRedirectGet '[JSON, HTML] (APIResponse PlayResult)

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
    link_ [rel_ "stylesheet", href_ "static/hearts.css", type_ "text/css"]
    main_ (toHtml result)
    nav_ (foldMap toHtml actions)
  toHtmlRaw = toHtml

data Action = Action
  { name :: Text
  , description :: Text
  , url :: Text
  , method :: Method
  , parameters :: [(Text, Text)]
  , inputs :: [Input]
  }
  deriving (Generic)

instance Aeson.ToJSON Action

instance ToHtml Action where
  toHtml Action{name, url, method, parameters, inputs} =
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
      ( ( case method of
            Get -> foldMap (\(k, v) -> input_ [type_ "hidden", name_ k, value_ v]) parameters
            Post -> mempty
        )
          <> foldMap toHtml inputs
          <> input_ [type_ "submit", value_ name]
      )
  toHtmlRaw = toHtml

data Method
  = Get
  | Post
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Method

data Input
  = TextInput Name Label Required
  deriving (Show, Eq, Generic)

type Name = Text
type Label = Text
type Required = Bool

instance Aeson.ToJSON Input

instance ToHtml Input where
  toHtml (TextInput name label required) = fieldset_ do
    label_ [for_ name] (toHtml label)
    input_
      [ type_ "text"
      , name_ name
      , id_ name
      , required_ (bool "false" "true" required)
      ]

  toHtmlRaw = toHtml

newtype RootResult = RootResult ()
  deriving (Generic)

instance Aeson.ToJSON RootResult

instance ToHtml RootResult where
  toHtml _ = p_ "Welcome to the Hearts server!"
  toHtmlRaw = toHtml

newtype JoinRequest = JoinRequest
  { username :: Text
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON JoinRequest

instance FromForm JoinRequest

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
  , usernames :: Player.FourPlayers Text
  , game :: Player.Game
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON GameResult

instance ToHtml GameResult where
  toHtml
    GameResult
      { gameId
      , usernames
      , game = Player.Game{players, hand, scores}
      } = do
      let ps = Player <$> players <*> usernames
      h1_ ("Game " <> toHtml (show gameId))
      p_ "The players in this game are:"
      toHtml (PlayerList ps)
      h2_ "Scores:"
      toHtml (ScoreTable ((,) <$> usernames <*> scores))
      h2_ "Your hand:"
      foldMap (foldMap toHtml) hand
  toHtmlRaw = toHtml

newtype PlayerList = PlayerList (Player.FourPlayers Player)

instance ToHtml PlayerList where
  toHtml (PlayerList Player.FourPlayers{one, two, three, four}) =
    ul_
      ( foldMap
          (li_ . toHtml . Player.username)
          [one, two, three, four]
      )
  toHtmlRaw = toHtml

newtype ScoreTable = ScoreTable (Player.FourPlayers (Text, Sum Integer))

instance ToHtml ScoreTable where
  toHtml (ScoreTable Player.FourPlayers{one, two, three, four}) =
    table_ do
      let ps = [one, two, three, four]
      thead_ (tr_ (foldMap (th_ . toHtml . fst) ps))
      tbody_ (tr_ (foldMap (td_ . toHtml . show . getSum . snd) ps))
  toHtmlRaw = toHtml

newtype PlayResult = PlayResult ()
  deriving (Show, Generic)

instance Aeson.ToJSON PlayResult

instance ToHtml PlayResult where
  toHtml _ = mempty
  toHtmlRaw = toHtml
