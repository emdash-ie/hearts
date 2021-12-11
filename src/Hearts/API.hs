{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hearts.API (
  HeartsAPI,
  RootResponse (..),
  JoinRequest (..),
  RoomResponse (..),
  CreateResult (..),
  GameResult (..),
  CardSelection (..),
  PlayResult (..),
  Action (..),
  Method (..),
  Input (..),
  WithLocation,
) where

import Control.Lens (Identity (runIdentity), (%~), (^.))
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.Coerce (coerce)
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import Data.Generics.Product (field)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Lucid (
  HtmlT,
  ToHtml (..),
  a_,
  action_,
  class_,
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

import Hearts.Card (Card, cardHtml, faceDownCard)
import qualified Hearts.Game as Game
import qualified Hearts.Game.ID as Game.ID
import Hearts.Player (Player)
import qualified Hearts.Player as Player
import Hearts.Player.Event (DealEvent (..), StartEvent (..))
import Hearts.Room (Room (..))

type HeartsAPI =
  Get '[JSON, HTML] RootResponse
    :<|> "static" :> Raw
    :<|> "join"
      :> ReqBody '[JSON, FormUrlEncoded] JoinRequest
      :> PostRedirectGet '[JSON, HTML] RoomResponse
    :<|> "room" :> QueryParam "playerId" Player.Id
      :> Get '[JSON, HTML] RoomResponse
    :<|> "game" :> GameAPI

type GameAPI =
  QueryParam "playerId" Player.Id
    :> PostRedirectGet '[JSON, HTML] CreateResult
    :<|> QueryParam "playerId" Player.Id
      :> Capture "gameID" Game.ID
      :> Get '[JSON, HTML] GameResult
    :<|> QueryParam "playerId" Player.Id
      :> Capture "gameID" Game.ID
      :> "play"
      :> ReqBody '[FormUrlEncoded] CardSelection
      :> PostRedirectGet '[JSON, HTML] PlayResult

type PostRedirectGet contentTypes a =
  Verb 'POST 303 contentTypes (WithLocation a)

type WithLocation a = Headers '[Header "Location" Text] a

withCSS :: Monad m => Text -> HtmlT m a -> HtmlT m a
withCSS p h = do
  link_ [rel_ "stylesheet", href_ (p <> "static/hearts.css"), type_ "text/css"]
  h

data Action = Action
  { name :: Text
  , description :: Text
  , url :: Text
  , method :: Method
  , parameters :: [(Text, Text)]
  , inputs :: [Input]
  }
  deriving (Show, Eq, Generic)

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

fullUrl :: Action -> Text
fullUrl Action{url, parameters} =
  url <> "?" <> foldMap (\(k, v) -> k <> "=" <> v <> "&") parameters

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

newtype RootResponse = RootResponse
  { joinRoom :: Action
  }
  deriving (Generic)

instance Aeson.ToJSON RootResponse

instance ToHtml RootResponse where
  toHtml RootResponse{joinRoom} = withCSS "./" $ main_ do
    p_ "Welcome to the Hearts server!"
    p_ "Please join a room to play a game:"
    toHtml joinRoom
  toHtmlRaw = toHtml

newtype JoinRequest = JoinRequest
  { username :: Text
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON JoinRequest

instance FromForm JoinRequest

data RoomResponse = RoomResponse
  { room :: Room
  , assignedId :: Player.Id
  , refresh :: Action
  , startGame :: Maybe Action
  }
  deriving (Generic)

instance Aeson.ToJSON RoomResponse

instance ToHtml RoomResponse where
  toHtml RoomResponse{room = Room{players, games}, assignedId, startGame, refresh} =
    withCSS "./" $ main_ do
      p_ ("Your assigned ID is: " <> toHtml (show assignedId))
      p_ ("The players in this room are: " <> toHtml (show players))
      unless (Vector.null games) do
        p_ "The games in this room so far are:"
        ul_ do
          foldMap (\g -> li_ (a_ [href_ (gameHref g)] (toHtml ("Game " <> Game.ID.toNumeral g)))) games
      ul_ do
        maybe mempty toHtml startGame
        li_ (a_ [href_ (fullUrl refresh)] "Check for updates")
    where
      gameHref :: Game.ID -> Text
      gameHref g =
        "game/" <> toUrlPiece g
          <> "?playerId="
          <> Text.pack (show (coerce assignedId :: Integer))
  toHtmlRaw = toHtml

data CreateResult = CreateResult
  { gameID :: Game.ID
  , startEvent :: StartEvent
  , dealEvent :: DealEvent
  }
  deriving (Generic)

instance Aeson.ToJSON CreateResult

instance ToHtml CreateResult where
  toHtml
    CreateResult
      { gameID
      , startEvent = StartEvent{players}
      , dealEvent = DealEvent{hand}
      } = withCSS "./" $ main_ do
      p_ ("Created a game with ID: " <> toHtml (show gameID))
      p_ ("The players in this game are: " <> toHtml (show players))
      p_ ("Your hand is: " <> toHtml (show hand))
  toHtmlRaw = toHtml

data GameResult = GameResult
  { gameID :: Game.ID
  , usernames :: Player.FourPlayers Text
  , game :: Player.Game
  , playingNext :: Maybe Player.PlayerIndex
  , you :: Player.PlayerIndex
  , playCard :: Action
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON GameResult

instance ToHtml GameResult where
  toHtml
    GameResult
      { gameID
      , usernames
      , game = Player.Game{players, hand, scores, trick, tricks}
      , playingNext
      , you
      , playCard
      } = withCSS "../" do
      h1_ "Hearts"
      let username = Player.getPlayerData you usernames
      p_ ("Welcome to game " <> toHtml (Game.ID.toNumeral gameID) <> ", " <> toHtml username <> "!")
      h2_ "Scores:"
      toHtml (ScoreTable (you, (,) <$> usernames <*> scores))
      case trick of
        Nothing -> mempty
        Just (_, cards) -> do
          let nextUsername = case playingNext of
                Nothing -> ""
                Just next -> usernames ^. Player.playerData next
          let statusMessage =
                if Just you == playingNext
                  then "you’re up!"
                  else nextUsername <> " is playing next"
          h2_ (toHtml ("Current trick: " <> statusMessage))
          let f :: Monad m => (Player.Id, Text) -> HtmlT m ()
              f (p, u) =
                if Just p == fmap ((players ^.) . Player.playerData) playingNext
                  then th_ [class_ "next"] (toHtml u)
                  else th_ (toHtml u)
          table_ do
            tr_ (foldMap f ((,) <$> players <*> usernames))
            tr_ (foldMap (td_ . maybe (cardHtml "black" faceDownCard) toHtml) cards)
      fromMaybe (pure ()) do
        h <- hand
        pure do
          h2_ "Your hand:"
          displayHand (fullUrl playCard) h
      case tricks of
        Nothing -> mempty
        Just ts -> do
          h2_ "Past tricks:"
          let makeRow :: Monad m => Player.Trick Identity -> HtmlT m ()
              makeRow (_, cards) = tr_ (foldMap (td_ . toHtml . runIdentity) cards)
          table_ do
            tr_ (foldMap (th_ . toHtml) usernames)
            tr_ (foldMap makeRow (Vector.reverse ts))
  toHtmlRaw = toHtml

displayHand :: Monad m => Text -> Vector Card -> HtmlT m ()
displayHand url hand = form_
  [ action_ url
  , method_ "post"
  ]
  do
    Foldable.for_ hand \c -> do
      let i = Text.pack (show c)
      label_ [for_ i] (toHtml c)
      input_
        [ type_ "radio"
        , name_ "card"
        , id_ i
        , value_ (toQueryParam c)
        ]
    input_ [type_ "submit", value_ "Play card"]

newtype PlayerList = PlayerList (Player.PlayerIndex, Player.FourPlayers Player)

instance ToHtml PlayerList where
  toHtml (PlayerList (you, players)) =
    let players' = players & Player.playerData you . field @"username" %~ (<> " (you)")
        usernames = fmap (players' ^.) [field @"one", field @"two", field @"three", field @"four"]
     in ul_
          ( foldMap
              (li_ . toHtml . Player.username)
              usernames
          )
  toHtmlRaw = toHtml

newtype ScoreTable = ScoreTable (Player.PlayerIndex, Player.FourPlayers (Text, Sum Integer))

instance ToHtml ScoreTable where
  toHtml (ScoreTable (you, fp)) =
    table_ do
      let ps :: [(Text, Sum Integer)]
          ps = fmap (fp ^.) [field @"one", field @"two", field @"three", field @"four"]
          makeHead :: Monad m => Text -> HtmlT m ()
          makeHead p =
            if p == fst (Player.getPlayerData you fp)
              then th_ [class_ "you"] (toHtml p)
              else th_ (toHtml p)
      thead_ (tr_ (foldMap (makeHead . fst) ps))
      tbody_ (tr_ (foldMap (td_ . toHtml . show . getSum . snd) ps))
  toHtmlRaw = toHtml

newtype CardSelection = CardSelection
  {card :: Card}
  deriving (Show, Eq, Generic)

instance FromForm CardSelection

newtype PlayResult = PlayResult ()
  deriving (Show, Generic)

instance Aeson.ToJSON PlayResult

instance ToHtml PlayResult where
  toHtml _ = mempty
  toHtmlRaw = toHtml
