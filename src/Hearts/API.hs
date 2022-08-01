{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
  ServerState (..),
) where

import Control.Lens (Identity (runIdentity), (%~), (^.))
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
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
import Lucid
import Servant hiding (Required)
import Servant.HTML.Lucid (HTML)
import Web.FormUrlEncoded (FromForm)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Hearts.Card (Card, cardHtml, faceDownCard)
import qualified Hearts.Game as Game
import qualified Hearts.Game.ID as Game.ID
import Hearts.Player (Player)
import qualified Hearts.Player as Player
import Hearts.Player.Event (DealEvent (..), StartEvent (..))
import Hearts.Room (Room (..))
import qualified Hearts.Room as Room

type HeartsAPI =
  Get '[JSON, HTML] RootResponse
    :<|> "static" :> Raw
    :<|> "state" :> Get '[JSON] ServerState
    :<|> "room" :> RoomAPI

data ServerState = ServerState
  { roomEvents :: Map Text (Vector Room.Event)
  , gameEvents :: Map Game.ID (Vector Game.Event)
  , nextGameID :: Game.ID
  , nextPlayerID :: Player.Id
  , staticPath :: String
  }
  deriving (Generic)

instance Aeson.ToJSON ServerState
instance Aeson.FromJSON ServerState

type RoomAPI =
  ReqBody '[JSON, FormUrlEncoded] JoinRequest
    :> PostRedirectGet '[JSON, HTML] RoomResponse
    :<|> Capture "roomName" Text
      :> ( "events"
            :> Get '[JSON] (Vector Room.Event)
            :<|> ( QueryParam "playerId" Player.Id
                    :> Get '[JSON, HTML] RoomResponse
                    :<|> "join"
                      :> ReqBody '[JSON, FormUrlEncoded] JoinRequest
                      :> PostRedirectGet '[JSON, HTML] RoomResponse
                    :<|> "game" :> GameAPI
                 )
         )

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
    :<|> Capture "gameID" Game.ID
      :> "events"
      :> Get '[JSON] (Vector Game.Event)
    :<|> Capture "gameID" Game.ID
      :> "eventsPlus"
      :> Get '[JSON] (Vector Game.Event)
    :<|> Capture "gameID" Game.ID
      :> "lastEvent"
      :> Get '[JSON] Game.Event

type PostRedirectGet contentTypes a =
  Verb 'POST 303 contentTypes (WithLocation a)

type WithLocation a = Headers '[Header "Location" Text] a

heartsPage ::
  Monad m =>
  Maybe (HtmlT m ()) ->
  Text ->
  HtmlT m a ->
  HtmlT m a
heartsPage subtitle cssPrefix mainHtml = do
  html_ do
    head_ do
      title_ ("Hearts" <> maybe "" (" - " <>) subtitle)
      link_ [rel_ "stylesheet", href_ (cssPrefix <> "static/hearts.css"), type_ "text/css"]
    body_ do
      main_ mainHtml

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
  | HiddenInput Name Value Required
  deriving (Show, Eq, Generic)

type Name = Text
type Label = Text
type Value = Text
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
  toHtml (HiddenInput name value required) = fieldset_ do
    input_
      [ type_ "hidden"
      , name_ name
      , value_ value
      , required_ (bool "false" "true" required)
      ]

  toHtmlRaw = toHtml

data RootResponse = RootResponse
  { rooms :: Vector (Text, Either Room.FoldError Room)
  , createRoom :: Action
  , joinRoom :: Map Text Action
  }
  deriving (Generic)

instance Aeson.ToJSON RootResponse

instance ToHtml RootResponse where
  toHtml RootResponse{rooms, createRoom, joinRoom} =
    heartsPage Nothing "./" $ do
      p_ "Welcome to the Hearts server!"
      p_ "You can join one of the following rooms to play:"
      ul_ do
        foldMap roomHtml rooms
      p_ "Alternatively, you can create a new room:"
      toHtml createRoom
    where
      roomHtml :: Monad m => (Text, Either Room.FoldError Room) -> HtmlT m ()
      roomHtml (name, _) = do
        li_ (toHtml name)
        maybe mempty toHtml (Map.lookup name joinRoom)
  toHtmlRaw = toHtml

data JoinRequest = JoinRequest
  { username :: Text
  , roomName :: Text
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
    heartsPage (Just "Room") "../../" do
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
          <> "/?playerId="
          <> toUrlPiece assignedId
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
      } = heartsPage (Just "Room created") "./" do
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
      , game = Player.Game{players, hand, scores, trick, lastTrick, finished}
      , playingNext
      , you
      , playCard
      } = heartsPage (Just "Game") "../../../../" do
      h1_ "Hearts"
      let username = Player.getPlayerData you usernames
      p_ ("Welcome to game " <> toHtml (Game.ID.toNumeral gameID) <> ", " <> toHtml username <> "!")
      when finished do
        p_ "This game is over!"
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
                if Just p == fmap (\next -> players ^. Player.playerData next) playingNext
                  then th_ [class_ "next"] (toHtml u)
                  else th_ (toHtml u)
          table_ do
            tr_ (foldMap f ((,) <$> players <*> usernames))
            tr_ (foldMap (td_ . maybe (cardHtml Nothing faceDownCard) toHtml) cards)
      case lastTrick of
        Nothing -> mempty
        Just t -> do
          let winner = usernames ^. Player.playerData (Game.winner t)
          h2_ ("Last trick: " <> toHtml winner <> " won")
          table_ do
            tr_ (foldMap (th_ . toHtml) usernames)
            tr_ (foldMap (td_ . toHtml . runIdentity) (snd t))
      fromMaybe (pure ()) do
        h <- hand
        pure do
          h2_ "Your hand:"
          displayHand (fullUrl playCard) h
  toHtmlRaw = toHtml

displayHand :: Monad m => Text -> Vector Card -> HtmlT m ()
displayHand url hand = form_
  [ action_ url
  , method_ "post"
  ]
  do
    Foldable.for_ hand \c -> do
      let i = Text.pack (show c)
      input_
        [ type_ "radio"
        , name_ "card"
        , id_ i
        , class_ "card-selector"
        , value_ (toQueryParam c)
        ]
      label_ [for_ i, class_ "card-selector"] (toHtml c)
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
