{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hearts.Server (runServer) where

import qualified Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap, first)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Conc (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import Hearts.API (HeartsAPI, RoomResponse (..))
import qualified Hearts.API as API
import Hearts.Game (Game (Game))
import qualified Hearts.Game as Game
import qualified Hearts.Game.Event as Event
import qualified Hearts.Game.ID as Game.ID
import qualified Hearts.Player as Player
import qualified Hearts.Player.Event as PlayerEvent
import Hearts.Player.Id (Id (..))
import Hearts.Room (Room (Room))
import qualified Hearts.Room as Room

runServer :: IO ()
runServer = do
  roomVar <- newTVarIO Vector.empty
  gameVar <- newTVarIO Map.empty
  gameIdVar <- newTVarIO Game.ID.first
  run 9999 (logStdoutDev (app (ServerState roomVar gameVar gameIdVar)))

server :: ServerT HeartsAPI AppM
server =
  root
    :<|> serveDirectoryWebApp "static"
    :<|> join
    :<|> roomEndpoint
    :<|> create
    :<|> gameEndpoint
    :<|> playEndpoint

heartsAPI :: Proxy HeartsAPI
heartsAPI = Proxy

app :: ServerState -> Application
app s = serve heartsAPI (hoistServer heartsAPI (nt s) server)

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s

type AppM = ReaderT ServerState Handler

data ServerState = ServerState
  { roomEvents :: TVar (Vector Room.Event)
  , gameEvents :: TVar (Map Game.ID (Vector Game.Event))
  , nextGameID :: TVar Game.ID
  }

root :: AppM API.RootResponse
root =
  pure
    ( API.RootResponse
        { joinRoom =
            API.Action
              { name = "Join room"
              , description = "Join room"
              , url = "join"
              , method = API.Post
              , parameters = []
              , inputs = [API.TextInput "username" "Username:" True]
              }
        }
    )

join :: API.JoinRequest -> AppM (API.WithLocation API.RoomResponse)
join API.JoinRequest{username} = do
  roomVar <- asks roomEvents
  withRoom \room@Room{players} -> do
    let newId = Id (maximum (Vector.cons 0 (coerce (Player.id <$> players))) + 1)
    let joinEvent = Room.Join newId username
    events <- readTVar roomVar
    writeTVar roomVar (Vector.snoc events joinEvent)
    pure
      ( bimap
          makeError
          (makeResponse (Player.id <$> players) newId)
          (Room.processEvent room joinEvent)
      )
  where
    makeResponse ::
      Vector Player.Id ->
      Id ->
      Room ->
      API.WithLocation API.RoomResponse
    makeResponse players assignedId newRoom =
      addHeader
        ("room?playerId=" <> toQueryParam assignedId)
        ( let startGame =
                if Vector.length players >= 3
                  then Just (startGameAction assignedId)
                  else Nothing
              refresh = refreshAction assignedId
           in RoomResponse{room = newRoom, assignedId, startGame, refresh}
        )
    makeError :: Aeson.ToJSON e => e -> ServerError
    makeError e =
      err400
        { errBody =
            "You can't join this room, because: "
              <> Aeson.encode e
        }

roomEndpoint :: Maybe Player.Id -> AppM API.RoomResponse
roomEndpoint Nothing = throwError err400
roomEndpoint (Just playerId) = withRoom \room@Room{players} ->
  pure
    ( Right
        ( let startGame =
                if Vector.length players >= 4
                  then Just (startGameAction playerId)
                  else Nothing
              refresh = refreshAction playerId
           in RoomResponse{room, assignedId = playerId, startGame, refresh}
        )
    )

startGameAction :: Player.Id -> API.Action
startGameAction playerId =
  API.Action
    { name = "Start game"
    , description = "Start a new game"
    , url = "game"
    , method = API.Post
    , parameters = [("playerId", toQueryParam playerId)]
    , inputs = []
    }

refreshAction :: Player.Id -> API.Action
refreshAction playerId =
  API.Action
    { name = "Check for updates"
    , description = "Check for updates"
    , url = "room"
    , method = API.Get
    , parameters = [("playerId", toQueryParam playerId)]
    , inputs = []
    }

create :: Maybe Player.Id -> AppM (API.WithLocation API.CreateResult)
create (Just playerId) = do
  roomVar <- asks roomEvents
  gameVar <- asks gameEvents
  gameIdVar <- asks nextGameID
  deck <- liftIO Event.shuffledDeck
  withRoom \Room{players} ->
    case Player.takeFour players of
      Nothing ->
        pure $
          Left $
            err500
              { errBody = "There aren’t enough players to start a game."
              }
      Just fourPlayers -> case Player.findIndex (== playerId) (Player.id <$> fourPlayers) of
        Nothing ->
          pure $
            Left $
              err500
                { errBody = "Player ID not found."
                }
        Just playerIndex -> do
          roomEvents' <- readTVar roomVar
          gameMap <- readTVar gameVar
          gameID <- readTVar gameIdVar
          -- append the new room event
          writeTVar roomVar (Vector.snoc roomEvents' (Room.StartGame gameID))
          -- append the new game events
          let gameStartEvent = Event.StartEvent (Player.id <$> fourPlayers)
          let gameStartEvent' = Event.Start gameStartEvent
          let gameDealEvent = Event.DealEvent deck
          let gameDealEvent' = Event.Deal gameDealEvent
          let es =
                Vector.fromList
                  [ gameStartEvent'
                  , gameDealEvent'
                  ]
          writeTVar gameVar (Map.insert gameID es gameMap)
          writeTVar gameIdVar (Game.ID.next gameID)
          -- return the create result
          let startEvent = PlayerEvent.fromGameStartEvent gameStartEvent
          let dealEvent = PlayerEvent.fromGameDealEvent playerIndex gameDealEvent
          pure $
            Right $
              addHeader
                ( "game/" <> toQueryParam gameID
                    <> "?playerId="
                    <> toQueryParam playerId
                )
                API.CreateResult{..}
create Nothing = throwError err400{errBody = "You must provide a player ID"}

gameEndpoint :: Maybe Player.Id -> Game.ID -> AppM API.GameResult
gameEndpoint (Just playerId) gameID = do
  gameVar <- asks gameEvents
  withRoom \Room{players} ->
    case Player.takeFour players of
      Nothing ->
        pure $
          Left
            err500
              { errBody = "Player ID not found."
              }
      Just fourPlayers ->
        case Player.findIndex (== playerId) (fmap Player.id fourPlayers) of
          Nothing ->
            pure $
              Left
                err500
                  { errBody = "Player ID not found."
                  }
          Just playerIndex -> do
            gameMap <- readTVar gameVar
            case Map.lookup gameID gameMap >>= Vector.uncons of
              Nothing -> pure (Left err404)
              Just gameEvents ->
                case Game.foldEvents Nothing gameEvents of
                  Left e ->
                    pure
                      ( Left
                          err500
                            { errBody =
                                "This game is in an inconsistent state: "
                                  <> Aeson.encode e
                            }
                      )
                  Right game -> do
                    let playAction :: API.Action
                        playAction =
                          ( API.Action
                              { name = "Play card"
                              , description = "Play card"
                              , url = "/game/" <> Game.ID.toNumeral gameID <> "/play"
                              , method = API.Post
                              , parameters = [("playerId", toQueryParam playerId)]
                              , inputs = []
                              }
                          )
                    pure $
                      Right
                        ( API.GameResult
                            { gameID
                            , usernames = Player.username <$> fourPlayers
                            , game = Game.toPlayerGame playerIndex game
                            , playingNext = Game.playingNext game
                            , you = playerIndex
                            , playCard = playAction
                            }
                        )
gameEndpoint Nothing _ = throwError err400{errBody = "You must provide a player ID"}

withRoom :: (Room -> STM (Either ServerError a)) -> AppM a
withRoom useRoom = do
  roomVar <- asks roomEvents
  result <- liftIO $ atomically do
    roomEvents' <- readTVar roomVar
    Monad.join
      <$> traverse
        useRoom
        (first roomError (Room.foldEvents Nothing roomEvents'))
  either throwError pure result
  where
    roomError :: Aeson.ToJSON e => e -> ServerError
    roomError e =
      err500
        { errBody =
            "The room has an inconsistent state: "
              <> Aeson.encode e
        }

playEndpoint ::
  Maybe Id ->
  Game.ID ->
  API.CardSelection ->
  AppM (API.WithLocation API.PlayResult)
playEndpoint (Just player) gameID (API.CardSelection card) = do
  gameVar <- asks gameEvents
  withGame gameID \game@Game{} ->
    if Game.playingNext' game /= Just player
      then
        pure
          ( Left
              ( err400
                  { errBody =
                      "It's not your turn! Player "
                        <> Aeson.encode (Game.playingNext' game)
                        <> " is playing next."
                  }
              )
          )
      else do
        let e = Event.Play (Event.PlayEvent card)
        gameMap <- readTVar gameVar
        case Map.lookup gameID gameMap >>= Vector.uncons of
          Nothing -> pure (Left (err400{errBody = "Game " <> Aeson.encode gameID <> " not found!"}))
          Just (e', es) -> case Game.foldEvents Nothing (e', Vector.snoc es e) of
            Left err -> pure (Left (err400{errBody = "Couldn't apply event: " <> Aeson.encode err}))
            Right _ -> do
              writeTVar gameVar (Map.adjust (`Vector.snoc` e) gameID gameMap)
              pure
                ( Right
                    ( addHeader
                        ( "/game/" <> toQueryParam gameID
                            <> "?playerId="
                            <> toQueryParam player
                        )
                        (API.PlayResult ())
                    )
                )
playEndpoint Nothing _ _ = throwError err400{errBody = "You must provide a player ID"}

withGame :: Game.ID -> (Game -> STM (Either ServerError a)) -> AppM a
withGame gameID useGame = do
  gameVar <- asks gameEvents
  result <- liftIO $ atomically do
    gameMap <- readTVar gameVar
    Monad.join
      <$> traverse
        useGame
        ( maybe
            (Left (err400{errBody = "Game not found"}))
            Right
            (Map.lookup gameID gameMap >>= Vector.uncons)
            >>= (first gameError . Game.foldEvents Nothing)
        )
  either throwError pure result
  where
    gameError :: Aeson.ToJSON e => e -> ServerError
    gameError e =
      err500
        { errBody =
            "The game has an inconsistent state: "
              <> Aeson.encode e
        }
