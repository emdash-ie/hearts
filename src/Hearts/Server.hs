{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Hearts.Server (runServer) where

import qualified Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Conc (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import Data.Bifunctor (bimap, first)
import Hearts.API (HeartsAPI, JoinResult (..))
import qualified Hearts.API as API
import qualified Hearts.Game as Game
import qualified Hearts.Game.Event as Event
import qualified Hearts.Player as Player
import qualified Hearts.Player.Event as PlayerEvent
import Hearts.Player.Id (Id (..))
import Hearts.Room (Room (Room))
import qualified Hearts.Room as Room

runServer :: IO ()
runServer = do
  roomVar <- newTVarIO Vector.empty
  gameVar <- newTVarIO Map.empty
  run 9999 (logStdoutDev (app (ServerState roomVar gameVar)))

server :: ServerT HeartsAPI AppM
server =
  root
    :<|> join
    :<|> roomEndpoint
    :<|> create
    :<|> gameEndpoint

heartsAPI :: Proxy HeartsAPI
heartsAPI = Proxy

app :: ServerState -> Application
app s = serve heartsAPI (hoistServer heartsAPI (nt s) server)

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s

type AppM = ReaderT ServerState Handler

data ServerState = ServerState
  { roomEvents :: TVar (Vector Room.Event)
  , gameEvents :: TVar (Map UUID (Vector Game.Event))
  }

root :: AppM (API.APIResponse API.RootResult)
root =
  pure
    ( API.APIResponse
        { result = API.RootResult ()
        , actions =
            Vector.singleton
              ( API.Action
                  { name = "Join room"
                  , description = "Join room"
                  , url = "join"
                  , method = API.Post
                  }
              )
        }
    )

join :: AppM (API.WithLocation API.JoinResponse)
join = do
  roomVar <- asks roomEvents
  withRoom \room@Room{..} -> do
    let newId = Id (maximum (Vector.cons 0 (coerce players)) + 1)
    let joinEvent = Room.Join newId
    events <- readTVar roomVar
    writeTVar roomVar (Vector.snoc events joinEvent)
    pure (bimap makeError (makeResponse players newId) (Room.processEvent room joinEvent))
  where
    makeResponse ::
      Vector Player.Id ->
      Id ->
      Room ->
      API.WithLocation API.JoinResponse
    makeResponse players assignedId newRoom =
      addHeader
        ("room?playerId=" <> toQueryParam assignedId)
        ( API.APIResponse
            { actions =
                if Vector.length players >= 3
                  then
                    Vector.singleton
                      ( API.Action
                          { name = "Start game"
                          , description = "Start a new game"
                          , url = "game?id=" <> toQueryParam assignedId
                          , method = API.Post
                          }
                      )
                  else Vector.empty
            , result = JoinResult{room = newRoom, assignedId}
            }
        )
    makeError :: Aeson.ToJSON e => e -> ServerError
    makeError e =
      err400
        { errBody =
            "You can't join this room, because: "
              <> Aeson.encode e
        }

roomEndpoint :: Maybe Player.Id -> AppM API.JoinResponse
roomEndpoint Nothing = throwError err400
roomEndpoint (Just playerId) = withRoom \room@Room{players} ->
  pure
    ( Right
        ( API.APIResponse
            { actions =
                let start =
                      if Vector.length players >= 4
                        then
                          Vector.singleton
                            ( API.Action
                                { name = "Start game"
                                , description = "Start a new game"
                                , url = "game?playerId=" <> toQueryParam playerId
                                , method = API.Post
                                }
                            )
                        else Vector.empty
                    refresh =
                      API.Action
                        { name = "Check for updates"
                        , description = "Check for updates"
                        , url = "room?playerId=" <> toQueryParam playerId
                        , method = API.Get
                        }
                 in Vector.cons refresh start
            , result = JoinResult{room, assignedId = playerId}
            }
        )
    )

create :: Maybe Player.Id -> AppM (API.WithLocation API.CreateResponse)
create (Just playerId) = do
  roomVar <- asks roomEvents
  gameVar <- asks gameEvents
  gameId <- liftIO UUID.nextRandom
  deck <- liftIO Event.shuffledDeck
  withRoom \Room{players} ->
    case Player.takeFour players of
      Nothing ->
        pure $
          Left $
            err500
              { errBody = "There aren’t enough players to start a game."
              }
      Just fourPlayers -> case Player.findIndex (== playerId) fourPlayers of
        Nothing ->
          pure $
            Left $
              err500
                { errBody = "Player ID not found."
                }
        Just playerIndex -> do
          roomEvents' <- readTVar roomVar
          gameMap <- readTVar gameVar
          -- append the new room event
          writeTVar roomVar (Vector.snoc roomEvents' (Room.StartGame gameId))
          -- append the new game events
          let gameStartEvent = Event.StartEvent fourPlayers
          let gameStartEvent' = Event.Start gameStartEvent
          let gameDealEvent = Event.DealEvent deck
          let gameDealEvent' = Event.Deal gameDealEvent
          let es =
                Vector.fromList
                  [ gameStartEvent'
                  , gameDealEvent'
                  ]
          writeTVar gameVar (Map.insert gameId es gameMap)
          -- return the create result
          let startEvent = PlayerEvent.fromGameStartEvent gameStartEvent
          let dealEvent = PlayerEvent.fromGameDealEvent playerIndex gameDealEvent
          pure $
            Right $
              addHeader
                ("game/" <> toQueryParam gameId)
                ( API.APIResponse
                    { actions = Vector.empty
                    , result = API.CreateResult{..}
                    }
                )
create Nothing = throwError err400{errBody = "You must provide a player ID"}

gameEndpoint :: Maybe Player.Id -> UUID -> AppM (API.APIResponse API.GameResult)
gameEndpoint (Just playerId) gameId = do
  gameVar <- asks gameEvents
  withRoom \Room{players} ->
    case Player.takeFour players >>= Player.findIndex (== playerId) of
      Nothing ->
        pure $
          Left
            err500
              { errBody = "Player ID not found."
              }
      Just playerIndex -> do
        gameMap <- readTVar gameVar
        case Map.lookup gameId gameMap >>= Vector.uncons of
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
              Right game ->
                pure $
                  Right
                    ( API.APIResponse
                        { actions = Vector.empty
                        , result =
                            API.GameResult
                              { gameId
                              , game = Game.toPlayerGame playerIndex game
                              }
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
