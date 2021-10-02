{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

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
  join
    :<|> create

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

join :: AppM API.JoinResponse
join = do
  roomVar <- asks roomEvents
  Monad.join $
    liftIO $
      atomically $ do
        events <- readTVar roomVar
        case Room.foldEvents Nothing events of
          Left e ->
            pure $
              throwError
                err500
                  { errBody =
                      "The room has an inconsistent state: "
                        <> Aeson.encode e
                  }
          Right room@Room{..} -> do
            let newId = Id (maximum (Vector.cons 0 (coerce players)) + 1)
            let joinEvent = Room.Join newId
            writeTVar roomVar (Vector.snoc events joinEvent)
            pure (toResponse players newId (Room.processEvent room joinEvent))
  where
    toResponse :: Vector Player.Id -> Id -> Either Room.FoldError Room -> AppM API.JoinResponse
    toResponse players assignedId = \case
      Left e ->
        throwError
          err400
            { errBody =
                "You can't join this room, because: "
                  <> Aeson.encode e
            }
      Right newRoom ->
        pure
          ( API.APIResponse
              { actions =
                  if Vector.length players >= 3
                    then
                      Vector.singleton
                        ( API.Action
                            { name = "Start game"
                            , description = "Start a new game"
                            , url = "game?id=" <> toQueryParam assignedId
                            }
                        )
                    else Vector.empty
              , result = JoinResult{room = newRoom, assignedId}
              }
          )

create :: Maybe Player.Id -> AppM API.CreateResponse
create (Just playerId) = do
  roomVar <- asks roomEvents
  gameVar <- asks gameEvents
  gameId <- liftIO UUID.nextRandom
  deck <- liftIO Event.shuffledDeck
  Monad.join $
    liftIO $ atomically do
      roomEvents' <- readTVar roomVar
      gameMap <- readTVar gameVar
      case Room.foldEvents Nothing roomEvents' of
        Left e ->
          pure $
            throwError
              err500
                { errBody =
                    "The room has an inconsistent state: "
                      <> Aeson.encode e
                }
        Right Room{players} -> case Player.takeFour players of
          Nothing ->
            pure $
              throwError
                err500
                  { errBody = "There aren’t enough players to start a game."
                  }
          Just fourPlayers -> case Player.findIndex (== playerId) fourPlayers of
            Nothing ->
              pure $
                throwError
                  err500
                    { errBody = "Player ID not found."
                    }
            Just playerIndex -> do
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
                pure
                  ( API.APIResponse
                      { actions = Vector.empty
                      , result = API.CreateResult{..}
                      }
                  )
create Nothing = throwError err400{errBody = "You must provide a player ID"}
