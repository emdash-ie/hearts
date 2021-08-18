{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hearts.Server (runServer) where

import Hearts.API (HeartsAPI, JoinResult (..))
import qualified Hearts.API as API
import qualified Hearts.Game as Game
import Hearts.Player.Id (Id (..))
import Hearts.Room (Room (Room))
import qualified Hearts.Room as Room

import qualified Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

runServer :: IO ()
runServer = do
  roomVar <- newTVarIO Vector.empty
  gameVar <- newTVarIO Vector.empty
  run 9999 (logStdoutDev (app (ServerState roomVar gameVar)))

server :: ServerT HeartsAPI AppM
server = join

heartsAPI :: Proxy HeartsAPI
heartsAPI = Proxy

app :: ServerState -> Application
app s = serve heartsAPI (hoistServer heartsAPI (nt s) server)

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s

type AppM = ReaderT ServerState Handler

data ServerState = ServerState
  { roomEvents :: TVar (Vector Room.Event)
  , _gameEvents :: TVar (Vector Game.Event)
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
            pure (toResponse newId (Room.processEvent room joinEvent))
  where
    toResponse :: Id -> Either Room.FoldError Room -> AppM API.JoinResponse
    toResponse assignedId = \case
      Left e ->
        throwError
          err400
            { errBody =
                "You can't join this room, because: "
                  <> Aeson.encode e
            }
      Right newRoom ->
        pure
          ( API.JoinResponse
              ( API.APIResponse
                  { actions = Vector.empty
                  , result = API.Join (JoinResult{room = newRoom, assignedId})
                  }
              )
          )
