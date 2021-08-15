{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hearts.Server (runServer) where

import Hearts.API (HeartsAPI, JoinResult (..))
import qualified Hearts.API as API
import qualified Hearts.Game as Game
import Hearts.Player.Id (Id (..))
import Hearts.Room (Room (Room))
import qualified Hearts.Room as Room

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
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
  , gameEvents :: TVar (Vector Game.Event)
  }

join :: AppM API.JoinResponse
join = do
  roomVar <- asks roomEvents
  liftIO $
    atomically $ do
      events <- readTVar roomVar
      case Room.foldEvents Nothing events of
        Left _ -> error "oh no"
        Right room@Room{..} -> do
          let assignedId = Id (maximum (Vector.cons 0 (coerce players)) + 1)
          let joinEvent = Room.Join assignedId
          writeTVar roomVar (Vector.snoc events joinEvent)
          case Room.processEvent room joinEvent of
            Left _ -> error "oh no again"
            Right newRoom ->
              pure
                ( API.JoinResponse
                    ( API.APIResponse
                        { actions = Vector.empty
                        , result = API.Join (JoinResult{room = newRoom, assignedId})
                        }
                    )
                )
