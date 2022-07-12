{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Hearts.Server (runServer) where

import qualified Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (Sum))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Conc (STM, TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import Control.Category ((>>>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Hearts.API (HeartsAPI, RoomResponse (..))
import qualified Hearts.API as API
import Hearts.Game (Game (Game))
import qualified Hearts.Game as Game
import qualified Hearts.Game.Event as Event
import qualified Hearts.Game.ID as Game.ID
import qualified Hearts.Player as Player
import qualified Hearts.Player.Event as PlayerEvent
import Hearts.Player.Id (Id)
import qualified Hearts.Player.Id as Player.Id
import Hearts.Room (Room (Room))
import qualified Hearts.Room as Room

runServer :: Int -> IO ()
runServer port = do
  roomVar <- newTVarIO Map.empty
  gameVar <- newTVarIO Map.empty
  gameIdVar <- newTVarIO Game.ID.first
  playerIdVar <- newTVarIO Player.Id.first
  run port (logStdoutDev (app (ServerState roomVar gameVar gameIdVar playerIdVar)))

server :: ServerT HeartsAPI AppM
server =
  root
    :<|> serveDirectoryWebApp "static"
    :<|> createRoom
    :<|> ( \roomName ->
            roomEndpoint roomName
              :<|> join roomName
              :<|> create roomName
              :<|> gameEndpoint roomName
              :<|> playEndpoint
              :<|> eventsEndpoint
              :<|> eventsHeadEndpoint
         )

heartsAPI :: Proxy HeartsAPI
heartsAPI = Proxy

app :: ServerState -> Application
app s = serve heartsAPI (hoistServer heartsAPI (nt s) server)

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s

type AppM = ReaderT ServerState Handler

data ServerState = ServerState
  { roomEvents :: TVar (Map Text (Vector Room.Event))
  , gameEvents :: TVar (Map Game.ID (Vector Game.Event))
  , nextGameID :: TVar Game.ID
  , nextPlayerID :: TVar Player.Id
  }

root :: AppM API.RootResponse
root = do
  roomMap <- asks (roomEvents >>> readTVarIO) >>= liftIO
  let rooms = (Vector.fromList . catMaybes) do
        (roomName, events) <- Map.toList roomMap
        pure do
          t <- Vector.uncons events
          pure (roomName, Room.foldEvents Nothing t)
  pure
    ( API.RootResponse
        { createRoom =
            API.Action
              { name = "Create room"
              , description = "Create a room"
              , url = "room"
              , method = API.Post
              , parameters = []
              , inputs =
                  [ API.TextInput "username" "Username:" True
                  , API.TextInput "roomName" "Room name:" True
                  ]
              }
        , joinRoom =
            (Map.fromList . catMaybes)
              [ either (const Nothing) (Just . (name,) . roomAction) roomOrError
              | (name, roomOrError) <- toList rooms
              ]
        , rooms
        }
    )
  where
    roomAction :: Room -> API.Action
    roomAction Room{name} =
      API.Action
        { name = "Join room"
        , description = "Join this room"
        , url = "room/" <> name <> "/join"
        , method = API.Post
        , parameters = []
        , inputs =
            [ API.TextInput "username" "Username:" True
            , API.HiddenInput "roomName" name True
            ]
        }

createRoom :: API.JoinRequest -> AppM (API.WithLocation API.RoomResponse)
createRoom joinRequest@API.JoinRequest{roomName} = do
  roomVar <- asks roomEvents
  result <- (liftIO . atomically) do
    roomMap <- readTVar roomVar
    case Map.lookup roomName roomMap of
      Nothing -> do
        let createEvent = Vector.singleton (Room.Create roomName)
        writeTVar roomVar (Map.insert roomName createEvent roomMap)
        pure (Right ())
      Just _ -> pure (Left roomExistsError)
  case result of
    Left e -> throwError e
    Right () -> do
      (assignedId, roomResponse) <- join' roomName joinRequest
      pure $
        addHeader
          ("room/" <> roomName <> "/?playerId=" <> toQueryParam assignedId)
          roomResponse
  where
    roomExistsError = err400{errBody = "Error: Room " <> encodeUtf8 (fromStrict roomName) <> " already exists!"}

join :: Text -> API.JoinRequest -> AppM (API.WithLocation API.RoomResponse)
join roomName joinRequest = do
  (assignedId, roomResponse) <- join' roomName joinRequest
  pure $
    addHeader
      ("./?playerId=" <> toQueryParam assignedId)
      roomResponse

join' :: Text -> API.JoinRequest -> AppM (Player.Id, API.RoomResponse)
join' roomName API.JoinRequest{username} = do
  roomVar <- asks roomEvents
  playerIdVar <- asks nextPlayerID
  withRoom roomName \room@Room{players} -> do
    newId <- readTVar playerIdVar
    let joinEvent = Room.Join newId username
    roomMap <- readTVar roomVar
    let newMap = Map.alter (pure . maybe (Vector.singleton joinEvent) (`Vector.snoc` joinEvent)) roomName roomMap
    case Room.processEvent (Just room) joinEvent of
      Right r -> do
        writeTVar roomVar newMap
        writeTVar playerIdVar (Player.Id.next newId)
        pure (Right (makeResponse (Player.id <$> players) newId r))
      Left e ->
        pure (Left (makeError e))
  where
    makeResponse ::
      Vector Player.Id ->
      Player.Id ->
      Room ->
      (Player.Id, API.RoomResponse)
    makeResponse players assignedId newRoom =
      let startGame =
            if Vector.length players >= 3
              then Just (startGameAction assignedId)
              else Nothing
          refresh = refreshAction assignedId
       in (assignedId, RoomResponse{room = newRoom, assignedId, startGame, refresh})
    makeError :: Aeson.ToJSON e => e -> ServerError
    makeError e =
      err400
        { errBody =
            "You can't join this room, because: "
              <> Aeson.encode e
        }

roomEndpoint :: Text -> Maybe Player.Id -> AppM API.RoomResponse
roomEndpoint _ Nothing = throwError err400
roomEndpoint roomName (Just playerId) = withRoom roomName \room@Room{players} ->
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
    , url = ""
    , method = API.Get
    , parameters = [("playerId", toQueryParam playerId)]
    , inputs = []
    }

create :: Text -> Maybe Player.Id -> AppM (API.WithLocation API.CreateResult)
create roomName (Just playerId) = do
  roomVar <- asks roomEvents
  gameVar <- asks gameEvents
  gameIdVar <- asks nextGameID
  deck <- liftIO Event.shuffledDeck
  withRoom roomName \Room{players} ->
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
          roomMap <- readTVar roomVar
          gameMap <- readTVar gameVar
          gameID <- readTVar gameIdVar
          -- append the new room event
          let roomStartEvent = Room.StartGame gameID
          let newMap = Map.alter (pure . maybe (Vector.singleton roomStartEvent) (`Vector.snoc` roomStartEvent)) roomName roomMap
          writeTVar roomVar newMap
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
                    <> "/?playerId="
                    <> toQueryParam playerId
                )
                API.CreateResult{..}
create _ Nothing = throwError err400{errBody = "You must provide a player ID"}

gameEndpoint :: Text -> Maybe Player.Id -> Game.ID -> AppM API.GameResult
gameEndpoint roomName (Just playerId) gameID = do
  gameVar <- asks gameEvents
  withRoom roomName \Room{players} ->
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
                              , url = "play"
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
gameEndpoint _ Nothing _ = throwError err400{errBody = "You must provide a player ID"}

withRoom :: Text -> (Room -> STM (Either ServerError a)) -> AppM a
withRoom roomName useRoom = do
  roomVar <- asks roomEvents
  result <- liftIO $ atomically do
    roomMap <- readTVar roomVar
    Monad.join
      <$> traverse
        useRoom
        ( maybe
            (Left (err400{errBody = "Room not found"}))
            Right
            (Map.lookup roomName roomMap >>= Vector.uncons)
            >>= (first roomError . Room.foldEvents Nothing)
        )
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
  deck <- liftIO Event.shuffledDeck
  withGame gameID \game@Game{hands} ->
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
        let newEvents =
              Vector.cons
                (Event.Play (Event.PlayEvent card))
                ( if foldMap (Sum . Vector.length) (maybe [] toList hands) == Sum 1
                    then Vector.singleton (Event.Deal (Event.DealEvent deck))
                    else Vector.empty
                )
        gameMap <- readTVar gameVar
        case Map.lookup gameID gameMap >>= Vector.uncons of
          Nothing -> pure (Left (err400{errBody = "Game " <> Aeson.encode gameID <> " not found!"}))
          Just (e', es) -> case Game.foldEvents Nothing (e', es <> newEvents) of
            Left err -> pure (Left (err400{errBody = "Couldn't apply event: " <> Aeson.encode err}))
            Right _ -> do
              writeTVar gameVar (Map.adjust (<> newEvents) gameID gameMap)
              pure
                ( Right
                    ( addHeader
                        ( "./?playerId="
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

eventsEndpoint :: Game.ID -> AppM (Vector Game.Event)
eventsEndpoint gameID = do
  gameVar <- asks gameEvents
  result <- liftIO $ atomically do
    gameMap <- readTVar gameVar
    pure
      ( maybe
          (Left err500)
          Right
          (Map.lookup gameID gameMap)
      )
  either throwError pure result

eventsHeadEndpoint :: Game.ID -> AppM (Game.Event)
eventsHeadEndpoint gameID = do
  gameVar <- asks gameEvents
  result <- liftIO $ atomically do
    gameMap <- readTVar gameVar
    pure
      ( maybe
          (Left err500)
          Right
          ( do
              v <- Map.lookup gameID gameMap
              (e, _) <- Vector.uncons v
              pure e
          )
      )
  either throwError pure result
