{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Hearts.Game (
  Event,
  processEvent,
  Game (..),
  ID,
  dealAmong4,
  sortHand,
  foldEvents,
  toPlayerGame,
  playingNext,
  playingNext',
  winner,
) where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Lens (
  ASetter,
  ASetter',
  over,
  set,
  to,
  view,
  (%~),
  (.~),
  (?~),
  (^.),
  _2,
  _Just,
 )
import Control.Monad (guard, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Foldable (foldl', toList)
import Data.Function (on, (&))
import Data.Functor (($>))
import Data.Functor.Identity
import Data.Generics.Product (field)
import Data.List (find, maximumBy)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (First (First), Sum (..), getFirst)
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

import Hearts.Card
import qualified Hearts.Card as Card
import Hearts.Game.Event
import Hearts.Game.ID (ID)
import Hearts.Player hiding (Game (..), RoundInProgress (..))
import qualified Hearts.Player as Player
import Prelude hiding (round)

data Game = Game
  { players :: FourPlayers Player.Id
  , scores :: FourPlayers (Sum Integer)
  , currentRound :: Maybe RoundInProgress
  , pastRounds :: Vector Round
  , finished :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Game where
  toJSON Game{..} =
    Aeson.object
      [ "players" .= players
      , "scores" .= fmap getSum scores
      , "currentRound" .= currentRound
      , "pastRounds" .= pastRounds
      , "finished" .= finished
      ]

data RoundInProgress = RoundInProgress
  { hands :: Maybe (FourPlayers (Vector Card))
  , trick :: Maybe (Trick Maybe)
  , tricks :: Maybe (Vector (Trick Identity))
  , heartsBroken :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON RoundInProgress

finishGame :: Game -> Maybe Game
finishGame g = do
  _ <- Player.findIndex (>= 100) (g ^. field @"scores")
  return (set (field @"finished") True g)

finishRound :: RoundInProgress -> Maybe Round
finishRound RoundInProgress{..} = do
  guard (handIsFinished hands)
  guard (null trick)
  ts <- tricks
  return Round{tricks = ts, scores = scoreHand ts}
  where
    scoreHand :: Vector (Trick Identity) -> FourPlayers (Sum Integer)
    scoreHand ts =
      let initialScores = foldMap scoreTrick ts
       in case Player.findIndex (== 26) initialScores of
            Nothing -> initialScores
            Just i -> set (playerData i) 0 (pure 26)
    handIsFinished :: Maybe (FourPlayers (Vector a)) -> Bool
    handIsFinished = maybe False \FourPlayers{..} ->
      all ((== 0) . Vector.length) [one, two, three, four]

startNewTrick :: Trick Identity -> RoundInProgress -> RoundInProgress
startNewTrick lastTrick = field @"trick" .~ Just (winner lastTrick, pure Nothing)

finishTrick :: RoundInProgress -> Maybe (Trick Identity, RoundInProgress)
finishTrick r = do
  trick <- r ^. field @"trick" . to (>>= finishedTrick)
  return
    ( trick
    , r
        & (field @"tricks" %~ Just . maybe (Vector.singleton trick) (`Vector.snoc` trick))
          . (field @"trick" .~ Nothing)
    )
  where
    finishedTrick :: Trick Maybe -> Maybe (Trick Identity)
    finishedTrick (i, FourPlayers{..}) =
      case (one, two, three, four) of
        (Just one', Just two', Just three', Just four') ->
          Just (i, Identity <$> FourPlayers{one = one', two = two', three = three', four = four'})
        _ -> Nothing

playCard :: Card -> PlayerIndex -> RoundInProgress -> RoundInProgress
playCard card playerIndex r = fromMaybe r do
  let addCardToTrick =
        field @"trick" . _Just . _2 . playerData playerIndex
          ?~ card
  let removeCardFromHand =
        field @"hands" . _Just . playerData playerIndex
          %~ Vector.filter (/= card)
  pure ((removeCardFromHand >>> addCardToTrick >>> maybeBreakHearts) r)
  where
    maybeBreakHearts :: RoundInProgress -> RoundInProgress
    maybeBreakHearts g =
      set (field @"heartsBroken") (heartsBroken g || suit card == Hearts) g

toPlayerGame :: PlayerIndex -> Game -> Player.Game
toPlayerGame index Game{..} =
  Player.Game
    { currentRound = fmap (toPlayerRoundInProgress index) currentRound
    , ..
    }

toPlayerRoundInProgress :: PlayerIndex -> RoundInProgress -> Player.RoundInProgress
toPlayerRoundInProgress index RoundInProgress{..} =
  Player.RoundInProgress
    { hand = Player.getPlayerData index <$> hands
    , lastTrick = tricks >>= (fmap snd . Vector.unsnoc)
    , ..
    }

data FoldError
  = -- | This event (which isn't a 'Start') doesn't make sense, because the game
    -- has not yet started.
    GameNotStarted Event
  | -- | This 'Game' has already started, so this 'StartEvent' can't be applied.
    GameAlreadyStarted Game StartEvent
  | -- | This 'Game' is finished, so this event can't be applied.
    GameFinished Game Event
  | -- | This play can't be made with this game state.
    PlayInvalid PlayError Game PlayEvent
  deriving (Show, Generic)

instance Aeson.ToJSON FoldError

data PlayError
  = -- | This play is invalid because it does not follow suit.
    NotFollowingSuit
  | -- | This play is invalid because it breaks hearts incorrectly.
    BreakingHeartsIncorrectly
  | -- | This play is invalid because this is the first trick in a hand and
    -- it contains a penalty card.
    PlayedPenaltyCardIllegally
  deriving (Show, Generic)

instance Aeson.ToJSON PlayError

foldEvents ::
  Foldable f =>
  Maybe Game ->
  (Event, f Event) ->
  Either FoldError Game
foldEvents game (event, events) =
  foldl' fold (processEvent game event) events
  where
    fold ::
      Either FoldError Game ->
      Event ->
      Either FoldError Game
    fold = either (const . Left) (processEvent . Just)

processEvent :: Maybe Game -> Event -> Either FoldError Game
processEvent Nothing (Start StartEvent{..}) =
  Right $
    Game
      { players = players
      , scores = pure 0
      , currentRound =
          Just
            RoundInProgress
              { hands = Nothing
              , trick = Nothing
              , tricks = Nothing
              , heartsBroken = False
              }
      , pastRounds = Vector.empty
      , finished = False
      }
processEvent Nothing event = Left (GameNotStarted event)
processEvent (Just game) (Start event) = Left (GameAlreadyStarted game event)
processEvent (Just game) event | finished game = Left (GameFinished game event)
processEvent (Just game) (Deal (DealEvent (Deck deck))) =
  let hands = sortHand <$> dealAmong4 deck
      updateHands = field @"hands" ?~ hands
      initialiseTrick :: RoundInProgress -> RoundInProgress
      initialiseTrick =
        field @"trick" .~ fmap (,pure Nothing) (playingFirst hands)
   in Right ((field @"currentRound" . _Just %~ (initialiseTrick . updateHands)) game)
processEvent (Just game) (Play playEvent@PlayEvent{card}) = do
  oldRound <- maybe (Left undefined) Right (game ^. field @"currentRound")
  next <- maybe (Left undefined) Right (playingNext oldRound)
  let notFollowingSuitWhenRequired :: Bool
      notFollowingSuitWhenRequired = fromMaybe False do
        hands <- oldRound ^. field @"hands"
        pure (not (checkFollowingSuit oldRound hands next card))
      breakingHeartsIncorrectly :: Bool
      breakingHeartsIncorrectly =
        leading && suit card == Hearts
          && not (heartsBroken oldRound)
          && hasNonHearts
      playedPenaltyCardIllegally :: Bool
      playedPenaltyCardIllegally =
        null (game ^. field @"currentRound" . _Just . field @"tricks") && isPenaltyCard
      isPenaltyCard :: Bool
      isPenaltyCard =
        (suit card == Hearts)
          || (suit card == Spades && value card == Queen)
      leading :: Bool
      leading =
        all
          isNothing
          (maybe [Nothing] (toList . snd) (oldRound ^. field @"trick"))
      hasNonHearts :: Bool
      hasNonHearts = fromMaybe False do
        playerHand <- view (playerData next) <$> (oldRound ^. field @"hands")
        pure (any ((/= Hearts) . suit) playerHand)
  when
    notFollowingSuitWhenRequired
    (Left (PlayInvalid NotFollowingSuit game playEvent))
  when
    breakingHeartsIncorrectly
    (Left (PlayInvalid BreakingHeartsIncorrectly game playEvent))
  when
    playedPenaltyCardIllegally
    (Left (PlayInvalid PlayedPenaltyCardIllegally game playEvent))
  let currentRound = playCard card next oldRound
  case finishTrick currentRound of
    Nothing -> return (game & field @"currentRound" ?~ currentRound)
    Just (lastTrick, currentRound') -> case finishRound currentRound' of
      Nothing ->
        return (game & field @"currentRound" ?~ startNewTrick lastTrick currentRound')
      Just finishedRound ->
        let game' =
              game
                & (field @"pastRounds" %~ (`Vector.snoc` finishedRound))
                  . (field @"scores" %~ ((finishedRound ^. field @"scores") <>))
         in case finishGame game' of
              Nothing ->
                return
                  ( game'
                      & ( field @"currentRound"
                            ?~ RoundInProgress Nothing Nothing Nothing False
                        )
                  )
              Just x -> return x

-- | Played next, would this card follow suit (or not) correctly?
checkFollowingSuit :: RoundInProgress -> FourPlayers (Vector Card) -> PlayerIndex -> Card -> Bool
checkFollowingSuit round hands nextPlayer card =
  isFollowingSuit || not shouldFollowSuit
  where
    firstCard :: Maybe Card
    firstCard = do
      (startingPlayer, trick) <- round ^. field @"trick"
      Player.getPlayerData startingPlayer trick
    playerCards :: Vector Card
    playerCards = Player.getPlayerData nextPlayer hands
    shouldFollowSuit :: Bool
    shouldFollowSuit = fromMaybe False do
      c <- firstCard
      pure (Vector.any (((==) `on` suit) c) playerCards)
    isFollowingSuit :: Bool
    isFollowingSuit = maybe False ((suit card ==) . suit) firstCard

scoreTrick :: Trick Identity -> FourPlayers (Sum Integer)
scoreTrick t =
  let w = winner t
      winnerScore = foldMap Card.score (toList (runIdentity <$> snd t))
   in set (playerData w) winnerScore (pure 0)

playingNext :: RoundInProgress -> Maybe PlayerIndex
playingNext currentRound =
  checkCurrentTrick <|> checkLastTrick
  where
    checkCurrentTrick :: Maybe PlayerIndex
    checkCurrentTrick = do
      (firstPlayer, t) <- trick currentRound
      let cards :: [(PlayerIndex, Maybe Card)]
          cards = Player.playOrder firstPlayer ((,) <$> indices <*> t)
      fmap fst (find (isNothing . snd) cards)
    checkLastTrick = do
      ts <- currentRound ^. field @"tricks"
      t <- guard (not (Vector.null ts)) $> Vector.last ts
      pure (winner t)
    indices = FourPlayers PlayerOne PlayerTwo PlayerThree PlayerFour

playingNext' :: Game -> Maybe Player.Id
playingNext' g = do
  i <- g ^. field @"currentRound" >>= playingNext
  pure (g ^. field @"players" . playerData i)

playingFirst :: FourPlayers (Vector Card) -> Maybe PlayerIndex
playingFirst hands = do
  let indices = [minBound .. maxBound]
  let indexed = zip indices (toList hands)
  let f :: (PlayerIndex, Vector Card) -> First PlayerIndex
      f (i, h) =
        First $
          if Card Clubs Two `elem` h
            then Just i
            else Nothing
  getFirst (foldMap f indexed)

winner :: Trick Identity -> PlayerIndex
winner (firstPlayer, trick) =
  let s = Card.suit (runIdentity (Player.getPlayerData firstPlayer trick))
      effectiveValue Card{suit, value} =
        if suit == s
          then Just value
          else Nothing
      indices = [minBound .. maxBound]
   in fst
        ( maximumBy
            (comparing (effectiveValue . snd))
            (zip indices (runIdentity <$> toList trick))
        )

dealAmong ::
  (Monoid (h [a]), Foldable t) =>
  [ASetter' (h [a]) [a]] ->
  t a ->
  h [a]
dealAmong setters xs = foldr ($) mempty zipped
  where
    zipped = zipWith prependTo (cycle setters) (toList xs)
    prependTo ::
      ASetter s t1 [a1] [a1] ->
      a1 ->
      s ->
      t1
    prependTo s x = over s (x :)

_dealAmong3 :: Vector a -> ThreePlayers (Vector a)
_dealAmong3 =
  fmap Vector.fromList
    <$> dealAmong [field @"one", field @"two", field @"three"]

dealAmong4 :: Vector a -> FourPlayers (Vector a)
dealAmong4 =
  fmap Vector.fromList
    <$> dealAmong [field @"one", field @"two", field @"three", field @"four"]

sortHand :: Vector Card -> Vector Card
sortHand = Vector.fromList . Card.sortCards . toList
