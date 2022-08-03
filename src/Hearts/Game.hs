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
import Hearts.Player (FourPlayers (..), PlayerIndex (..), ThreePlayers (..), Trick, playerData)
import qualified Hearts.Player as Player

data Game = Game
  { players :: FourPlayers Player.Id
  , scores :: FourPlayers (Sum Integer)
  , hands :: Maybe (FourPlayers (Vector Card))
  , trick :: Maybe (Trick Maybe)
  , tricks :: Maybe (Vector (Trick Identity))
  , heartsBroken :: Bool
  , finished :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Game where
  toJSON Game{..} =
    Aeson.object
      [ "players" .= players
      , "scores" .= fmap getSum scores
      , "hands" .= hands
      , "trick" .= trick
      , "tricks" .= tricks
      , "heartsBroken" .= heartsBroken
      , "finished" .= finished
      ]

toPlayerGame :: PlayerIndex -> Game -> Player.Game
toPlayerGame index Game{..} =
  Player.Game
    { hand = Player.getPlayerData index <$> hands
    , lastTrick = fmap Vector.last tricks
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
      , hands = Nothing
      , trick = Nothing
      , tricks = Nothing
      , heartsBroken = False
      , finished = False
      }
processEvent Nothing event = Left (GameNotStarted event)
processEvent (Just game) (Start event) = Left (GameAlreadyStarted game event)
processEvent (Just game) event | finished game = Left (GameFinished game event)
processEvent (Just game) (Deal (DealEvent (Deck deck))) =
  let hands = sortHand <$> dealAmong4 deck
      updateHands = field @"hands" .~ Just hands
      initialiseTrick :: Game -> Game
      initialiseTrick =
        field @"trick" .~ fmap (,pure Nothing) (playingFirst hands)
      unbreakHearts :: Game -> Game
      unbreakHearts = set (field @"heartsBroken") False
   in Right (unbreakHearts (initialiseTrick (updateHands game)))
processEvent (Just game) (Play playEvent@PlayEvent{card}) =
  let trickIsFinished :: Trick Maybe -> Maybe (Trick Identity)
      trickIsFinished (i, FourPlayers{..}) =
        case (one, two, three, four) of
          (Just one', Just two', Just three', Just four') ->
            Just (i, Identity <$> FourPlayers{one = one', two = two', three = three', four = four'})
          _ -> Nothing
      maybeDiscardTrick :: Game -> Game
      maybeDiscardTrick g = fromMaybe g do
        trick <-
          g
            ^. field @"trick"
              . to (>>= trickIsFinished)
        pure
          ( g & field @"tricks" %~ Just . maybe (Vector.singleton trick) (`Vector.snoc` trick)
              & field @"trick"
                .~ ( if g ^. field @"hands" . to handIsFinished
                      then Nothing
                      else Just (winner trick, pure Nothing)
                   )
          )
      handIsFinished :: Maybe (FourPlayers (Vector a)) -> Bool
      handIsFinished = maybe False \FourPlayers{..} ->
        all ((== 0) . Vector.length) [one, two, three, four]
      maybeScoreHand :: Game -> Game
      maybeScoreHand g =
        if g ^. field @"hands" . to handIsFinished
          then fromMaybe g do
            initialScores <-
              g
                ^. field @"tricks"
                  . to (fmap (foldMap scoreTrick))
            let scores = case Player.findIndex (== 26) initialScores of
                  Nothing -> initialScores
                  Just i -> set (playerData i) 26 (pure 0)
            let newGame = over (field @"scores") (scores <>) g
            pure $ case Player.findIndex (>= 100) (g ^. field @"scores") of
              Nothing -> newGame
              Just _ -> set (field @"finished") True newGame
          else g
      playCard :: Game -> Game
      playCard g = fromMaybe g do
        index <- playingNext g
        let addCardToTrick =
              field @"trick" . _Just . _2 . playerData index
                ?~ card
        let removeCardFromHand =
              field @"hands" . _Just . playerData index
                %~ Vector.filter (/= card)
        pure ((removeCardFromHand >>> addCardToTrick >>> maybeBreakHearts) g)
      maybeBreakHearts :: Game -> Game
      maybeBreakHearts g =
        set (field @"heartsBroken") (heartsBroken g || suit card == Hearts) g
      notFollowingSuitWhenRequired :: Bool
      notFollowingSuitWhenRequired = fromMaybe False do
        hands <- game ^. field @"hands"
        next <- playingNext game
        pure (not (checkFollowingSuit game hands next card))
      breakingHeartsIncorrectly :: Bool
      breakingHeartsIncorrectly =
        leading && suit card == Hearts
          && not (heartsBroken game)
          && hasNonHearts
      playedPenaltyCardIllegally :: Bool
      playedPenaltyCardIllegally =
        null (tricks game) && isPenaltyCard
      isPenaltyCard :: Bool
      isPenaltyCard =
        (suit card == Hearts)
          || (suit card == Spades && value card == Queen)
      leading :: Bool
      leading =
        all
          isNothing
          (maybe [Nothing] (toList . snd) (game ^. field @"trick"))
      hasNonHearts :: Bool
      hasNonHearts = fromMaybe False do
        index <- playingNext game
        playerHand <- view (playerData index) <$> (game ^. field @"hands")
        pure (any ((/= Hearts) . suit) playerHand)
   in do
        when
          notFollowingSuitWhenRequired
          (Left (PlayInvalid NotFollowingSuit game playEvent))
        when
          breakingHeartsIncorrectly
          (Left (PlayInvalid BreakingHeartsIncorrectly game playEvent))
        when
          playedPenaltyCardIllegally
          (Left (PlayInvalid PlayedPenaltyCardIllegally game playEvent))
        Right (maybeScoreHand (maybeDiscardTrick (playCard game)))

-- | Played next, would this card follow suit (or not) correctly?
checkFollowingSuit :: Game -> FourPlayers (Vector Card) -> PlayerIndex -> Card -> Bool
checkFollowingSuit game hands nextPlayer card =
  isFollowingSuit || not shouldFollowSuit
  where
    firstCard :: Maybe Card
    firstCard = do
      (startingPlayer, trick) <- game ^. field @"trick"
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

playingNext :: Game -> Maybe PlayerIndex
playingNext Game{trick, tricks} =
  checkCurrentTrick <|> checkLastTrick
  where
    checkCurrentTrick :: Maybe PlayerIndex
    checkCurrentTrick = do
      (firstPlayer, t) <- trick
      let cards :: [(PlayerIndex, Maybe Card)]
          cards = Player.playOrder firstPlayer ((,) <$> indices <*> t)
      fmap fst (find (isNothing . snd) cards)
    checkLastTrick = do
      ts <- tricks
      t <- guard (not (Vector.null ts)) $> Vector.last ts
      pure (winner t)
    indices = FourPlayers PlayerOne PlayerTwo PlayerThree PlayerFour

playingNext' :: Game -> Maybe Player.Id
playingNext' g = do
  i <- playingNext g
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
