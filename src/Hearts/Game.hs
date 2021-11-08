{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
  dealAmong4,
  sortHand,
  foldEvents,
  toPlayerGame,
  playingNext,
  playingNext',
) where

import Control.Applicative ((<|>))
import Control.Lens (ASetter, ASetter', over, set, to, (%~), (.~), (?~), (^.), _2, _Just)
import Control.Monad (guard)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Foldable (foldl', toList)
import Data.Function ((&))
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

import Control.Category ((>>>))
import Hearts.Card
import qualified Hearts.Card as Card
import Hearts.Game.Event
import Hearts.Player (FourPlayers (..), PlayerIndex (..), ThreePlayers (..), Trick, playerData)
import qualified Hearts.Player as Player

data Game = Game
  { players :: FourPlayers Player.Id
  , scores :: FourPlayers (Sum Integer)
  , hands :: Maybe (FourPlayers (Vector Card))
  , trick :: Maybe (Trick Maybe)
  , tricks :: Maybe (Vector (Trick Identity))
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
      ]

toPlayerGame :: PlayerIndex -> Game -> Player.Game
toPlayerGame index Game{..} =
  Player.Game
    { hand = Player.getPlayerData index <$> hands
    , ..
    }

data FoldError
  = -- | This event (which isn't a 'Start') doesn't make sense, because the game
    -- has not yet started.
    GameNotStarted Event
  | -- | This 'Game' has already started, so this 'StartEvent' can't be applied.
    GameAlreadyStarted Game StartEvent
  deriving (Show, Generic)

instance Aeson.ToJSON FoldError

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
      }
processEvent Nothing event = Left (GameNotStarted event)
processEvent (Just game) (Start event) = Left (GameAlreadyStarted game event)
processEvent (Just game) (Deal (DealEvent (Deck deck))) =
  let hands = sortHand <$> dealAmong4 deck
      updateHands = field @"hands" .~ Just hands
      initialiseTrick :: Game -> Game
      initialiseTrick =
        field @"trick" .~ fmap (,pure Nothing) (playingFirst hands)
   in Right (initialiseTrick (updateHands game))
processEvent (Just game) (Play PlayEvent{card}) =
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
            scores <-
              g
                ^. field @"tricks"
                  . to (fmap (foldMap scoreTrick))
            pure (over (field @"scores") (scores <>) g)
          else g
      playCard :: Game -> Game
      playCard g = fromMaybe g do
        index <- playingNext g
        let addCardToTrick = field @"trick" . _Just . _2 . playerData index ?~ card
        let removeCardFromHand = field @"hands" . _Just . playerData index %~ Vector.filter (/= card)
        pure ((removeCardFromHand >>> addCardToTrick) g)
   in Right (maybeScoreHand (maybeDiscardTrick (playCard game)))

scoreTrick :: Trick Identity -> FourPlayers (Sum Integer)
scoreTrick t =
  let w = winner t
      winnerScore = foldMap Card.score (t ^. to snd . playerData w)
   in set (playerData w) winnerScore (pure 0)

playingNext :: Game -> Maybe PlayerIndex
playingNext Game{trick, tricks} =
  checkCurrentTrick <|> checkLastTrick
  where
    checkCurrentTrick :: Maybe PlayerIndex
    checkCurrentTrick = do
      (firstPlayer, t) <- trick
      let cards :: [(PlayerIndex, Maybe Card)]
          cards = playOrder firstPlayer ((,) <$> indices <*> t)
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

playOrder :: PlayerIndex -> FourPlayers a -> [a]
playOrder index pd =
  let as = (`Player.getPlayerData` pd) <$> cycle [minBound .. maxBound]
   in take
        (fromEnum (maxBound :: PlayerIndex) - fromEnum (minBound :: PlayerIndex) + 1)
        (drop (fromEnum index) as)

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
