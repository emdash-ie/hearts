{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hearts.Game (
  Event,
  processEvent,
  Game (..),
  dealAmong4,
  sortHand,
) where

import Control.Lens (ASetter, ASetter', over, to, (%~), (.~), (^.))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Generics.Product (field)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

import Hearts.Card
import qualified Hearts.Card as Card
import Hearts.Game.Event
import Hearts.Player (FourPlayers (..), ThreePlayers (..))
import qualified Hearts.Player as Player

data Game = Game
  { players :: FourPlayers Player.Id
  , scores :: FourPlayers (Sum Integer)
  , hands :: Maybe (FourPlayers (Vector Card))
  , trick :: Maybe (FourPlayers (Maybe Card))
  , tricks :: Maybe (FourPlayers (Vector Card))
  }
  deriving (Show, Eq, Generic)

data FoldError
  = -- | This event (which isn't a 'Start') doesn't make sense, because the game
    -- has not yet started.
    GameNotStarted Event
  | -- | This 'Game' has already started, so this 'StartEvent' can't be applied.
    GameAlreadyStarted Game StartEvent
  deriving (Show)

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
  let updateHands = field @"hands" .~ Just (sortHand <$> dealAmong4 deck)
   in Right (updateHands game)
processEvent (Just game) (Play PlayEvent{}) =
  let trickIsFinished :: FourPlayers (Maybe Card) -> Maybe (FourPlayers Card)
      trickIsFinished = \FourPlayers{..} ->
        case (one, two, three, four) of
          (Just one', Just two', Just three', Just four') ->
            Just (FourPlayers{one = one', two = two', three = three', four = four'})
          _ -> Nothing
      maybeDiscardTrick :: Game -> Game
      maybeDiscardTrick g = fromMaybe g do
        trick :: FourPlayers Card <- g ^. field @"trick" . to (>>= trickIsFinished)
        pure
          ( g & field @"tricks" %~ fmap (fmap (flip Vector.snoc) trick <*>)
              & field @"trick" .~ Nothing
          )
      handIsFinished :: Maybe (FourPlayers (Vector a)) -> Bool
      handIsFinished = maybe False \FourPlayers{..} ->
        all ((== 0) . Vector.length) [one, two, three, four]
      maybeScoreHand :: Game -> Game
      maybeScoreHand g =
        if g ^. field @"hands" . to handIsFinished
          then fromMaybe g do
            updateScores <-
              g
                ^. field @"tricks"
                  . to (fmap (fmap (foldMap Card.score)))
            pure (over (field @"scores") (updateScores <>) g)
          else g
   in Right (maybeScoreHand (maybeDiscardTrick game))

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
