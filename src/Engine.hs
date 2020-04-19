{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Engine where

import Tools
import EntityType
import GameState
import Entity

import Control.Lens
import Control.Monad.State
import Data.Default
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import TextShow
import qualified Data.List.Safe as SL
import Control.Monad.Extra
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

addAlert :: AlertID -> Alert -> App ()
addAlert aID a = modify $ \s -> s & alerts %~ M.insert aID a

removeAlert :: AlertID -> App ()
removeAlert aID = modify $ \s -> s & alerts %~ M.delete aID

registerAchievement :: AchievementID -> App ()
registerAchievement aID = modify $ over remainingAchievements (S.insert aID)

addAchievement :: Achievement -> App ()
addAchievement a@(Achievement aID aContent) = do
  logT $ "\n***ACHIEVEMENT UNLOCKED***\n" <> aID <> "\n" <> aContent
  modify (\s -> s & achievements %~ M.insert aID a)
  modify (\s -> s & remainingAchievements %~ S.delete aID)

hasAchievement :: AchievementID -> App Bool
hasAchievement aID = do
  as <- gets (view achievements)
  return $ M.member aID as

setGameOver :: App ()
setGameOver = modify $ set gameOver True

-- Increment game time by one.
incrementClock :: App ()
incrementClock = modify $ over clock (+1)

describeCurrentTurn :: App Text
describeCurrentTurn = do
  st <- get
  p <- getPlayer
  l <- getEntity $ p^.?locationID
  es <- filter (\e -> entityType e /= Player) <$> getEntitiesAt (l^.?entityID)
  clock <- gets (view clock)
  lDesc <- getDescription $ l^.?entityID
  alertsMap <- gets (view alerts)
  let header = Just $ "\n" <> T.toUpper (l^.?name) <> "\n" <> T.replicate (T.length $ l^.?name) "="
      desc = Just lDesc
      alerts = case snd <$> M.toList alertsMap of
                 [] -> Nothing
                 as -> Just $ T.intercalate "\n" as
      thingsHere =
        case length es of
          0 -> Nothing
          _ -> Just $ "\nYou can see: " <> (T.intercalate ", " ((^.?name) <$> es) <> "\n")
  directions <-
    sequence 
    $ mapMaybe
      (\(dirLens, dirString) ->
        case l^.dirLens of
          Nothing -> Nothing
          Just lID -> Just $ do
            l <- getEntity lID
            return $ dirString <> " is the " <> l^.?name)
      [ (toNorth, "To the North")
      , (toEast, "To the East")
      , (toSouth, "To the South")
      , (toWest, "To the West")
      , (toUp, "Above you")
      , (toDown, "Below you")
      ]

  return $ T.intercalate "\n" (catMaybes [header, desc, alerts, thingsHere] ++ directions)
