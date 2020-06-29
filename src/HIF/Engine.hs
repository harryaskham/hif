{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}

module HIF.Engine where

import HIF.Tools
import HIF.EntityType
import HIF.GameState
import HIF.Entity

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
import System.IO

registerGameBuilder :: App () -> App ()
registerGameBuilder b = modify $ set gameBuilder (Just b)

addAlert :: AlertID -> Alert -> App ()
addAlert aID a = modify $ \s -> s & alerts %~ M.insert aID a

removeAlert :: AlertID -> App ()
removeAlert aID = modify $ \s -> s & alerts %~ M.delete aID

registerAchievement :: AchievementID -> App ()
registerAchievement aID = modify $ over remainingAchievements (S.insert aID)

addAchievement :: Achievement -> App ()
addAchievement a@(Achievement aID aContent) = do
  hasIt <- hasAchievement aID
  when (not hasIt) do
    logT $ "***ACHIEVEMENT UNLOCKED***\n" <> aID <> "\n" <> aContent <> "\n"
    modify (\s -> s & achievements %~ M.insert aID a)
    modify (\s -> s & remainingAchievements %~ S.delete aID)

hasAchievement :: AchievementID -> App Bool
hasAchievement aID = do
  as <- gets (view achievements)
  return $ M.member aID as

setCondition :: Condition -> App ()
setCondition c = modify $ over conditions (S.insert c)

conditionMet :: Condition -> App Bool
conditionMet c = do
  cs <- gets (view conditions)
  return $ c `S.member` cs

removeCondition :: Condition -> App ()
removeCondition c = modify $ over conditions (S.delete c)

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
