{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidGame where

import Game
import Control.Monad.State
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad
import Data.Maybe

-- Helpers to build out descriptions
buildDescription f st eID = evalState (f eID) st
desc e d = addDesc (e^.?entityID) (buildDescription d)

radioLines =
  [ "... absolutely certain that we can keep deaths below twenty - maybe even fift<STATIC>..."
  , "...<STATIC>illion. I'm told that this morning's Hancock Simulation puts a vaccine only a few weeks away..."
  , "...and so remember. Repeat: STAY AT HOME. REMEMBER THE NHS. DEFY DEATH. STAY AT..."
  ] ++ repeat "...HOME. REMEMBER THE NHS. DEFY DEATH. STAY AT..."

bedroomDesc eID = do
  e <- getEntity eID
  clock <- gets (view clock)
  radio <- getOnlyEntity Radio
  let lines = [ if not (e^.?visited) then Just "You awake on Day 812 of The Quarantine.\n\nHalf-forgetten dreams of exponential curves leave you as you reluctantly get out of bed." else Nothing
              , Just "This is your childhood bedroom.\nThe indigo wallpaper you chose for your tenth birthday peels from the walls in uneven patches.\nThe bed is single and sweat-damp - the walls too, ever since they closed up all the vents."
              , if radio^.?onOff == On then Just $ "The wall radio blares a 24/7 cast of the Boris Johnson simulacrum:\n  \"" <> (radioLines !! fromIntegral clock) <> "\"" else Nothing
              ]
  return $ T.unlines $ catMaybes lines

radioDesc eID = do
  e <- getEntity eID
  let lines = [ Just "A wall-mounted FM radio - they had to turn the signal back on last year. Government issue."
              , if e^.?onOff == On then Just "It's on, and blaring headlines at you." else Just "It's dialled to static right now."
              ]
  return $ T.unlines $ catMaybes lines

alarmDesc eID = do
  e <- getEntity eID
  let lines = [ Just "An oldey-timey alarm clock with those two ringing bells. It's nailed to the bedside table. Where did you even get one of these?"
              , if e^.?onOff == On then Just "It tolls fiercly for you." else Just "Mercifully, it's silent."
              ]
  return $ T.unlines $ catMaybes lines

-- TODO: Hint at needing to wash to progress
playerDesc eID = return "You look like shit. Beard well past shoulder-length but still patchy after all this time."

alarmWatcher :: Watcher
alarmWatcher = execState go
  where
    go = do
      e <- getOnlyEntity Alarm
      when ((e^.?onOff) == Off) $ removeAlert "Alarm"

deliveryKnockWatcher :: Watcher
deliveryKnockWatcher = execState go
  where
    go = do
      clock <- gets (view clock)
      when (clock == 2) $ addAlert "Delivery" "You hear a frantic knocking at your front door. You should get that."

buildCovidGame :: (MonadState GameState m) => m ()
buildCovidGame = do
  addAlert "Alarm" "Your alarm clock emits a shrill screech, signalling 05:00 - just half an hour until your shift starts."
  addWatcher alarmWatcher

  addWatcher deliveryKnockWatcher

  bedroom <- mkLocation "your Bedroom"
  desc bedroom bedroomDesc

  bed <- mkSimpleObj "your bed" ["bed"] (bedroom^.?entityID)
  desc bed (const $ return "Yellowed sheets last changed months ago cover a parabolic mattress.")

  player <- mkPlayer "yourself" $ bedroom^.?entityID
  desc player playerDesc

  radio <- mkRadio $ bedroom^.?entityID
  modifyEntity (set onOff $ Just On) (radio^.?entityID)
  desc radio radioDesc

  alarm <- mkAlarm $ bedroom ^.?entityID
  modifyEntity (set onOff $ Just On) (alarm^.?entityID)
  desc alarm alarmDesc
