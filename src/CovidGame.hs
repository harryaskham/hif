{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidGame where

import Game
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad
import Data.Maybe

bedroomDesc :: Description
bedroomDesc st eID = evalState go st
  where
    go = do
      e <- getEntity eID
      radio <- getOnlyEntity Radio
      let lines = [ if not (e^.?visited) then Just "You awake on Day 812 of The Quarantine.\n\nHalf-forgetten dreams of exponential curves leave you as you take in your surroundings. You get out of bed." else Nothing
                  , Just "This is the bedroom you got stuck with when the military started their patrols. The bed is sweat-damp - the walls too, since they closed up all the vents."
                  , if radio^.?onOff == On then Just "The wall radio is on, as always." else Nothing
                  , Just "TODO: RADIO HEADLINES"
                  ]
      return $ T.unlines $ catMaybes lines

radioDesc :: Description
radioDesc st eID = evalState go st
  where
    go = do
      e <- getEntity eID
      let lines = [ Just "A wall-mounted FM radio - they had to turn the signal back on last year. Government issue."
                  , if e^.?onOff == On then Just "It's on, and blaring headlines at you." else Just "It's dialled to static right now."
                  ]
      return $ T.unlines $ catMaybes lines

alarmDesc :: Description
alarmDesc st eID = evalState go st
  where
    go = do
      e <- getEntity eID
      let lines = [ Just "An oldey-timey alarm clock with those two ringing bells. It's nailed to the bedside table. Where did you even get one of these?"
                  , if e^.?onOff == On then Just "It tolls fiercly for you." else Just "Mercifully, it's silent."
                  ]
      return $ T.unlines $ catMaybes lines

playerDesc :: Description
playerDesc st eID = evalState go st
  where
    -- TODO: Hint at needing a wash to progress
    go = return "You look like shit. Beard still patchy after all this time."

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
  addDesc (bedroom^.?entityID) bedroomDesc

  player <- mkPlayer "yourself" $ bedroom^.?entityID
  addDesc (player^.?entityID) playerDesc

  radio <- mkRadio $ bedroom^.?entityID
  modifyEntity (set onOff $ Just On) (radio^.?entityID)
  addDesc (radio^.?entityID) radioDesc

  alarm <- mkAlarm $ bedroom ^.?entityID
  modifyEntity (set onOff $ Just On) (alarm^.?entityID)
  addDesc (alarm^.?entityID) alarmDesc
