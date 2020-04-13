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
      alarm <- getOnlyEntity Alarm
      let lines = [ if not (e^.?visited) then Just "You awake on Day 812 of The Quarantine. Half-forgetten dreams of exponential curves leave you as you take in your surroundings. You stand." else Nothing
                  , Just "This is the bedroom you got stuck with when the military started their patrols. The bed is sweat-damp - the walls too, since they closed the vents."
                  ]
      return $ T.unlines $ catMaybes lines

radioDesc :: Description
radioDesc st eID = evalState go st
  where
    go = do
      e <- getEntity eID
      let lines = [ Just "A battery-powered FM radio - they had to turn the signal back on last year. Government issue. "
                  , if e^.?onOff == On then Just "It's on, and blaring headlines at you. It'll run out of power soon." else Just "It's dialled to static right now."
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

alarmWatcher :: Watcher
alarmWatcher st = let (_, st') = runState go st in st'
  where
    go = do
      e <- getOnlyEntity Alarm
      when ((e^.?onOff) == Off) $ removeAlert "Alarm"

buildCovidGame :: (MonadState GameState m) => m ()
buildCovidGame = do
  addAlert "Alarm" "Your alarm clock is emitting a shrill screech, signalling 05:00 - just half an hour until your shift starts."
  addWatcher alarmWatcher

  bedroom <- mkLocation "your Bedroom"
  addDesc (bedroom^.?entityID) bedroomDesc

  player <- mkPlayer "yourself" $ bedroom^.?entityID
  addDesc (player^.?entityID) (\_ _ -> "You look like shit.")

  radio <- mkRadio $ bedroom^.?entityID
  modifyEntity (set onOff $ Just On) (radio^.?entityID)
  addDesc (radio^.?entityID) radioDesc

  alarm <- mkAlarm $ bedroom ^.?entityID
  modifyEntity (set onOff $ Just On) (alarm^.?entityID)
  addDesc (alarm^.?entityID) alarmDesc
