{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidGame where

import Game
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Maybe

bedroomDesc :: Description
bedroomDesc st eID = evalState go st
  where
    go = do
      e <- getEntity eID
      alarm <- getOnlyEntity Alarm
      let lines = [ Just "You awake on Day 812 of your quarantine."
                  , if alarm^.?onOff == On then Just "Your alarm clock emits a shrill screech, signalling 05:00." else Nothing
                  ]
      -- TODO: Need to add the USE command to turn off the alarm
      return $ T.unlines $ catMaybes lines

buildCovidGame :: (MonadState GameState m) => m ()
buildCovidGame = do
  bedroom <- mkLocation "the Bedroom"
  addDesc (bedroom^.?entityID) bedroomDesc

  player <- mkPlayer "Player" $ bedroom^.?entityID
  addDesc (player^.?entityID) (\_ _ -> "You look like shit.")

  radio <- mkRadio $ bedroom^.?entityID
  modifyEntity (set onOff $ Just On) (radio^.?entityID)
  addDesc (radio^.?entityID) (\_ _ -> "A battery-powered FM radio - they had to turn the signal back on. It'll run out soon.")

  alarm <- mkAlarm $ bedroom ^.?entityID
  modifyEntity (set onOff $ Just On) (alarm^.?entityID)
  addDesc (radio^.?entityID) (\_ _ -> "A battery-powered FM radio - they had to turn the signal back on. It'll run out soon.")
