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
  , "...and so remember - please - and repeat: STAY AT HOME. REMEMBER THE NHS. DEFY DEATH. STAY AT..."
  ] ++ repeat "...HOME. REMEMBER THE NHS. DEFY DEATH. STAY AT..."

bedroomDesc eID = do
  e <- getEntity eID
  clock <- gets (view clock)
  radio <- getOnlyEntity Radio
  let lines = [ if not (e^.?visited) then Just "You awake on Day 812 of The Quarantine.\n\nHalf-forgetten dreams of exponential curves leave you as you reluctantly get out of bed." else Nothing
              , Just "This is your childhood bedroom.\nThe indigo wallpaper you chose for your tenth birthday peels from the walls in uneven patches.\nThe bed is single and sweat-damp - the walls too, ever since they closed up all the vents."
              , if radio^.?onOff == On then Just $ "\nThe wall radio blares a 24/7 cast of the Boris Johnson simulacrum:\n  \"" <> (radioLines !! fromIntegral clock) <> "\"" else Nothing
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
      when (clock == 5) $ do
        addAlert "Delivery" "You hear a frantic knocking at your front door. You should get that."
        hallway <- getLocationByName "hallway"
        deliveryMan <- mkHuman "delivery man" ["man", "delivery", "delivery man"] (hallway^.?entityID)
        desc deliveryMan (const $ return "You can't see him too well through the frosted glass, but you can hear him okay")

hallwayDesc eID = do
  e <- getEntity eID
  let canLeave = isJust $ e^.toNorth
  deliveryMan <- getTargetedEntitiesNearPlayer "delivery man"
  let lines = [ if not (e^.?visited) then Just "You shuffle into the hallway, suppressing a rattling cough." else Nothing
              , Just "Framed family photos thick with dust adorn the walls."
              , if not canLeave then Just "The front door - for ingress only, of course - is to the North. There's a delivery slot at the bottom."
                                else Just "The delivery slot on the front door is stuck open."
              , if not . null $ deliveryMan then Just "The ration delivery man is on the other side of the door, shouting at you to confirm receipt." else Nothing
              ]
  return $ T.unlines $ catMaybes lines

hatchDesc eID = do
  e <- getEntity eID
  -- TODO: Another line about being propped open once we have it
  let isOpen = e^.?openClosed == Open
      lines = [ Just "This is the hatch through which your rations get chucked. It opens for - I don't know, maybe two turns worth of time? - every week."
              , if isOpen then Just "The hatch is open, and you can see the forbidden pavement outside. It's been years since you set foot there." else Nothing
              ]
  return $ T.unlines $ catMaybes lines

buildCovidGame :: (MonadState GameState m) => m ()
buildCovidGame = do
  addAlert "Alarm" "Your alarm clock emits a shrill screech, signalling 05:00 - just half an hour until your shift starts."
  addWatcher alarmWatcher

  addWatcher deliveryKnockWatcher

  bedroom <- mkLocation "bedroom"
  desc bedroom bedroomDesc

  bed <- mkSimpleObj "your bed" ["bed"] (bedroom^.?entityID)
  desc bed (const $ return "Yellowed sheets last changed months ago cover a parabolic mattress. You want back in so, so badly.")

  hairband <- mkHairband (bedroom^.?entityID)
  desc hairband (const $ return "A faded elasticated hairband. Your head's big but it looks like it'd get around it.")

  player <- mkPlayer "yourself" $ bedroom^.?entityID
  desc player playerDesc

  radio <- mkRadio $ bedroom^.?entityID
  modifyEntity (set onOff $ Just On) (radio^.?entityID)
  desc radio radioDesc

  alarm <- mkAlarm $ bedroom ^.?entityID
  modifyEntity (set onOff $ Just On) (alarm^.?entityID)
  desc alarm alarmDesc

  hallway <- mkLocation "hallway"
  desc hallway hallwayDesc
  modifyEntity (set toNorth $ hallway^.entityID) (bedroom^.?entityID)
  modifyEntity (set toSouth $ bedroom^.entityID) (hallway^.?entityID)

  photos <- mkSimpleObj "photos" ["photo", "photos"] (hallway ^.?entityID)
  desc photos (const $ return "There's one of your parents looking happy a decade before you were born, and another of you nude in a public fountain chasing pigeons.")
  frontDoor <- mkSimpleObj "front door" ["door", "front door"] (hallway ^.?entityID)
  desc frontDoor (const $ return "This used to be your way to the outside world. Now it only yawns, once a week, to receive a box of rations through the hatch in the lower half.")
  hatch <- mkSimpleObj "hatch" ["hatch", "slot"] (hallway^.?entityID)
  desc hatch hatchDesc
  modifyEntity (set openClosed $ Just Closed) (hatch^.?entityID)

  -- TODO: A watcher that adds the front door once open
  -- TODO: A watcher to open and close the slot at the right time - first watcher opens it, second closes it
