{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidGame where

import Game
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad
import Data.Maybe

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

alarmWatcher = do
  e <- getOnlyEntity Alarm
  when ((e^.?onOff) == Off) $ removeAlert "Alarm"

deliveryKnockWatcher = do
  clock <- gets (view clock)
  deliveryMan <- getEntityByName Human "delivery man"
  when (clock == 5 && isNothing deliveryMan) $ do
    addAlert "Delivery" "You hear a frantic knocking at your front door. You should get that."
    hallway <- getLocationByName "hallway"
    deliveryMan <- mkHuman "delivery man" ["man", "delivery", "delivery man"] (hallway^.?entityID)
    desc deliveryMan (const $ return "You can't see him too well through the frosted glass, but you can hear him okay")

hallwayDesc eID = do
  e <- getEntity eID
  deliveryMan <- getEntityByName Human "delivery man"
  hatch <- getOneEntityByName SimpleObj "hatch"
  let lines = [ if not (e^.?visited) then Just "You shuffle into the hallway, suppressing a rattling cough." else Nothing
              , Just "Framed family photos thick with dust adorn the walls."
              , Just "The front door - for ingress only, of course - is to the North. There's a delivery slot at the bottom."
              , Just "The flat extends to the West, but I didn't have time to write that code yet, so don't go there."
              , if (hatch^.?openClosed) == Open then Just "The delivery slot on the front door is stuck open - you could probably fit through..." else Nothing
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

streetDesc eID = do
  e <- getEntity eID
  p <- getPlayer
  maskM <- getEntityByName SimpleObj "makeshift facemask" 
  let wearingMask = case maskM of
                      Nothing -> False
                      Just mask -> (mask^.?entityID) `S.member` (p^.?wearing) 
  if wearingMask
     then return "Your roughshod PPE holds strong as you emerge into the street for the first time in years. What challenges await? Who knows, these things take fucking ages to write. You win, in any case."
     else return
       $ "You emerge into the street for the first time in years. As you crawl out of the hatch, your fingers blister upon contact with the harsh and "
       <> "infected concrete. Your first free breath scours your throat and lungs - you are unable to take a second. Why did you go outside without wearing a mask?"

bathDesc eID = do
  e <- getEntity eID
  let isOn = e^.?onOff == On
  let lines = [ Just "A crust of your skin coats the bottom of the freestanding tub. You ran out of domestic cleaning products in the first month of Quarantine. There is no plug."
              , if isOn then Just "The water enters the overflow, and the taps continue to pour." else Just "The taps are off right now. Don't waste water."
              ]
  return $ T.unlines $ catMaybes lines

alarmBathWatcher :: (MonadState GameState m, MonadIO m) => m ()
alarmBathWatcher = do
  alarm <- getOnlyEntity Alarm
  bath <- getOneEntityByName SimpleObj "bath"
  hasCheev <- hasAchievement "Big Wet Clock"
  when (not hasCheev && alarm^.locationID == bath^.entityID && bath^.?onOff == On)
    $ addAchievement $ Achievement "Big Wet Clock" "Why did you do this???"

buildCovidGame :: (MonadState GameState m) => m ()
buildCovidGame = do
  addAlert "Alarm" "Your alarm clock emits a shrill screech, signalling 05:00 - just half an hour until your shift starts."
  addWatcher alarmWatcher

  addWatcher deliveryKnockWatcher

  bedroom <- mkLocation "bedroom"
  desc bedroom bedroomDesc

  bed <- mkSimpleObj "your bed" ["bed"] (Just $ bedroom^.?entityID)
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

  photos <- mkSimpleObj "photos" ["photo", "photos"] (Just $ hallway ^.?entityID)
  desc photos (const $ return "There's one of your parents looking happy a decade before you were born, and another of you nude in a public fountain chasing pigeons.")
  frontDoor <- mkSimpleObj "front door" ["door", "front door"] (Just $ hallway ^.?entityID)
  desc frontDoor (const $ return "This used to be your way to the outside world. Now it only yawns, once a week, to receive a box of rations through the hatch in the lower half.")
  hatch <- mkSimpleObj "hatch" ["hatch", "slot"] (Just $ hallway^.?entityID)
  desc hatch hatchDesc
  modifyEntity (set openClosed $ Just Closed) (hatch^.?entityID)

  street <- mkLocation "street"
  desc street streetDesc

  bathroom <- mkLocation "bathroom"
  modifyEntity (set toEast $ bathroom^.entityID) (hallway^.?entityID)
  modifyEntity (set toWest $ hallway^.entityID) (bathroom^.?entityID)
  desc bathroom (const $ return "You certainly haven't been following the sterilisation guidelines in here.\nAn exotic mould snakes down from the ceiling.\nYou catch a glimpse of yourself in the mirror and recoil in disgust.")

  plunger <- mkSimpleObj "plunger" ["plunger"] (Just $ bathroom^.?entityID)
  desc plunger (const $ return "A well-used, not-so-well-cleaned toilet plunger. It's about the width of that hatch out there.")
  modifyEntity (set storable Storable) (plunger^.?entityID)

  bath <- mkSimpleObj "bath" ["bath", "tub", "bathtub", "tap", "taps"] (Just $ bathroom^.?entityID)
  modifyEntity (set onOff $ Just Off) (bath^.?entityID)
  desc bath bathDesc
  
  addWatcher alarmBathWatcher
