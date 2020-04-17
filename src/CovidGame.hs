{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidGame where

import Tools
import GameState
import EntityType
import Engine
import Handler
import Entity
import Instruction

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
  radio <- getOneEntityByName SimpleObj "radio"
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

alarmWatcher = do
  e <- getOneEntityByName SimpleObj "alarm clock"
  when ((e^.?onOff) == Off) $ removeAlert "Alarm"

deliveryKnockWatcher = do
  time <- gets (view clock)
  deliveryMan <- getEntityByName Human "delivery man"
  when (time == 5 && isNothing deliveryMan) $ do
    addAlert "Delivery" "You hear a frantic knocking at your front door. You should get that."
    hallway <- getLocationByName "hallway"
    deliveryMan <- mkHuman "delivery man" ["man", "delivery", "delivery man"] (hallway^.?entityID)
    modifyEntity (set talkable Talkable) (deliveryMan^.?entityID)
    desc deliveryMan (const $ return "You can't see him too well through the frosted glass, but you can hear him okay")
    addTalkToHandler (deliveryMan^.?entityID) $ do
      incrementClock
      logT "\"Fuckin' finally man! What took you? I gotta make hundreds more of these today to get mine!\""
      logT "The delivery guy swipes a card, throwing open the hatch, slides a ration box through, and leaves hurriedly."
      logT "The box tips over, spilling some of its contents."

      -- The guy leaves, the hatch opens
      removeAlert "Delivery"
      removeEntity $ deliveryMan^.?entityID
      hatch <- getOneEntityByName SimpleObj "hatch"
      modifyEntity (set openClosed $ Just Open) (hatch^.?entityID)

      -- Can now get to the street
      hallway <- getLocationByName "hallway"
      street <- getLocationByName "street"
      modifyEntity (set toNorth $ Just (street^.?entityID)) (hallway^.?entityID)

      -- The hatch shuts after 2 more goes
      oldTime <- gets (view clock)
      addWatcher $ do
        hatch <- getOneEntityByName SimpleObj "hatch"
        newTime <- gets (view clock)
        plunger <- getOneEntityByName SimpleObj "plunger"
        when (hatch^.?openClosed == Open && newTime >= oldTime + 1 && (plunger^.locationID) /= (hatch ^.entityID)) $ do
          addAlert "HatchShut" "The hatch on the front door has slammed shut, and won't reopen for another week."
          modifyEntity (set openClosed $ Just Closed) (hatch^.?entityID)

      -- The rations arrive
      paperPlate <- mkSimpleObj "paper plate" ["plate", "paper plate"] (Just $ hallway^.?entityID)
      modifyEntity (set storable Storable) (paperPlate^.?entityID)
      desc paperPlate (const $ return "A paper plate, like we used to use at picnics in the before times.")

      rationBox <- mkSimpleObj "ration box" ["ration box", "box"] (Just $ hallway^.?entityID)
      desc rationBox (const $ return "A brown cardboard box.")
      modifyEntity (set storable Storable) (rationBox^.?entityID)
      addOpenHandler (rationBox^.?entityID) (const $ logT "The box tipped over and lays open on its side")

      rations <- mkSimpleObj "assorted rations" ["rations"] (Just $ hallway^.?entityID)
      desc rations (const $ return "Assorted rations - pouches of dehydrated egg, carbohydrate gunge, that sort of thing.")
      modifyEntity (set storable Storable) (rations^.?entityID)
      modifyEntity (set edible Edible) (rations^.?entityID)
      addEatHandler (rations^.?entityID) (\eID -> do
        p <- getPlayer
        logT "You eat the full week's supply of rations in one go. Aren't you worried you'll need those later?"
        addAchievement $ Achievement "Eyes Bigger Than Belly" "You're gonna regret that"
        modifyPlayer (over inventory (fmap (S.delete eID)))
        removeEntity eID)

      -- Now that the plate exists, can add the handler
      hairband <- getOneEntityByName SimpleObj "hairband"
      addCombinationHandler (paperPlate^.?entityID) (hairband^.?entityID) (\plateID hairbandID -> do
        logT "Using your medical expertise and surgical dexterity, you fashion a fucking facemask out of these two unlikely items."
        modifyPlayer (over inventory (fmap (S.delete plateID)))
        modifyPlayer (over inventory (fmap (S.delete hairbandID)))
        removeEntity plateID
        removeEntity hairbandID
        mask <- mkSimpleObj "makeshift facemask" ["facemask", "mask"] Nothing
        modifyEntity (set wearable Wearable) (mask^.?entityID)
        desc mask (const $ return "A super-safe, military grade, virus-repellant face mask.")
        modifyPlayer (over inventory (fmap (S.insert $ mask^.?entityID))))

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

streetEndgameWatcher = do
  l <- getPlayerLocation
  when (l^.?name == "street") setGameOver

bathDesc eID = do
  e <- getEntity eID
  let isOn = e^.?onOff == On
  let lines = [ Just "A crust of your skin coats the bottom of the freestanding tub. You ran out of domestic cleaning products in the first month of Quarantine. There is no plug."
              , if isOn then Just "The water enters the overflow, and the taps continue to pour." else Just "The taps are off right now. Don't waste water."
              ]
  return $ T.unlines $ catMaybes lines

alarmBathWatcher = do
  alarm <- getOneEntityByName SimpleObj "alarm clock"
  bath <- getOneEntityByName SimpleObj "bath"
  hasCheev <- hasAchievement "Big Wet Clock"
  when (not hasCheev && alarm^.locationID == bath^.entityID && bath^.?onOff == On)
    $ addAchievement $ Achievement "Big Wet Clock" "Why did you do this???"

buildCovidGame = do
  addAlert "Alarm" "Your alarm clock emits a shrill screech, signalling 05:00 - just half an hour until your shift starts."
  addWatcher alarmWatcher

  addWatcher deliveryKnockWatcher

  bedroom <- mkLocation "bedroom"
  desc bedroom bedroomDesc

  bed <- mkSimpleObj "bed" ["bed"] (Just $ bedroom^.?entityID)
  desc bed (const $ return "Yellowed sheets last changed months ago cover a parabolic mattress. You want back in so, so badly.")

  hairband <- mkSimpleObj "elasticated hairband" ["hairband", "band", "headband"] (Just $ bedroom^.?entityID)
  modifyEntity (set storable Storable) (hairband^.?entityID)
  desc hairband (const $ return "A faded elasticated hairband. Your head's big but it looks like it'd get around it.")

  player <- mkPlayer "yourself" $ bedroom^.?entityID
  desc player (const $ return "You look like shit. Beard well past shoulder-length yet still patchy after all this time.")

  radio <- mkSimpleObj "radio" ["radio"] (Just $ bedroom^.?entityID)
  modifyEntity (set onOff $ Just On) (radio^.?entityID)
  desc radio radioDesc
  addTurnOnHandler (radio^.?entityID) (\eID -> do
    e <- getEntity eID
    case e^.?onOff of
      Off -> do
        logT "You twist the dial until the bad news starts to roll once more."
        modifyEntity (set onOff $ Just On) (e^.?entityID)
      On -> logT "Already chirping away, friend.")
  addTurnOffHandler (radio^.?entityID) (\eID -> do
    e <- getEntity eID
    case e^.?onOff of
      Off -> logT "The radio is already off."
      On -> do
        logT "There's no off switch, but you dial your way to the most quiet static you can find."
        modifyEntity (set onOff $ Just Off) (e^.?entityID))

  alarm <- mkSimpleObj "alarm clock" ["alarm", "clock", "alarm clock"] (Just $ bedroom^.?entityID)
  modifyEntity (set storable Storable) (alarm^.?entityID)
  modifyEntity (set onOff $ Just On) (alarm^.?entityID)
  desc alarm alarmDesc
  addTurnOnHandler (alarm^.?entityID) (const $ logT "You can't turn an alarm on at will, man. Time only goes one way.")
  addTurnOffHandler (alarm^.?entityID) (\eID -> do
    e <- getEntity eID
    case e^.?onOff of
      Off -> logT "You already took care of that, chap."
      On -> do
        logT "You slam a calloused hand onto the rusty metal bells, and the alarm is silenced."
        modifyEntity (set onOff $ Just Off) (e^.?entityID))

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
  addTurnOnHandler (bath^.?entityID) (\eID -> do
    logT "You turn the rusty taps, and water floods the rotten tub. It quickly reaches the overflow."
    modifyEntity (set onOff $ Just On) eID)
  addTurnOffHandler (bath^.?entityID) (\eID -> do
    logT "You turn off the taps and the water quickly drains through the open plug."
    modifyEntity (set onOff $ Just Off) eID)
  
  addWatcher alarmBathWatcher
  addWatcher streetEndgameWatcher

  -- Add the radio ending
  addSayHandler (\content -> do
    l <- getPlayerLocation
    when ( (l^.?name) == "bedroom" && T.isInfixOf "HOME" (T.toUpper content) && T.isInfixOf "NHS" (T.toUpper content) && T.isInfixOf "DEATH" (T.toUpper content) ) $ do
      addAchievement $ Achievement "Simon Says" "Do you do everything you hear on the radio?"
      setGameOver
      ap <- mkLocation "Astral Plane"
      desc ap (const $ return
        $ "As you recite the mantra on the radio, you lose touch with your corporeal body.\n"
        <> "You feel yourself becoming one with the simulacrum as you continue your chant.\n"
        <> "Hours pass - then days - and your lips chap with thirst. Still you chant.\n"
        <> "Your body expires, but your immortal soul may yet live on in the Hancock Machine.")
      modifyPlayer (set locationID $ Just $ ap^.?entityID))

  -- You can say anything to the delivery guy
  addSayHandler (\_ -> do
    l <- getPlayerLocation
    deliveryMan <- getEntityByName Human "delivery man"
    when ( (l^.?name) == "hallway" && isJust deliveryMan) $ enactInstruction (TalkTo "delivery man"))

  addCombinationHandler (plunger^.?entityID) (hatch^.?entityID) (\plungerID hatchID -> do
    plunger <- getEntity plungerID
    hatch <- getEntity hatchID
    if hatch^.?openClosed == Closed
       then logT "The hatch is closed."
       else do
         logT "You use the filthy plunger to prop open the delivery hatch."
         modifyEntity (set locationID $ Just (hatch^.?entityID)) (plunger^.?entityID)
         modifyPlayer (over inventory (fmap (S.delete $ plunger^.?entityID))))

  addCombinationHandler (alarm^.?entityID) (bath^.?entityID) (\alarmID bathID -> do
    logT "You place the alarm clock into the bath, like a normal person would."
    modifyPlayer (over inventory (fmap (S.delete alarmID)))
    modifyEntity (set locationID $ Just bathID) alarmID)

  addOpenHandler (frontDoor^.?entityID) (const $ logT "This hasn't been opened in years. Government mandate. You wouldn't want that air coming in, anyhow.\nYour voice would carry through it.")
  addOpenHandler (hatch^.?entityID) (\eID -> do
    e <- getEntity eID
    case e^.?openClosed of
      Open -> logT "The hatch is already open"
      Closed -> logT "This can only be opened from the outside for deliveries.")
