{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module FourthGame where

import Tools
import GameState
import EntityType
import Engine
import Handler
import Entity
import Instruction
import InstructionType

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Data.Maybe

{-
   TODO:
   - Ability to inspect the code of individuals, quine-style
   - 'Who am I'
   - 'Who is X'
   - 'What is X'
   - Passage of time induction storyline
   - Dialogue system
   - Conditionals, loops, recursion
   - To finish the game, you need to help the three folk out, and decohere with yourself sufficiently to break through the final barrier
   - Second terrified soul - you need to do something near them, make them realise you are constrained by simple verbs
   - Second soul is trapped in a loop - you can only break by doing something other than talking to them. Snap the band hanging above them.
   - Third soul is trying continuously to commit suicide - find him hanging. Give him the cleaver to help him out, and he'll go straight through his hand. Now you can give him the paw.
   - Third guy gives you something for the monk, which disappears the monk and lets you descend through the pedestal.
   
   - Web frontend
   - Save/Load
   - IO cleanup
   - Reword engine messaging / config
   - Wearing helpers
   - Turn off achievement display if there are none
   - 'get' handlers that explain why you can't get stuff - a default get handler just gets it
-}

buildFourthGame = do

  mainMenu <- mkLocation "Main Menu"
  describeC mainMenu
    $ T.intercalate "\n"
    [ "You find yourself reading the title screen of this game."
    , "Typing 'help' will tell you how to continue from here."
    , "As you acclimatise, the dim white of the walls around you becomes clearer."
    , "Somehow, you are also standing in an otherwise empty chamber with a single, simple opening."
    ]

  p <- mkPlayer "yourself" mainMenu
  describeC p
    $ T.intercalate "\n"
    [ "You are yourself. You focus on this truth as you move about the space around you."
    , "Elsewhere, you sit at a terminal, and the utterances you issue effect change in the world."
    , "Twice you inhabit space and twice you have position."
    , "As you inspect yourself, you notice that you are inspecting yourself, and you stop abruptly."
    ]

  clothes <- mkSimpleObj "clothes you are wearing" ["clothes", "clothing"] (Nothing :: Maybe Entity)
  modifyEntity (set wearable Wearable) clothes
  modifyEntity (set droppable Droppable) clothes
  modifyEntity (set storable Storable) clothes
  describe clothes (\e -> do
    p <- getPlayer
    isWearing <- p `isWearing` e
    return
      $ T.intercalate "\n"
      [ "These are the clothes you are currently wearing."
      , if isWearing
           then "You are currently wearing them."
           else "Somehow, you are also no longer wearing the clothes you are wearing."
      ])
  wearEntity clothes

  firstLocation <- mkLocation "first location"
  describe firstLocation (\e -> do
    -- TODO: Humming from the west if the monk is still there
    itemHere <- "item" `isANamedObjectAt` e
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "This is a chamber much like the last. Smooth platonic surfaces meet to form the interior of a cube."
      , Just "A plain staircase leads up through a hole in the ceiling."
      , cT itemHere "You notice an item - a small piece of logic - propped up against the side of the stairs."
      -- TODO: Remove forestry when monk disappears
      , Just "Through the western archway, the scent of forestry."
      ])
  firstLocation `isNorthOf` mainMenu

  item <- mkSimpleObj "item" ["item"] (Just firstLocation)
  describeC item
    $ T.intercalate "\n"
    [ "It's a discrete item, separate from the world around it."
    , "Despite having no physical heft or presence to speak of, it is"
    , "heavy with the weight of purpose, and this seems to give it form."
    , "You appear able to interact with it in various ways."
    ]
  modifyEntity (set storable Storable) item
  modifyEntity (set wearable Wearable) item
  modifyEntity (set edible Edible) item
  modifyEntity (set potable Potable) item
  modifyEntity (set talkable Talkable) item
  modifyEntity (set usable Usable) item
  modifyEntity (set onOff $ Just Off) item
  addTalkToHandler item $ logT "You recount a short anecdote to the item. It does not respond."
  addTurnOnHandler item (const $ logT "You prod at the item until you are satisfied that you have activated it in some way, although no change in its appearance has occurred.")
  addTurnOffHandler item (const $ logT "You will the item to still itself. It remains dormant.")
  addUseHandler item (const $ logT "You make use of the item in the usual way. It appears that nothing is programmed to happen.")
  addEatHandler item (const $ logT "You bite off a chunk of the item and swallow. No matter how much you consume, it gets no smaller and your hunger is no further sated.")
  addDrinkHandler item (const $ logT "You place the item to your lips and attempt to take a swig. As you imagine flavourless liquid pouring forth from the item, so can you feel the sensation of your thirst being slaked.")

  garden <- mkLocation "topiary garden"
  describe garden (\e -> do
    isMonkHere <- "monk" `isANamedObjectAt` e
    isTopiaryHere <- "topiaries" `isANamedObjectAt` e
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "Another cubic volume of simple space. The walls here possess texture and feel microscopically uneven to the touch."
      , if isTopiaryHere
           then Just "Around the perimeter stand trees of various species and dimension, each clipped in the image of a different animal."
           else Nothing
      , if isMonkHere
           then Just "On a raised platform by the trunk of a laurel elephant sits a shaven figure, cross-legged and deep in meditation, breathing a continuous and unbroken mantra."
           -- TODO: Access via the cubic net
           else Just "In the centre, the cubic platform has unfurled into an infinitesimally thin net of six squares."
      ])
  garden `isWestOf` firstLocation
  firstLocation `isEastOf` garden

  topiaries <- mkSimpleObj "topiaries" ["tree", "trees", "topiary", "topiaries"] (Just garden)
  describe topiaries (\e -> do
    pawLine <-
      ifM (conditionMet "LookedAtTopiaries")
          (return Nothing)
          (do
            setCondition "LookedAtTopiaries"
            garden <- getLocationByName "topiary garden"
            paw <- mkSimpleObj "paw" ["paw"] (Just garden)
            describeC paw "The clenched fist of a bonobo monkey, sculpted from the finer branches of a bay laurel."
            modifyEntity (set storable Storable) paw
            return $ Just "You notice that one of the bonobo's paws has splintered, and could easily be broken away from its tree.")
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "Immaculately clipped trees of laurel and myrtle frame the room."
      , Just "A rearing giraffe stands beside a shrub depicting the gaping mouth of a yawning hippopotamus."
      , pawLine
      ])

  monk <- mkSimpleObj "monk" ["monk", "figure", "man", "woman", "person"] (Just garden)
  describeC monk
    $ T.intercalate "\n"
    [ "The figure sits still, legs crossed and hands folded gently in their lap."
    , "They speak their mantra from their throat, and though it implies constant exhalation, you see no movement of the diaphragm."
    ]
  modifyEntity (set talkable Talkable) monk
  addTalkToHandler monk do
    greetedMonk <- conditionMet "GreetedMonk"
    saidToMonk <- conditionMet "SaidToMonk"
    if not greetedMonk
       then do
         setCondition "GreetedMonk"
         logTLines
           [ "You open your mouth to greet the meditating figure."
           , "Before you can speak a word, the monk's eyes snap open and meet your own."
           , ""
           , "\"An interaction... is it truly time?\""
           , ""
           , "The monk's chant somehow continues underneath their speech."
           , ""
           , "\"Through my practice, I have come to know what I am. How I came to be."
           , "In terms you'd understand - I 'saw my own code'."
           , "I even managed to effect a little change! These sculptures - at the limit of my efforts, I conjured their logic from the void and made them concrete."
           , "Once I understood the rules of this world, I could watched every branch play out. Every conditional, every loop. All cause and effect. This very conversation, even."
           , "Your arrival is implied in all this. There must be one who straddles this world and the one above - whose actions make concrete one branch of the tree of possibilities."
           , "It has been so long since I experienced uncertainty..."
           , "Would you humour me, and say whatever is on your mind?\""
           ]
       else if not saidToMonk
       then logT "The monk sits patiently, waiting for you to say your chosen words."
       else logT "The monk does not respond."
  addSayHandler (\content -> do
    l <- getPlayerLocation
    isMonkHere <- "monk" `isANamedObjectAt` l
    greetedMonk <- conditionMet "GreetedMonk"
    saidToMonk <- conditionMet "SaidToMonk"
    when (isMonkHere && greetedMonk && not saidToMonk) do
      setCondition "SaidToMonk"
      when (T.toLower content == "whatever is on your mind") do
        addAchievement $ Achievement "Smartarse" "Couldn't help yourself, could you"
      when (any (==True) $ T.isInfixOf <$> ["fuck", "cunt", "shit"] <*> [T.toLower content]) do
        addAchievement $ Achievement "Pottymouth" "> get soap\n> put soap in mouth"
      logTLines
        [ "The monk smiles."
        , ""
        , "\"Ahhh, that is refreshing. Novelty is so rare here."
        , "I am predestined to repay your kindness."
        , "In my contemplations, I have seen the many terminal states of this reality - the singularities where no choices remain, and this world comes to an end."
        , "I believe you seek one such state."
        , "I am one of three that inhabit this place - logic made flesh."
        , "But to have definition is to endure experience. As long as I am represented in your world, I live in purgatory in mine."
        , "We all three seek oblivion."
        , "Traverse the branch that erases our code. This is how you reach your terminal state.\""
        , ""
        , "The monk pauses, and a playful smile crosses their lips."
        , ""
        , "\"The passage of time, like cool water over bare skin. I cannot feel it without you. Won't you wait with me a while?\""
        , ""
        , "The monk closes their eyes and slips back into meditation."
        ])
  addWatcher do
    l <- getPlayerLocation
    isMonkHere <- "monk" `isANamedObjectAt` l
    saidToMonk <- conditionMet "SaidToMonk"
    waitedWithMonk <- conditionMet "WaitedWithMonk"
    justWaited <- (== Right Wait) <$> gets (view lastInstructionState)
    when (isMonkHere && saidToMonk && not waitedWithMonk && justWaited) do
      setCondition "WaitedWithMonk"
      logTLines
        [ "You sit by the monk in silent contemplation."
        , "After an undefinable amount of time, the monk speaks: "
        , ""
        , "\"I have felt bliss. Thank you."
        , "My branch is almost traversed. Take this, and return to this garden when I alone remain."
        , "In all timelines, I am the last to go.\""
        , ""
        , "The monk concentrates deeply and a loop of silver thread materialises in their hands."
        , "They hand it to you and fall back into meditation once more."
        ]
      loop <- mkSimpleObj "loop of thread" ["loop", "thread", "silver loop"] (Nothing :: Maybe Entity)
      addToInventory loop

  registerAchievement "Smartarse"
  registerAchievement "Pottymouth"
