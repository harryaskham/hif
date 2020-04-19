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
   - Limited agency - not God but limited by verbs all the same
   - Increase seperation; become less like yourself. Desynchronise the player and the avatar
   - The monk wants to feel the passage of time again - wait for him
   - Monk reveals fate - release the 3 souls trapped here and you too can face oblivion
   - The monk has meditated the forest into existence - to prove it he wills it away and the code really does change
   - He knows because he has seen the code
   - conditions and condition-mets; set flags as you progress throuhg the game. one for waiting
   - Say anything - ah, uncertainty!
   
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
      [ Just "You enter another cubic volume of space. The walls possess texture and feel microscopically uneven to the touch."
      , if isTopiaryHere
           then Just "Around the perimeter stand trees of various species and dimension, each clipped in the image of a different animal."
           else Nothing
      , if isMonkHere
           then Just "On a raised platform by the laurel trunk of an elephant sits a shaven figure, cross-legged and deep in meditation, breathing a continuous and unbroken mantra."
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
  addTalkToHandler monk do
    greetedMonk <- conditionMet "GreetedMonk"
    saidToMonk <- conditionMet "SaidToMonk"
    if not greetedMonk
       then do
         setCondition "GreetedMonk"
         logT $ T.intercalate "\n"
           [ "You open your mouth to greet the meditating figure."
           , "Before you can speak a word, the monk's eyes snap open and meet your own."
           , ""
           , "\"An interaction... is it truly time?\""
           , ""
           , "The monk's chant somehow continues as they speak."
           , ""
           , "\"Through, I have come to know what I am. How I came to be."
           , "In terms you'd understand - I 'saw my own code'."
           , "Once I understood the rules of this world, I could watched every branch play out. Every conditional, every loop. All cause and effect. This very conversation, even."
           , "Your arrival is implied in all this. There must be one who straddles this world and the one above - whose actions make concrete one branch of the tree of possibilities."
           , "It has been so long since I experienced uncertainty..."
           , "Would you humour me, and say whatever is on your mind?"
           ]
       else if not saidToMonk
       then logT "The monk sits patiently, waiting for you to say your chosen words."
       else
         logT $ T.intercalate "\n"
         [ "TODO"
         ]

  return ()
