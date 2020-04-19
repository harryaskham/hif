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
import Data.Maybe

{-
   TODO:
   - Wearing whatever you're wearing right now
   - Meditating NPC storyline
   - Ability to inspect the code of individuals, quine-style
   - 'Who am I'
   - 'Who is X'
   - 'What is X'
   - Passage of time induction storyline
   - Dialogue system
   - Conditionals, loops, recursion
   - Limited agency - not God but limited by verbs all the same
   
   - Web frontend
   - Save/Load
   - IO cleanup
   - Reword engine messaging / config
-}

buildFourthGame = do

  mainMenu <- mkLocation "Main Menu"
  describeC mainMenu
    $ T.intercalate "\n"
    [ "You find yourself at the title screen of the game you have chosen to play."
    , "Typing 'help' will let you know how to continue from here."
    , "As you acclimatise, the dim matte white of the walls around you becomes clearer."
    , "Somehow, you are also standing in a small, empty chamber with a single, simple opening."
    ]

  firstLocation <- mkLocation "first location"
  describe firstLocation (\e -> do
    -- TODO: Humming from the west if the monk is still there
    itemHere <- "item" `isANamedObjectAt` e
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "This is a chamber much like the last. Smooth platonic surfaces meet to form the interior of a cube."
      , Just "A plain staircase leads up through a hole in the ceiling."
      , cT itemHere "A game item is propped up against the side of the stairs."
      ])
  firstLocation `isNorthOf` mainMenu

  item <- mkSimpleObj "item" ["item"] (Just firstLocation)



  p <- mkPlayer "yourself" mainMenu
  -- TODO: Include clothing status
  describeC p
    $ T.intercalate "\n"
    [ "You are yourself. You focus on this truth as you move about the space around you."
    , "Elsewhere, you sit at a terminal, and the utterances you issue effect change in the world."
    , "Twice you inhabit space and twice you have position."
    , "As you inspect yourself, you notice that you are inspecting yourself, and you stop abruptly."
    ]

  return ()
