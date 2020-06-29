{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module HIF.ExampleGame where

import HIF.Tools
import HIF.GameState
import HIF.EntityType
import HIF.Engine
import HIF.Handler
import HIF.Entity
import HIF.Instruction
import HIF.InstructionType

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Data.Maybe

buildGame :: App ()
buildGame = do
  -- Register the game builder, enabling rebuilding the world from scratch.
  registerGameBuilder buildGame

  -- Create a few locations.
  forest <- mkLocation "forest"
  clearing <- mkLocation "clearing"
  unknown <- mkLocation "unknown location"

  -- Link the locations geographically.
  -- We need to specify both directions, because we allow for non-reversible paths.
  forest `isSouthOf` clearing
  clearing `isNorthOf` forest

  -- Add a simple constant description to the forest and the through-the-portal place.
  describeC
    forest
    "A thick, overgrown forest. Little sunlight penetrates the canopy overhead."
  describeC
    unknown
    "The land beyond the portal is strange beyond description. Say 'end' to conclude the game."
 
  -- Create and describe the player.
  player <- mkPlayer "yourself" forest
  describeC player "You are a weary adventurer, wandering this forest."

  -- Create a simple, interactable object that appears in the clearing.
  -- It can be referred to by the alias 'chain'.
  -- By making it 'Storable' it can be picked up, and by 'Wearable' can be worn.
  necklace <- mkSimpleObj "necklace" ["necklace", "chain"] (Just clearing)
  describeC necklace "A simple golden necklace. It radiates mystery. You should try it on."
  modifyEntity (set storable Storable) necklace
  modifyEntity (set wearable Wearable) necklace

  -- Add a dynamic description to the clearing.
  -- Whenever the description is called upon, the lambda will be called
  -- with an up-to-date view of the 'clearing' entity.
  describe clearing $ const do
    wearingNecklace <- player `isWearing` necklace
    if wearingNecklace
      then return "A clearing in the forest. The necklace you found has caused a portal to open."
      else return "An unremarkable clearing in the otherwise dense forest."

  -- Add a watcher that opens and closes the portal based on the status of the necklace.
  -- All watchers trigger each turn.
  addWatcher do
    wearingNecklace <- player `isWearing` necklace
    if wearingNecklace
       then unknown `isNorthOf` clearing
       else modifyEntity (set toNorth Nothing) clearing

  -- 'Win' the game by following the instructions to say 'end'
  addSayHandler (\words -> when (words == "end") setGameOver)

  -- Finally, add an achievement for eating the necklace...
  registerAchievement "Fool's Gold"
  modifyEntity (set edible Edible) necklace
  addEatHandler necklace $ const do
    logT "You struggle, but manage to swallow the necklace whole."
    addAchievement $ Achievement "Fool's Gold" "You should fix up your diet..."
