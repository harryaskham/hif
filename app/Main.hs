{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tools
import EntityType
import GameState
import CovidGame
import Engine
import Handler
import Entity
import Instruction

import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Text as T
import Control.Lens
import qualified Data.Text.IO as TIO
import Control.Monad
import System.IO

runApp :: App ()
runApp = do
  buildCovidGame
  loop
  where
    loop = do
      -- Describe the place
      ct <- describeCurrentTurn
      logT ct

      -- Mark this place as visited
      p <- getPlayer
      modifyEntity (set visited $ Just True) (p^.?locationID)

      -- End game if it's over
      isOver <- gets (view gameOver)
      if isOver
         then do
           logT "END OF STORY SO FAR"
           _ <- liftIO getLine
           logT "CIAO"
         else instructionLoop

    instructionLoop = do
      -- Get input and run instruction. Store the old state for UNDO purposes.
      prevState <- get
      liftIO $ TIO.putStr "> "
      instruction <- liftIO TIO.getLine
      outcome <- runInstruction instruction
      case outcome of
        Left InstructionError -> do
          logT "Invalid instruction"
          instructionLoop
        Right i -> do
          -- Run any predicates, store historical state, and re-loop
          runWatchers
          unless (i == Undo) $ modify $ over history (prevState:)
          loop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  _ <- runStateT runApp mkGameState
  return ()
