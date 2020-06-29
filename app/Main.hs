{-# LANGUAGE OverloadedStrings #-}

module Main where

import HIF.Tools
import HIF.EntityType
import HIF.GameState
import HIF.CovidGame
import HIF.FourthGame
import qualified HIF.ExampleGame as EG
import HIF.Engine
import HIF.Handler
import HIF.Entity
import HIF.Instruction

import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Text as T
import Control.Lens
import qualified Data.Text.IO as TIO
import Control.Monad
import System.IO

runApp :: App () -> App ()
runApp builder = do
  -- Construct game
  builder
 
  logT =<< describeCurrentTurn
  loop
  where
    loop = do
      flushLog
      -- End game if it's over
      isOver <- gets (view gameOver)
      if isOver
         then do
           flushLog
           _ <- liftIO getLine
           return ()
         else do
           -- Get input and run instruction. Store the old state for UNDO purposes.
           prevState <- get
           liftIO $ TIO.putStr "\n> "
           instruction <- liftIO TIO.getLine
           liftIO $ TIO.putStr "\n"
           runInstruction instruction
           loop

runWithBuilder :: App () -> IO ()
runWithBuilder builder = do
  hSetBuffering stdout NoBuffering
  _ <- runStateT (runApp builder) mkGameState
  return ()

main :: IO ()
main = runWithBuilder EG.buildGame
