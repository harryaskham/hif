{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tools
import EntityType
import GameState
import CovidGame
import FourthGame
import qualified ExampleGame as EG
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
  --buildCovidGame
  --buildFourthGame
  EG.buildGame
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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  _ <- runStateT runApp mkGameState
  return ()
