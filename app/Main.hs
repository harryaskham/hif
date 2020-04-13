{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Game
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad

type App = StateT GameState IO

runApp :: App ()
runApp = do
  buildSimpleGame
  loop
  where
    loop :: App ()
    loop = do
      ct <- describeCurrentTurn
      liftIO $ TIO.putStrLn ct
      liftIO $ TIO.putStr "> "
      instruction <- liftIO TIO.getLine
      runInstruction instruction
      runWatchers
      loop

main :: IO ()
main = do
  _ <- runStateT runApp mkGameState
  return ()
