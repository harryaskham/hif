{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.State
import Game
import CovidGame
import qualified Data.Text as T
import Control.Lens
import qualified Data.Text.IO as TIO
import Control.Monad

runApp :: App ()
runApp = do
  -- buildSimpleGame
  buildCovidGame
  loop
  where
    loop :: App ()
    loop = do
      -- Describe the place
      ct <- describeCurrentTurn
      liftIO $ TIO.putStrLn ct

      -- Mark this place as visited
      p <- getPlayer
      modifyEntity (set visited $ Just True) (p^.?locationID)

      -- Get input and run instruction
      liftIO $ TIO.putStr "> "
      instruction <- liftIO TIO.getLine
      runInstruction instruction

      -- Run any predicates and re-loop
      runWatchers
      loop

main :: IO ()
main = do
  _ <- runStateT runApp mkGameState
  return ()
