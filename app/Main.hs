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
import System.IO

runApp :: App ()
runApp = do
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

      -- If we made it to the street, finish
      -- TODO: remove specificity for street
      l <- getPlayerLocation
      if l^.?name == "street"
         then  liftIO $ TIO.putStrLn "END OF STORY SO FAR"
         else do
           -- Get input and run instruction
           liftIO $ TIO.putStr "> "
           instruction <- liftIO TIO.getLine
           runInstruction instruction

           -- Run any predicates and re-loop
           runWatchers
           loop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  _ <- runStateT runApp mkGameState
  return ()
