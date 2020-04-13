{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Game
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type App = StateT GameState IO

runApp :: App ()
runApp = do
  buildSimpleGame
  env <- get
  -- liftIO $ print env
  ct <- describeCurrentTurn
  liftIO $ TIO.putStrLn ct

main :: IO ()
main = do
  _ <- runStateT runApp mkGameState
  return ()
