{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidGame where

import Game
import Control.Monad.State
import Data.Text
import qualified Data.Map.Strict as M
import Control.Lens

bedroomDesc :: Description
bedroomDesc st eID = evalState go st
  where
    go = do
      e <- getEntity eID
      return $ e^.?name

buildCovidGame :: (MonadState GameState m) => m ()
buildCovidGame = do
  bedroom <- mkLocation "the Bedroom"
  addDesc (bedroom^.?entityID) bedroomDesc

  player <- mkPlayer "Player" $ bedroom^.?entityID
  return ()
