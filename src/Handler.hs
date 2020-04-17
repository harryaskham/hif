{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Handler where

import Tools
import EntityType
import GameState

import Control.Lens
import Control.Monad.State
import Data.Default
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import TextShow
import qualified Data.List.Safe as SL
import Control.Monad.Extra
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

-- Register the given description function with the entity
addDesc :: EntityID -> Description -> App ()
addDesc eID d = modify $ \s -> s & descriptions %~ M.insert eID d

-- Quick helper to avoid ID usage
desc :: Entity -> Description -> App ()
desc e = addDesc (e^.?entityID)

addWatcher :: Watcher -> App ()
addWatcher w = modify $ over watchers (w:)

runWatchers :: App ()
runWatchers = do
  ws <- gets (view watchers)
  go ws
    where
      go [] = return ()
      go (w:ws) = do
        _ <- w
        go ws

addTalkToHandler :: EntityID -> TalkToHandler -> App ()
addTalkToHandler eID h = modify $ over talkToHandlers (M.insert eID h)

addEatHandler :: EntityID -> EatHandler -> App ()
addEatHandler eID h = modify $ over eatHandlers (M.insert eID h)

addOpenHandler :: EntityID -> OpenHandler -> App ()
addOpenHandler eID h = modify $ over openHandlers (M.insert eID h)

addSayHandler :: SayHandler -> App ()
addSayHandler h = modify $ over sayHandlers (h:)

addTurnOnHandler :: EntityID -> TurnOnHandler -> App ()
addTurnOnHandler eID h = modify $ over turnOnHandlers (M.insert eID h)

addTurnOffHandler :: EntityID -> TurnOffHandler -> App ()
addTurnOffHandler eID h = modify $ over turnOffHandlers (M.insert eID h)

addCombinationHandler :: EntityID -> EntityID -> CombinationHandler -> App ()
addCombinationHandler eID1 eID2 h = do
  modify $ over combinationHandlers (M.insert (eID1, eID2) h)
  modify $ over combinationHandlers (M.insert (eID2, eID1) (flip h))
