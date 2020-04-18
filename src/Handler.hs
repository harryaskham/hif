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
import Entity

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
describe :: (HasID e) => e -> Description -> App ()
describe e d = modify $ \s -> s & descriptions %~ M.insert (getID e) d

-- As above but adds an unpredicated description.
describeC :: (HasID e) => e -> Text -> App ()
describeC e d = describe e (const . return $ d)

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

addTalkToHandler :: (HasID e) => e -> TalkToHandler -> App ()
addTalkToHandler e h = modify $ over talkToHandlers (M.insert (getID e) h)

addEatHandler :: (HasID e) => e -> EatHandler -> App ()
addEatHandler e h = modify $ over eatHandlers (M.insert (getID e) h)

addOpenHandler :: (HasID e) => e -> OpenHandler -> App ()
addOpenHandler e h = modify $ over openHandlers (M.insert (getID e) h)

addSayHandler :: SayHandler -> App ()
addSayHandler h = modify $ over sayHandlers (h:)

addTurnOnHandler :: (HasID e) => e -> TurnOnHandler -> App ()
addTurnOnHandler e h = modify $ over turnOnHandlers (M.insert (getID e) h)

addTurnOffHandler :: (HasID e) => e -> TurnOffHandler -> App ()
addTurnOffHandler e h = modify $ over turnOffHandlers (M.insert (getID e) h)

addCombinationHandler :: (HasID e) => e -> e -> CombinationHandler -> App ()
addCombinationHandler e1 e2 h = do
  let eID1 = getID e1
      eID2 = getID e2
  modify $ over combinationHandlers (M.insert (eID1, eID2) h)
  modify $ over combinationHandlers (M.insert (eID2, eID1) (flip h))
