{- TODO: need to fix stack or convert to cabal and add this to the project proper -}
{-# LANGUAGE OverloadedStrings #-}

module HIF.Session where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text
import qualified Data.Text as T
import HIF.Engine
import HIF.GameState
import HIF.Instruction
import HIF.Runner

newtype SessionId = SessionId Text

data Session = Session
  { _sessionId :: SessionId,
    _app :: App (),
    _gameState :: GameState
  }

mkSessionId :: IO SessionId
mkSessionId = pure $ SessionId "todo replace me"

mkSession :: App () -> IO Session
mkSession builder = Session <$> mkSessionId <*> pure (runApp builder) <*> pure mkGameState

newtype SessionRunner = SessionRunner
  { _sessions :: Map SessionId Session
  }
