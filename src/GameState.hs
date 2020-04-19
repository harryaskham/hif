{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module GameState where

import Tools
import EntityType
import InstructionType

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

type AlertID = Text
type Alert = Text

type AchievementID = Text
data Achievement = Achievement AchievementID Text

type Condition = Text

type Stack st = StateT st IO
data GameState =
  GameState
    { _entities :: Map EntityID Entity
    , _descriptions :: Map EntityID (Entity -> Stack GameState Text)
    , _clock :: Integer
    , _alerts :: Map AlertID Alert
    , _watchers :: [Stack GameState ()]
    , _history :: [GameState]
    , _remainingAchievements :: Set AchievementID
    , _achievements :: Map AchievementID Achievement
    , _gameOver :: Bool
    , _talkToHandlers :: Map EntityID (Stack GameState ())
    , _sayHandlers :: [Text -> Stack GameState ()]
    , _useHandlers :: Map EntityID (Entity -> Stack GameState ())
    , _turnOnHandlers :: Map EntityID (Entity -> Stack GameState ())
    , _turnOffHandlers :: Map EntityID (Entity -> Stack GameState ())
    , _combinationHandlers :: Map (EntityID, EntityID) (Entity -> Entity -> Stack GameState ())
    , _eatHandlers :: Map EntityID (Entity -> Stack GameState ())
    , _drinkHandlers :: Map EntityID (Entity -> Stack GameState ())
    , _openHandlers :: Map EntityID (Entity -> Stack GameState ())
    , _outLines :: [Text]
    , _lastInstructionState :: Either InstructionError Instruction
    , _conditions :: Set Condition
    }
makeLenses ''GameState

mkGameState :: GameState
mkGameState = GameState { _entities=M.empty
                        , _descriptions=M.empty
                        , _clock=0
                        , _alerts=M.empty
                        , _watchers=[]
                        , _history=[]
                        , _remainingAchievements=S.empty
                        , _achievements=M.empty
                        , _talkToHandlers=M.empty
                        , _gameOver=False
                        , _sayHandlers=[]
                        , _useHandlers=M.empty
                        , _turnOnHandlers=M.empty
                        , _turnOffHandlers=M.empty
                        , _combinationHandlers=M.empty
                        , _eatHandlers=M.empty
                        , _drinkHandlers=M.empty
                        , _openHandlers=M.empty
                        , _outLines=[]
                        , _lastInstructionState=Left InstructionError  -- TODO: woops
                        , _conditions=S.empty
                        }

-- Instantiate MTL stack for game
type App = Stack GameState

-- Helpers for concrete workers
type Description = Entity -> App Text
type Watcher = App ()
type TalkToHandler = App ()
type SayHandler = Text -> App ()
type UseHandler = Entity -> App ()
type TurnOnHandler = Entity -> App ()
type TurnOffHandler = Entity -> App ()
type CombinationHandler = Entity -> Entity -> App ()
type EatHandler = Entity -> App ()
type DrinkHandler = Entity -> App ()
type OpenHandler = Entity -> App ()

-- Output text to the screen within the Monad stack
-- TODO: This would work better with Endo or RState
logT :: Text -> App ()
logT t = modify $ over outLines (t:)

logTLines :: [Text] -> App ()
logTLines = logT . T.intercalate "\n"

flushLog :: App ()
flushLog = do
  ls <- gets (view outLines)
  mapM_ (liftIO . TIO.putStrLn) (reverse ls)
  modify $ set outLines []
