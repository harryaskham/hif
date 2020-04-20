{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module InstructionType where

import Tools
import EntityType

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

data Direction = DirNorth
               | DirEast
               | DirSouth
               | DirWest
               | DirUp
               | DirDown
               deriving (Eq, Show)

instance TextShow Direction where
  showb DirNorth = "North"
  showb DirEast = "East"
  showb DirSouth = "South"
  showb DirWest = "West"
  showb DirUp = "upwards"
  showb DirDown = "downwards"

data Instruction = Go Direction
                 | Wait
                 | Get Target
                 | Drop Target
                 | Eat Target
                 | Drink Target
                 | TalkTo Target 
                 | Look
                 | LookAt Target
                 | Inventory
                 | Use Target
                 | TurnOn Target
                 | TurnOff Target
                 | Combine Target Target
                 | Give Target Target
                 | Wear Target
                 | Remove Target
                 | Help
                 | Undo
                 | OpenI Target
                 | Break Target
                 | Say Text
                 | Save
                 | Load Text
                 deriving (Eq, Show)

instance TextShow Instruction where
  showb = showb

data InstructionError = InstructionError deriving (Eq, Show)

instance TextShow InstructionError where
  showb = showb
