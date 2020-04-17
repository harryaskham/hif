{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module EntityType where

import Tools

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

data EntityType = Player
                | Human
                | Location
                | SimpleObj
                deriving (Eq, Show, Ord)

data EntityID = EntityID EntityType Integer deriving (Eq, Show, Ord)

type Name = Text
type Target = Text
type Inventory = Set EntityID
type Wearing = Set EntityID

data StorableState = Storable
                   | Unstorable
                   deriving (Eq, Show)

data UsableState = Usable
                 | Unusable
                 deriving (Eq, Show)

data EdibleState = Edible
                 | Inedible
                 deriving (Eq, Show)

data PotableState = Potable
                  | Unpotable
                  deriving (Eq, Show)

data DroppableState = Droppable
                    | Undroppable
                    deriving (Eq, Show)

data OnOffState = On
                | Off
                deriving (Eq, Show)

data OpenClosedState = Open
                     | Closed
                     deriving (Eq, Show)

data WearableState = Wearable
                   | Unwearable
                   deriving (Eq, Show)

data TalkableState = Talkable
                   | Untalkable
                   deriving (Eq, Show)

data Entity = Entity { _entityID :: Maybe EntityID
                     , _name :: Maybe Name
                     , _targets :: Maybe (Set Target)
                     , _storable :: StorableState
                     , _droppable :: DroppableState
                     , _usable :: UsableState
                     , _edible :: EdibleState
                     , _potable :: PotableState
                     , _wearable :: WearableState
                     , _talkable :: TalkableState
                     , _onOff :: Maybe OnOffState
                     , _openClosed :: Maybe OpenClosedState
                     , _locationID :: Maybe EntityID
                     , _inventory :: Maybe Inventory
                     , _wearing :: Maybe Wearing
                     , _toNorth :: Maybe EntityID
                     , _toEast :: Maybe EntityID
                     , _toSouth :: Maybe EntityID
                     , _toWest :: Maybe EntityID
                     , _toUp :: Maybe EntityID
                     , _toDown :: Maybe EntityID
                     , _visited :: Maybe Bool
                     } deriving (Show)
makeLenses ''Entity

instance Default Entity where
  def = Entity { _entityID=Nothing
               , _name=Nothing
               , _targets=Nothing
               , _storable=Unstorable
               , _droppable=Droppable
               , _usable=Unusable
               , _edible=Inedible
               , _potable=Unpotable
               , _wearable=Unwearable
               , _talkable=Untalkable
               , _locationID=Nothing
               , _onOff=Nothing
               , _openClosed=Nothing
               , _inventory=Nothing
               , _wearing=Nothing
               , _toNorth=Nothing
               , _toEast=Nothing
               , _toSouth=Nothing
               , _toWest=Nothing
               , _toUp=Nothing
               , _toDown=Nothing
               , _visited=Nothing
               }

entityType :: Entity -> EntityType
entityType e = let (EntityID et _) = (e^.?entityID) in et
