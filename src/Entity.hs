{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Entity where

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

-- Non-thread-safe way to get a new entity ID.
newID :: EntityType -> App EntityID
newID et = do
  es <- getAllEntities et
  return $ EntityID et (fromIntegral (length es) + 1)

-- Write an entity back to the register.
registerEntity :: Entity -> App ()
registerEntity e = do
  es <- gets (view entities)
  modify $ \s -> s & entities %~ M.insert (e^.?entityID) e

mkSimpleObj :: Name -> [Target] -> Maybe EntityID -> App Entity
mkSimpleObj name targets locationID = do
  objID <- newID SimpleObj
  let obj = def { _entityID=Just objID
                , _name=Just name
                , _locationID=locationID
                , _targets=Just $ S.fromList targets
                }
  registerEntity obj
  return obj

mkPlayer :: Name -> EntityID -> App Entity
mkPlayer name locationID = do
  playerID <- newID Player
  let player = def { _entityID=Just playerID
                   , _name=Just name
                   , _locationID=Just locationID
                   , _inventory=Just S.empty
                   , _wearing=Just S.empty
                   , _targets=Just $ S.fromList ["me", "self", "myself", "i", "player", "yourself"]
                   }
  registerEntity player
  return player

mkLocation :: Name -> App Entity
mkLocation name = do
  locationID <- newID Location
  let location = def { _entityID=Just locationID
                     , _name=Just name
                     , _visited=Just False
                     }
  registerEntity location
  return location

-- Modify the given entity persisted in the state.
modifyEntity :: (Entity -> Entity) -> EntityID -> App ()
modifyEntity f eID = do
  es <- gets (view entities)
  case M.lookup eID es of
    Just e -> modify $ \s -> s & entities %~ M.insert eID (f e)
    Nothing -> return ()

modifyPlayer :: (Entity -> Entity) -> App ()
modifyPlayer f = do
  p <- getPlayer
  modifyEntity f (p^.?entityID)

getAllEntities :: EntityType -> App [Entity]
getAllEntities et = do
  es <- gets (view entities)
  return [snd e | e <- M.toList es, entityType (snd e) == et]

-- Unsafely get the entity with the given ID
getEntity :: EntityID -> App Entity
getEntity eID = do
  es <- gets (view entities)
  let (Just l) = M.lookup eID es
  return l

getEntities :: [EntityID] -> App [Entity]
getEntities = traverse getEntity

getLocationByName :: Name -> App Entity
getLocationByName n = do
  es <- getAllEntities Location
  return $ head [e | e <- es, e^.name == Just n]

getEntityByName :: EntityType -> Name -> App (Maybe Entity)
getEntityByName et n = do
  es <- getAllEntities et
  let matches = [e | e <- es, e^.name == Just n]
  case matches of
    [] -> return Nothing
    [e] -> return $ Just e
    _ -> error $ "More than one entity named " ++ T.unpack n

-- Unsafe version of the above
getOneEntityByName :: EntityType -> Name -> App Entity
getOneEntityByName et n = do
  eM <- getEntityByName et n
  case eM of
    Just e -> return e
    Nothing -> error $ "getOneEntityByName error: cant find " <> T.unpack n

getOnlyEntity :: EntityType -> App Entity
getOnlyEntity et = do
  es <- getAllEntities et
  case es of
    [] -> error "No single entity"
    [e] -> return e
    _ -> error "More than one entity"

-- Get all entities at a given location.
-- Should usually not include the player itself
-- Also recursively gets all entities inside all other things...
getEntitiesAt :: EntityID -> App [Entity]
getEntitiesAt lID = do
  es <- gets (view entities)
  let esHere = [e | e <- snd <$> M.toList es, (e^.locationID) == Just lID]
      eIDs = (^.?entityID) <$> esHere
  hiddenEs <- traverse getEntitiesAt eIDs
  return $ esHere ++ concat hiddenEs

-- Deletes an entity.
removeEntity :: EntityID -> App ()
removeEntity eID = do
  modifyPlayer $ over wearing (fmap $ S.delete eID)
  removeFromInventory eID
  modify $ \s -> s & entities %~ M.delete eID

-- Get all entities at the player's location, including contained things.
getEntitiesNearPlayer :: App [Entity]
getEntitiesNearPlayer = do
  p <- getPlayer
  getEntitiesAt (p^.?locationID)

filterByTarget :: Target -> [Entity] -> [Entity]
filterByTarget t = filter (\e -> t `S.member` (e^.?targets))

-- All non-held items near the player
getTargetedEntitiesNearPlayer :: Target -> App [Entity]
getTargetedEntitiesNearPlayer t = filterByTarget t <$> getEntitiesNearPlayer

-- All items either near player or in inventory matching the given target
allValidTargetedEntities :: Target -> App [Entity]
allValidTargetedEntities t = do
  es1 <- getTargetedEntitiesNearPlayer t
  es2 <- filterInventoryByTarget t
  return $ es1 ++ es2

-- Gets a single arbitrary match to the given target.
-- Nothing if it doesn't match or can't be found.
oneValidTargetedEntity :: Target -> App (Maybe Entity)
oneValidTargetedEntity t = do
  es <- allValidTargetedEntities t
  return $ SL.head es

-- As above but only over inventory
oneInventoryTargetedEntity :: Target -> App (Maybe Entity)
oneInventoryTargetedEntity t = do
  es <- filterByTarget t <$> getInventoryEntities
  return $ SL.head es

getInventoryEntities :: App [Entity]
getInventoryEntities = do
  p <- getPlayer
  traverse getEntity (S.toList $ p^.?inventory)

inPlayerInventory :: EntityID -> App Bool
inPlayerInventory eID = do
  p <- getPlayer
  return $ eID `S.member` (p^.?inventory)

moveToInventory :: EntityID -> App ()
moveToInventory eID = do
  modifyEntity (set locationID Nothing) eID
  addToInventory eID

addToInventory :: EntityID -> App ()
addToInventory eID = modifyPlayer (over inventory $ fmap (S.insert eID))

removeFromInventory :: EntityID -> App ()
removeFromInventory eID = modifyPlayer (over inventory $ fmap (S.delete eID))

moveFromInventory :: EntityID -> EntityID -> App ()
moveFromInventory eID lID = do
  removeFromInventory eID
  modifyEntity (set locationID $ Just lID) eID

getPlayerWornEntities :: App [Entity]
getPlayerWornEntities = do
  p <- getPlayer
  traverse getEntity (S.toList $ p^.?wearing)

filterInventoryByTarget :: Target -> App [Entity]
filterInventoryByTarget t = filterByTarget t <$> getInventoryEntities

-- Get the single player entity
getPlayer :: App Entity
getPlayer = do
  es <- getAllEntities Player
  case es of
    [] -> error "No Player defined"
    [e] -> return e
    _ -> error "Multiple Players defined"

-- Get the location of the player
getPlayerLocation :: App Entity
getPlayerLocation = do
  p <- getPlayer
  getEntity $ p^.?locationID

-- Gets the compiled description for the given entity
getDescription :: EntityID -> App Text
getDescription eID = do
  ds <- gets (view descriptions)
  let d = fromMaybe (error $ "No desc for " ++ show eID) $ M.lookup eID ds
  d eID

