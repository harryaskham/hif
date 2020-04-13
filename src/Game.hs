{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Control.Lens
import Control.Monad.State
import Data.Default
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import TextShow


-- Unsafely lens into a Maybe
(^.?) :: s -> Getting (Maybe a) s (Maybe a) -> a
a ^.? b = fromMaybe (error "Unsafe entity attribute access") (a ^. b)

data EntityType = Player
                | Human
                | Location
                | Rock
                deriving (Eq, Show, Ord)

-- A unique ID for each type of entity
data EntityID = PlayerID
              | EntityID EntityType Integer deriving (Eq, Show, Ord)

type Name = Text
type Description = Text
type Inventory = [Entity]

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

data Entity = Entity { _entityID :: Maybe EntityID
                     , _name :: Maybe Name
                     , _description :: Maybe Description
                     , _storable :: Maybe StorableState
                     , _usable :: Maybe UsableState
                     , _edible :: Maybe EdibleState
                     , _potable :: Maybe PotableState
                     , _locationID :: Maybe EntityID
                     , _inventory :: Maybe Inventory
                     , _toNorth :: Maybe EntityID
                     , _toEast :: Maybe EntityID
                     , _toSouth :: Maybe EntityID
                     , _toWest :: Maybe EntityID
                     , _toUp :: Maybe EntityID
                     , _toDown :: Maybe EntityID
                     } deriving (Show)
makeLenses ''Entity

instance Default Entity where
  def = Entity { _entityID=Nothing
               , _name=Nothing
               , _description=Nothing
               , _storable=Nothing
               , _usable=Nothing
               , _edible=Nothing
               , _potable=Nothing
               , _locationID=Nothing
               , _inventory=Nothing
               , _toNorth=Nothing
               , _toEast=Nothing
               , _toSouth=Nothing
               , _toWest=Nothing
               , _toUp=Nothing
               , _toDown=Nothing
               }

data GameState = GameState { _entities :: Map EntityID Entity
                           , _clock :: Integer
                           } deriving (Show)
makeLenses ''GameState

type Speech = Text

data Instruction e = GoNorth
                   | GoEast
                   | GoSouth
                   | GoWest
                   | GoUp
                   | GoDown
                   | Wait
                   | Use e
                   | Get e
                   | Drop e
                   | Eat e
                   | Drink e
                   | Say Speech 
                   | SayTo e Speech
                   | Give e e
                   | Take e e
                   | Look
                   | LookAt e
                   | Inventory

getAllEntities :: (MonadState GameState m) => EntityType -> m [Entity]
getAllEntities et = do
  es <- gets (view entities)
  return [snd e | e <- M.toList es, let (EntityID et' _) = fst e in et == et']

-- Unsafely get the entity with the given ID
getEntity :: (MonadState GameState m) => EntityID -> m Entity
getEntity eID = do
  es <- gets (view entities)
  let (Just l) = M.lookup eID es
  return l

-- Get all entities at a given location.
-- Should usually not include the player itself
getEntitiesAt :: (MonadState GameState m) => EntityID -> m [Entity]
getEntitiesAt lID = do
  es <- gets (view entities)
  return [e | e <- snd <$> M.toList es, (e^.locationID) == Just lID, let (EntityID et _) = e^.?entityID in et /= Player]

-- Non-thread-safe way to get a new entity ID.
newID :: (MonadState GameState m) => EntityType -> m EntityID
newID et = do
  es <- getAllEntities et
  return $ EntityID et (fromIntegral (length es) + 1)

mkGameState :: GameState
mkGameState = GameState { _entities=M.empty
                        , _clock=0
                        }

mkPlayer :: (MonadState GameState m) => Name -> EntityID -> Description -> m Entity
mkPlayer name locationID description = do
  playerID <- newID Player
  let player = def { _entityID=Just playerID
                   , _name=Just name
                   , _locationID=Just locationID
                   , _inventory=Just []
                   , _description=Just description
                   }
  registerEntity player
  return player

mkHuman :: (MonadState GameState m) => Name -> EntityID -> Description -> m Entity
mkHuman name locationID description = do
  humanID <- newID Human
  let human = def { _entityID=Just humanID
                  , _name=Just name
                  , _locationID=Just locationID
                  , _description=Just description
                  }
  registerEntity human
  return human

mkLocation :: (MonadState GameState m) => Name -> Description -> m Entity
mkLocation name description = do
  locationID <- newID Location
  let location = def { _entityID=Just locationID
                     , _name=Just name
                     , _description=Just description
                     }
  registerEntity location
  return location

mkRock :: (MonadState GameState m) => EntityID -> m Entity
mkRock locationID = do
  rockID <- newID Rock
  let rock = def { _entityID=Just rockID
                  , _name=Just "a rock"
                  , _locationID=Just locationID
                  , _description=Just "A big ol' rock"
                  }
  registerEntity rock
  return rock

-- Write an entity back to the register.
registerEntity :: (MonadState GameState m) => Entity -> m ()
registerEntity e = do
  es <- gets (view entities)
  modify $ \s -> s & entities %~ M.insert (e^.?entityID) e

-- Run a step of the game.
-- If the instruction is invalid, maybe don't step time?
-- Run the instruction, run any activity for the entities in the game,
-- update clock.
-- TODO: Maybe put this inside the monad
stepGame :: Maybe (Instruction Entity) -> GameState -> GameState
stepGame instruction game = undefined

-- Modify the given entity persisted in the state.
modifyEntity :: (MonadState GameState m) => (Entity -> Entity) -> EntityID -> m ()
modifyEntity f eID = do
  es <- gets (view entities)
  case M.lookup eID es of
    Just e -> modify $ \s -> s & entities %~ M.insert eID (f e)
    Nothing -> return ()

buildSimpleGame :: (MonadState GameState m) => m ()
buildSimpleGame = do
  southRoom <- mkLocation "South Room" "This is the southmost room."
  player <- mkPlayer "Player" (southRoom^.?entityID) "This is you."
  rock <- mkRock (southRoom^.?entityID)

  northRoom <- mkLocation "North Room" "This is the northmost room."
  modifyEntity (set toSouth (Just $ southRoom^.?entityID)) (northRoom^.?entityID)
  modifyEntity (set toNorth (Just $ northRoom^.?entityID)) (southRoom^.?entityID)
  
  return ()

-- Get the single player entity
getPlayer :: (MonadState GameState m) => m Entity
getPlayer = do
  es <- getAllEntities Player
  case es of
    [] -> error "No Player defined"
    [e] -> return e
    _ -> error "Multiple Players defined"

describeCurrentTurn :: (MonadState GameState m) => m Text
describeCurrentTurn = do
  p <- getPlayer
  l <- getEntity $ p^.?locationID
  es <- getEntitiesAt (l^.?entityID)
  clock <- gets (view clock)
  let clockrow = "The time is " <> showt clock
      header = "You are at " <> l^.?name
      desc = l^.?description
      thingsHere = "You can see: " <> T.intercalate ", " ((^.?name) <$> es)
  directions <-
    sequence 
    $ mapMaybe
      (\(dirLens, dirString) ->
        case l^.dirLens of
          Nothing -> Nothing
          Just lID -> Just $ do
            l <- getEntity lID
            return $ dirString <> " is " <> l^.?name)
      [ (toNorth, "To the North")
      , (toEast, "To the East")
      , (toSouth, "To the South")
      , (toWest, "To the West")
      , (toUp, "Above you")
      , (toDown, "Below you")
      ]

  return $ T.intercalate "\n" ([clockrow, header, desc, thingsHere] ++ directions)
