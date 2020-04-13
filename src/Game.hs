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
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import TextShow

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

-- Unsafely lens into a Maybe
(^.?) :: s -> Getting (Maybe a) s (Maybe a) -> a
a ^.? b = fromMaybe (error "Unsafe entity attribute access") (a ^. b)

data EntityType = Player
                | Human
                | Location
                | Rock
                deriving (Eq, Show, Ord)

-- A unique ID for each type of entity
data EntityID = EntityID EntityType Integer deriving (Eq, Show, Ord)

type Name = Text
type Target = Text
type Inventory = Set EntityID

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

data Entity = Entity { _entityID :: Maybe EntityID
                     , _name :: Maybe Name
                     , _targets :: Maybe (Set Target)
                     , _storable :: StorableState
                     , _droppable :: DroppableState
                     , _usable :: UsableState
                     , _edible :: EdibleState
                     , _potable :: PotableState
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
               , _targets=Nothing
               , _storable=Unstorable
               , _droppable=Droppable
               , _usable=Unusable
               , _edible=Inedible
               , _potable=Unpotable
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
                           , _descriptions :: Map EntityID (GameState -> EntityID -> Text)
                           , _clock :: Integer
                           }
makeLenses ''GameState

-- TODO: Somehow move back to monadig
-- TODO: Figure out how to remove duplication with the above circular reference
type Description = GameState -> EntityID -> Text

type Speech = Text

data Direction = DirNorth
               | DirEast
               | DirSouth
               | DirWest
               | DirUp
               | DirDown

instance TextShow Direction where
  showb DirNorth = "North"
  showb DirEast = "East"
  showb DirSouth = "South"
  showb DirWest = "West"
  showb DirUp = "upwards"
  showb DirDown = "downwards"

data Instruction = Go Direction
                 | Wait
                 | Use Target
                 | Get Target
                 | Drop Target
                 | Eat Target
                 | Drink Target
                 | Say Speech 
                 | SayTo Target Speech
                 | Give Target Target
                 | Take Target Target
                 | Look
                 | LookAt Target
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

getEntities :: (MonadState GameState m) => [EntityID] -> m [Entity]
getEntities = traverse getEntity

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
                        , _descriptions=M.empty
                        , _clock=0
                        }

mkPlayer :: (MonadState GameState m) => Name -> EntityID -> m Entity
mkPlayer name locationID = do
  playerID <- newID Player
  let player = def { _entityID=Just playerID
                   , _name=Just name
                   , _locationID=Just locationID
                   , _inventory=Just S.empty
                   }
  registerEntity player
  return player

mkHuman :: (MonadState GameState m) => Name -> EntityID -> m Entity
mkHuman name locationID = do
  humanID <- newID Human
  let human = def { _entityID=Just humanID
                  , _name=Just name
                  , _locationID=Just locationID
                  }
  registerEntity human
  return human

mkLocation :: (MonadState GameState m) => Name -> m Entity
mkLocation name = do
  locationID <- newID Location
  let location = def { _entityID=Just locationID
                     , _name=Just name
                     }
  registerEntity location
  return location

mkRock :: (MonadState GameState m) => EntityID -> m Entity
mkRock locationID = do
  rockID <- newID Rock
  let rock = def { _entityID=Just rockID
                 , _name=Just "a rock"
                 , _locationID=Just locationID
                 , _targets=Just $ S.fromList ["rock", "stone"]
                 , _storable=Storable
                 }
  registerEntity rock
  return rock

-- Write an entity back to the register.
registerEntity :: (MonadState GameState m) => Entity -> m ()
registerEntity e = do
  es <- gets (view entities)
  modify $ \s -> s & entities %~ M.insert (e^.?entityID) e

-- Modify the given entity persisted in the state.
modifyEntity :: (MonadState GameState m) => (Entity -> Entity) -> EntityID -> m ()
modifyEntity f eID = do
  es <- gets (view entities)
  case M.lookup eID es of
    Just e -> modify $ \s -> s & entities %~ M.insert eID (f e)
    Nothing -> return ()

-- Register the given description function with the entity
addDesc :: (MonadState GameState m) => EntityID -> Description -> m ()
addDesc eID d = modify $ \s -> s & descriptions %~ M.insert eID d

buildSimpleGame :: (MonadState GameState m) => m ()
buildSimpleGame = do
  southRoom <- mkLocation "South Room" 
  let southRoomDesc st eID = "This is " <> (e^.?name) <> " at time " <> showt (st^.clock)
        where
          -- TODO: This is horrible, need to figure out how to have descriptions be monadic
          (Just e) = M.lookup eID $ st^.entities
  addDesc (southRoom^.?entityID) southRoomDesc

  player <- mkPlayer "Player" (southRoom^.?entityID)
  rock <- mkRock (southRoom^.?entityID)

  northRoom <- mkLocation "North Room"
  addDesc (northRoom^.?entityID) (\_ _ -> "This is the northernmost room")
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

-- Get the location of the player
getPlayerLocation :: (MonadState GameState m) => m Entity
getPlayerLocation = do
  p <- getPlayer
  getEntity $ p^.?locationID

-- Gets the compiled description for the given entity
getDescription :: (MonadState GameState m) => EntityID -> m Text
getDescription eID = do
  st <- get
  ds <- gets (view descriptions)
  let (Just d) = M.lookup eID ds
  return $ d st eID

-- TODO: Smarter location descriptions that build the things into the text.
-- Description should be a function that builds text, rather than just text.
-- So should name. These can be consts for now.
describeCurrentTurn :: (MonadState GameState m) => m Text
describeCurrentTurn = do
  st <- get
  p <- getPlayer
  l <- getEntity $ p^.?locationID
  es <- getEntitiesAt (l^.?entityID)
  clock <- gets (view clock)
  lDesc <- getDescription $ l^.?entityID
  let clockrow = Just $ "The time is " <> showt clock
      header = Just $ "You are at " <> l^.?name
      desc = Just lDesc
      thingsHere =
        case length es of
          0 -> Nothing
          _ -> Just $ "You can see: " <> T.intercalate ", " ((^.?name) <$> es)
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

  return $ T.intercalate "\n" (catMaybes [clockrow, header, desc, thingsHere] ++ directions)

-- Handle input, potentially running an instruction and modifying game state.
runInstruction :: (MonadState GameState m, MonadIO m) => Text -> m ()
runInstruction instructionText =
  case parseInstruction instructionText of
    (Just i) -> enactInstruction i
    Nothing -> do
      liftIO $ TIO.putStrLn "Invalid instruction"
      return ()

parseGo :: Parser Instruction
parseGo =
  try parseNorth
  <|> try parseSouth
  <|> try parseEast
  <|> try parseWest
  <|> try parseUp
  <|> try parseDown

anyString strs = foldl1 (<|>) ((\s -> try (string s >> eof)) <$> strs)

parseNorth :: Parser Instruction
parseNorth = do
  anyString ["n", "north"]
  return $ Go DirNorth

parseSouth :: Parser Instruction
parseSouth = do
  anyString ["s", "south"]
  return $ Go DirSouth

parseEast :: Parser Instruction
parseEast = do
  anyString ["e", "east"]
  return $ Go DirEast

parseWest :: Parser Instruction
parseWest = do
  anyString ["w", "west"]
  return $ Go DirWest

parseUp :: Parser Instruction
parseUp = do
  anyString ["u", "up"]
  return $ Go DirUp

parseDown :: Parser Instruction
parseDown = do
  anyString ["d", "down"]
  return $ Go DirDown

parseInventory :: Parser Instruction
parseInventory = do
  anyString ["i", "inv", "inventory"]
  return Inventory

parseGet :: Parser Instruction
parseGet = do
  string "get" <|> string "pick up"
  spaces
  target <- many1 anyChar
  eof
  return $ Get (T.pack target)

parseDrop :: Parser Instruction
parseDrop = do
  string "drop" <|> string "put down"
  spaces
  target <- many1 anyChar
  eof
  return $ Drop (T.pack target)

parseWait :: Parser Instruction
parseWait = do
  string "wait" <|> string "do nothing"
  eof
  return Wait

instructionParser :: Parser Instruction
instructionParser =
  try parseGo
  <|> try parseInventory
  <|> try parseGet
  <|> try parseDrop
  <|> try parseWait

-- Parse out the instruction from the given text string
parseInstruction :: Text -> Maybe Instruction
parseInstruction iText =
  case parse instructionParser "" (T.unpack iText) of
    Left e -> Nothing
    Right i -> Just i

-- Run f to modify the player.
modifyPlayer f = do
  p <- getPlayer
  modifyEntity f (p^.?entityID)

lensForDir DirNorth = toNorth
lensForDir DirEast = toEast
lensForDir DirSouth = toSouth
lensForDir DirWest = toWest
lensForDir DirUp = toUp
lensForDir DirDown = toDown

incrementClock :: MonadState GameState m => m ()
incrementClock = modify $ over clock (+1)

getEntitiesNearPlayer :: (MonadState GameState m) => m [Entity]
getEntitiesNearPlayer = do
  p <- getPlayer
  getEntitiesAt (p^.?locationID)

filterByTarget :: Target -> [Entity] -> [Entity]
filterByTarget t = filter (\e -> t `S.member` (e^.?targets))

getTargetedEntitiesNearPlayer :: (MonadState GameState m) => Target -> m [Entity]
getTargetedEntitiesNearPlayer t = filterByTarget t <$> getEntitiesNearPlayer

getInventoryEntities :: (MonadState GameState m) => m [Entity]
getInventoryEntities = do
  p <- getPlayer
  traverse getEntity (S.toList $ p^.?inventory)

filterInventoryByTarget :: (MonadState GameState m) => Target -> m [Entity]
filterInventoryByTarget t = filterByTarget t <$> getInventoryEntities

enactInstruction :: (MonadState GameState m, MonadIO m) => Instruction -> m ()
enactInstruction (Go dir) = do
  l <- getPlayerLocation
  case l^.lensForDir dir of
    Just lID -> do
      modifyPlayer (set locationID (Just lID))
      incrementClock
    Nothing -> liftIO $ TIO.putStrLn $ "Cannot travel " <> showt dir <> "."

enactInstruction (Get target) = do
  p <- getPlayer
  es <- getTargetedEntitiesNearPlayer target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "No " <> target <> " to get."
    es -> do
      let e = head es
      if (e^.?locationID) == (p^.?locationID) && e^.storable == Storable
         then do
           modifyEntity (set locationID Nothing) (e^.?entityID)
           modifyPlayer (over inventory (fmap (S.insert $ e^.?entityID)))
           incrementClock
           liftIO $ TIO.putStrLn $ "You get the " <> target
         else
           liftIO $ TIO.putStrLn $ "Cannot get " <> (e^.?name)

enactInstruction (Drop target) = do
  l <- getPlayerLocation
  es <- filterInventoryByTarget target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "No " <> target <> " to drop."
    es -> do
      let e = head es
      if e^.droppable == Droppable
         then do
           modifyEntity (set locationID (l^.entityID)) (e^.?entityID)
           modifyPlayer (over inventory (fmap (S.delete $ e^.?entityID)))
           incrementClock
           liftIO $ TIO.putStrLn $ "You drop the " <> target
        else
          liftIO $ TIO.putStrLn $ e^.?name <> " cannot be dropped"

enactInstruction Inventory = do
  p <- getPlayer
  case S.size (p^.?inventory) of
    0 -> liftIO $ TIO.putStrLn "Your inventory is empty."
    _ -> do
      es <- getInventoryEntities
      liftIO $ TIO.putStrLn $ "You have: " <> T.intercalate "\n" ((^.?name) <$> es)

enactInstruction Wait = do
  incrementClock
  liftIO $ TIO.putStrLn "You wait idly."
