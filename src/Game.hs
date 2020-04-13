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
(^.?) :: Show s => s -> Getting (Maybe a) s (Maybe a) -> a
a ^.? b = fromMaybe (error $ "Unsafe entity attribute access" ++ show a) (a ^. b)

data EntityType = Player
                | Human
                | Location
                | Rock
                | Radio
                | Alarm
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

data OnOffState = On
                | Off
                deriving (Eq, Show)

data Entity = Entity { _entityID :: Maybe EntityID
                     , _name :: Maybe Name
                     , _targets :: Maybe (Set Target)
                     , _storable :: StorableState
                     , _droppable :: DroppableState
                     , _usable :: UsableState
                     , _edible :: EdibleState
                     , _potable :: PotableState
                     , _onOff :: Maybe OnOffState
                     , _locationID :: Maybe EntityID
                     , _inventory :: Maybe Inventory
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
               , _locationID=Nothing
               , _onOff=Nothing
               , _inventory=Nothing
               , _toNorth=Nothing
               , _toEast=Nothing
               , _toSouth=Nothing
               , _toWest=Nothing
               , _toUp=Nothing
               , _toDown=Nothing
               , _visited=Nothing
               }

-- Get just the entity type
entityType :: Entity -> EntityType
entityType e = let (EntityID et _) = (e^.?entityID) in et

type AlertID = Text
type Alert = Text

data GameState = GameState { _entities :: Map EntityID Entity
                           , _descriptions :: Map EntityID (GameState -> EntityID -> Text)
                           , _clock :: Integer
                           , _alerts :: Map AlertID Alert
                           -- TODO: Better reconcile this to use 
                           , _watchers :: [GameState -> GameState]
                           }
makeLenses ''GameState

-- Overall stack for the App
type App = StateT GameState IO

-- TODO: Somehow move back to monadig
-- TODO: Figure out how to remove duplication with the above circular reference
type Description = GameState -> EntityID -> Text

-- TODO: Figure out as per above, should be monadic and avoid circular reference
type Watcher = (GameState -> GameState)

-- Add a watcher to the game checking for things. Run every turn.
addWatcher :: (MonadState GameState m) => Watcher -> m ()
addWatcher w = modify $ \s -> s & watchers %~ (w:)

-- Run all watchers
runWatchers :: (MonadState GameState m) => m ()
runWatchers = do
  ws <- gets (view watchers)
  modify $ foldl (.) id ws

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
                 | TurnOn Target
                 | TurnOff Target

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

getOnlyEntity :: (MonadState GameState m) => EntityType -> m Entity
getOnlyEntity et = do
  es <- getAllEntities et
  case es of
    [] -> error "No single entity"
    [e] -> return e
    _ -> error "More than one entity"

-- Get all entities at a given location.
-- Should usually not include the player itself
getEntitiesAt :: (MonadState GameState m) => EntityID -> m [Entity]
getEntitiesAt lID = do
  es <- gets (view entities)
  return [e | e <- snd <$> M.toList es, (e^.locationID) == Just lID]

-- Non-thread-safe way to get a new entity ID.
newID :: (MonadState GameState m) => EntityType -> m EntityID
newID et = do
  es <- getAllEntities et
  return $ EntityID et (fromIntegral (length es) + 1)

mkGameState :: GameState
mkGameState = GameState { _entities=M.empty
                        , _descriptions=M.empty
                        , _clock=0
                        , _alerts=M.empty
                        , _watchers=[]
                        }

mkPlayer :: (MonadState GameState m) => Name -> EntityID -> m Entity
mkPlayer name locationID = do
  playerID <- newID Player
  let player = def { _entityID=Just playerID
                   , _name=Just name
                   , _locationID=Just locationID
                   , _inventory=Just S.empty
                   , _targets=Just $ S.fromList ["me", "self", "myself", "i", "player", "yourself"]
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
                     , _visited=Just False
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

mkRadio :: (MonadState GameState m) => EntityID -> m Entity
mkRadio locationID = do
  radioID <- newID Radio
  let radio = def { _entityID=Just radioID
                  , _name=Just "a wall-mounted radio"
                  , _locationID=Just locationID
                  , _targets=Just $ S.fromList ["radio"]
                  , _onOff=Just Off
                  }
  registerEntity radio
  return radio

mkAlarm :: (MonadState GameState m) => EntityID -> m Entity
mkAlarm locationID = do
  alarmID <- newID Alarm
  let alarm = def { _entityID=Just alarmID
                  , _name=Just "an alarm clock"
                  , _locationID=Just locationID
                  , _targets=Just $ S.fromList ["alarm", "clock"]
                  , _storable=Storable
                  , _onOff=Just Off
                  }
  registerEntity alarm
  return alarm
  
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

-- Adds a given alert.
addAlert :: (MonadState GameState m) => AlertID -> Alert -> m ()
addAlert aID a = modify $ \s -> s & alerts %~ M.insert aID a

-- Removes the given alert.
removeAlert :: (MonadState GameState m) => AlertID -> m ()
removeAlert aID = modify $ \s -> s & alerts %~ M.delete aID

buildSimpleGame :: (MonadState GameState m) => m ()
buildSimpleGame = do
  addAlert "Alert1" "This is an alert for 4 turns"
  let watcher st = if st^.clock == 5 then st & alerts %~ M.delete "Alert1" else st
  addWatcher watcher

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
  es <- filter (\e -> entityType e /= Player) <$> getEntitiesAt (l^.?entityID)
  clock <- gets (view clock)
  lDesc <- getDescription $ l^.?entityID
  alertsMap <- gets (view alerts)
  let clockrow = Just $ "The time is " <> showt clock
      header = Just $ "\n" <> (T.toUpper $ (l^.?name)) <> "\n=========="
      desc = Just lDesc
      alerts = case snd <$> M.toList alertsMap of
                 [] -> Nothing
                 as -> Just $ T.intercalate "\n" as
      thingsHere =
        case length es of
          0 -> Nothing
          _ -> Just $ "\nYou can see: " <> (T.intercalate ", " ((^.?name) <$> es) <> "\n")
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

  -- TODO: Reinstate clock?
  return $ T.intercalate "\n" (catMaybes [header, desc, alerts, thingsHere] ++ directions)

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
  string "get" <|> string "pick up" <|> string "take"
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

parseLook :: Parser Instruction
parseLook = do
  anyString ["l", "look"]
  return Look

parseLookAt :: Parser Instruction
parseLookAt = do
  string "look at"
  spaces
  target <- many1 anyChar
  eof
  return $ LookAt (T.pack target)

parseTurnOn :: Parser Instruction
parseTurnOn = do
  string "turn on"
  spaces
  target <- many1 anyChar
  eof
  return $ TurnOn (T.pack target)

parseTurnOff :: Parser Instruction
parseTurnOff = do
  string "turn off"
  spaces
  target <- many1 anyChar
  eof
  return $ TurnOff (T.pack target)

instructionParser :: Parser Instruction
instructionParser =
  try parseGo
  <|> try parseInventory
  <|> try parseGet
  <|> try parseDrop
  <|> try parseWait
  <|> try parseLook
  <|> try parseLookAt
  <|> try parseTurnOn
  <|> try parseTurnOff

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

allValidTargetedEntities :: (MonadState GameState m) => Target -> m [Entity]
allValidTargetedEntities t = do
  es1 <- getTargetedEntitiesNearPlayer t
  es2 <- filterInventoryByTarget t
  return $ es1 ++ es2

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

enactInstruction (LookAt target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Can't see " <> target
    es -> do
      let e = head es
      d <- getDescription (e^.?entityID)
      liftIO $ TIO.putStrLn d

enactInstruction Look = liftIO $ TIO.putStrLn "You look around... some more?"

enactInstruction Inventory = do
  p <- getPlayer
  case S.size (p^.?inventory) of
    0 -> liftIO $ TIO.putStrLn "Your inventory is empty."
    _ -> do
      es <- getInventoryEntities
      liftIO $ TIO.putStrLn $ "You have: " <> T.intercalate ", " ((^.?name) <$> es)

enactInstruction Wait = do
  incrementClock
  liftIO $ TIO.putStrLn "You wait idly."

enactInstruction (TurnOn target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Don't know what " <> target <> " is"
    es -> do
      let e = head es
      turnOn (e^.?entityID)

enactInstruction (TurnOff target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Don't know what " <> target <> " is"
    es -> do
      let e = head es
      turnOff (e^.?entityID)

turnOn :: (MonadState GameState m, MonadIO m) => EntityID -> m ()
turnOn eID = do
  e <- getEntity eID
  if isNothing (e^.onOff)
     then liftIO $ TIO.putStrLn "Can't turn that on"
     else case entityType e of
       Alarm -> liftIO $ TIO.putStrLn "You can't turn an alarm on at will, man. Time only goes one way."
       Radio -> case e^.?onOff of
                  Off -> do
                    liftIO $ TIO.putStrLn "You twist the dial until the bad news starts to roll once more."
                    modifyEntity (set onOff $ Just On) (e^.?entityID)
                  On -> liftIO $ TIO.putStrLn "Already chirping away, friend."
       _ -> liftIO $ TIO.putStrLn "Nothing happens"

turnOff :: (MonadState GameState m, MonadIO m) => EntityID -> m ()
turnOff eID = do
  e <- getEntity eID
  if isNothing (e^.onOff)
     then liftIO $ TIO.putStrLn "Can't turn that off"
     else case entityType e of
       Alarm -> case e^.?onOff of
                  Off -> liftIO $ TIO.putStrLn "You already took care of that, chap."
                  On -> do
                    liftIO $ TIO.putStrLn "You slam a calloused hand onto the rusty metal bells, and the alarm is silenced."
                    modifyEntity (set onOff $ Just Off) (e^.?entityID)
       Radio -> case e^.?onOff of
                  Off -> liftIO $ TIO.putStrLn "The radio is already off."
                  On -> do
                    liftIO $ TIO.putStrLn "There's no off switch, but you dial your way to the most quiet static you can find."
                    modifyEntity (set onOff $ Just Off) (e^.?entityID)
       _ -> liftIO $ TIO.putStrLn "Nothing happens"
