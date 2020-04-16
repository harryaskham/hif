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

log :: (MonadIO m) => Text -> m ()
log = liftIO . TIO.putStrLn

data EntityType = Player
                | Human
                | Location
                | Rock
                | Radio
                | Alarm
                | SimpleObj
                | HairBand
                deriving (Eq, Show, Ord)

-- A unique ID for each type of entity
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

data Entity = Entity { _entityID :: Maybe EntityID
                     , _name :: Maybe Name
                     , _targets :: Maybe (Set Target)
                     , _storable :: StorableState
                     , _droppable :: DroppableState
                     , _usable :: UsableState
                     , _edible :: EdibleState
                     , _potable :: PotableState
                     , _wearable :: WearableState
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

-- Get just the entity type
entityType :: Entity -> EntityType
entityType e = let (EntityID et _) = (e^.?entityID) in et

type AlertID = Text
type Alert = Text

type AchievementID = Text
data Achievement = Achievement AchievementID Text

data GameState = GameState { _entities :: Map EntityID Entity
                           , _descriptions :: Map EntityID (GameState -> EntityID -> Text)
                           , _clock :: Integer
                           , _alerts :: Map AlertID Alert
                           , _watchers :: [StateT GameState IO ()]
                           , _history :: [GameState]
                           , _achievements :: Map AchievementID Achievement
                           , _gameOver :: Bool
                           }
makeLenses ''GameState

-- Overall stack for the App
type App = StateT GameState IO

-- TODO: Somehow move back to monadig
-- TODO: Figure out how to remove duplication with the above circular reference
type Description = GameState -> EntityID -> Text

-- TODO: Figure out as per above, should be monadic and avoid circular reference
type Watcher = App ()

-- Add a watcher to the game checking for things. Run every turn.
addWatcher :: (MonadState GameState m) => Watcher -> m ()
addWatcher w = modify $ \s -> s & watchers %~ (w:)

-- Run all watchers
runWatchers :: App ()
runWatchers = do
  ws <- gets (view watchers)
  go ws
    where
      go [] = return ()
      go (w:ws) = do
        _ <- w
        go ws

addAchievement :: (MonadState GameState m, MonadIO m) => Achievement -> m ()
addAchievement a@(Achievement aID aContent) = do
  liftIO $ TIO.putStrLn $ "\n***ACHIEVEMENT UNLOCKED***\n" <> aID <> "\n" <> aContent
  modify (\s -> s & achievements %~ M.insert aID a)

hasAchievement :: (MonadState GameState m) => AchievementID -> m Bool
hasAchievement aID = do
  as <- gets (view achievements)
  return $ M.member aID as

type Speech = Text

data Direction = DirNorth
               | DirEast
               | DirSouth
               | DirWest
               | DirUp
               | DirDown
               deriving (Eq)

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
                 | TalkTo Target 
                 | Look
                 | LookAt Target
                 | Inventory
                 | TurnOn Target
                 | TurnOff Target
                 | Combine Target Target
                 | Wear Target
                 | Remove Target
                 | Help
                 | Undo
                 | OpenI Target
                 | Say Text
                 deriving (Eq)

getAllEntities :: (MonadState GameState m) => EntityType -> m [Entity]
getAllEntities et = do
  es <- gets (view entities)
  return [snd e | e <- M.toList es, entityType (snd e) == et]

-- Unsafely get the entity with the given ID
getEntity :: (MonadState GameState m) => EntityID -> m Entity
getEntity eID = do
  es <- gets (view entities)
  let (Just l) = M.lookup eID es
  return l

getEntities :: (MonadState GameState m) => [EntityID] -> m [Entity]
getEntities = traverse getEntity

getLocationByName :: (MonadState GameState m) => Name -> m Entity
getLocationByName n = do
  es <- getAllEntities Location
  return $ head [e | e <- es, e^.name == Just n]

getEntityByName :: (MonadState GameState m) => EntityType -> Name -> m (Maybe Entity)
getEntityByName et n = do
  es <- getAllEntities et
  let matches = [e | e <- es, e^.name == Just n]
  case matches of
    [] -> return Nothing
    [e] -> return $ Just e
    _ -> error $ "More than one entity named " ++ T.unpack n

-- Unsafe version of the above
getOneEntityByName :: (MonadState GameState m) => EntityType -> Name -> m Entity
getOneEntityByName et n = do
  eM <- getEntityByName et n
  let (Just e) = eM
  return e

getOnlyEntity :: (MonadState GameState m) => EntityType -> m Entity
getOnlyEntity et = do
  es <- getAllEntities et
  case es of
    [] -> error "No single entity"
    [e] -> return e
    _ -> error "More than one entity"

-- Get all entities at a given location.
-- Should usually not include the player itself
-- Also recursively gets all entities inside all other things...
getEntitiesAt :: (MonadState GameState m) => EntityID -> m [Entity]
getEntitiesAt lID = do
  es <- gets (view entities)
  let esHere = [e | e <- snd <$> M.toList es, (e^.locationID) == Just lID]
      eIDs = (^.?entityID) <$> esHere
  hiddenEs <- traverse getEntitiesAt eIDs
  return $ esHere ++ concat hiddenEs

-- Deletes an entity.
removeEntity :: (MonadState GameState m) => EntityID -> m ()
removeEntity eID = modify $ \s -> s & entities %~ M.delete eID

setGameOver :: (MonadState GameState m) => m ()
setGameOver = modify $ set gameOver True

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
                        , _history=[]
                        , _achievements=M.empty
                        , _gameOver=False
                        }

mkSimpleObj :: (MonadState GameState m) => Name -> [Target] -> Maybe EntityID -> m Entity
mkSimpleObj name targets locationID = do
  objID <- newID SimpleObj
  let obj = def { _entityID=Just objID
                , _name=Just name
                , _locationID=locationID
                , _targets=Just $ S.fromList targets
                }
  registerEntity obj
  return obj

mkPlayer :: (MonadState GameState m) => Name -> EntityID -> m Entity
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

mkHuman :: (MonadState GameState m) => Name -> [Target] -> EntityID -> m Entity
mkHuman name targets locationID = do
  humanID <- newID Human
  let human = def { _entityID=Just humanID
                  , _name=Just name
                  , _targets=Just $ S.fromList targets
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
                  , _name=Just "alarm clock"
                  , _locationID=Just locationID
                  , _targets=Just $ S.fromList ["alarm", "clock", "alarm clock"]
                  , _storable=Storable
                  , _onOff=Just Off
                  }
  registerEntity alarm
  return alarm

mkHairband :: (MonadState GameState m) => EntityID -> m Entity
mkHairband locationID = do
  hairbandID <- newID HairBand
  let hairband = def { _entityID=Just hairbandID
                     , _name=Just "an elasticated hairband"
                     , _locationID=Just locationID
                     , _targets=Just $ S.fromList ["hairband", "band", "headband"]
                     , _storable=Storable
                     , _wearable=Wearable
                     }
  registerEntity hairband
  return hairband
  
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

-- Helpers to build out descriptions
buildDescription f st eID = evalState (f eID) st
desc e d = addDesc (e^.?entityID) (buildDescription d)

-- Adds a given alert.
addAlert :: (MonadState GameState m) => AlertID -> Alert -> m ()
addAlert aID a = modify $ \s -> s & alerts %~ M.insert aID a

-- Removes the given alert.
removeAlert :: (MonadState GameState m) => AlertID -> m ()
removeAlert aID = modify $ \s -> s & alerts %~ M.delete aID

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
      header = Just $ "\n" <> (T.toUpper (l^.?name)) <> "\n=========="
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
            return $ dirString <> " is the " <> l^.?name)
      [ (toNorth, "To the North")
      , (toEast, "To the East")
      , (toSouth, "To the South")
      , (toWest, "To the West")
      , (toUp, "Above you")
      , (toDown, "Below you")
      ]

  -- TODO: Reinstate clock / things here
  return $ T.intercalate "\n" (catMaybes [header, desc, alerts, thingsHere] ++ directions)

data InstructionError = InstructionError

-- Handle input, potentially running an instruction and modifying game state.
runInstruction :: (MonadState GameState m, MonadIO m) => Text -> m (Either InstructionError Instruction)
runInstruction instructionText =
  case parseInstruction instructionText of
    (Just i) -> do
      enactInstruction i
      return $ Right i
    Nothing -> do
      return $ Left InstructionError

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
  anyString ["n", "north", "go north"]
  return $ Go DirNorth

parseSouth :: Parser Instruction
parseSouth = do
  anyString ["s", "south", "go south"]
  return $ Go DirSouth

parseEast :: Parser Instruction
parseEast = do
  anyString ["e", "east", "go east"]
  return $ Go DirEast

parseWest :: Parser Instruction
parseWest = do
  anyString ["w", "west", "go west"]
  return $ Go DirWest

parseUp :: Parser Instruction
parseUp = do
  anyString ["u", "up", "go up"]
  return $ Go DirUp

parseDown :: Parser Instruction
parseDown = do
  anyString ["d", "down", "go down"]
  return $ Go DirDown

parseInventory :: Parser Instruction
parseInventory = do
  anyString ["i", "inv", "inventory"]
  return Inventory

parseHelp :: Parser Instruction
parseHelp = do
  anyString ["h", "help", "hint"]
  return Help

parseUndo :: Parser Instruction
parseUndo = do
  string "undo"
  eof
  return Undo

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

parseOpen :: Parser Instruction
parseOpen = do
  string "open"
  spaces
  target <- many1 anyChar
  eof
  return $ OpenI (T.pack target)

parseWait :: Parser Instruction
parseWait = do
  string "wait" <|> string "do nothing"
  eof
  return Wait

parseSay :: Parser Instruction
parseSay = do
  string "say" <|> string "speak"
  spaces
  content <- many1 anyChar
  return $ Say (T.pack content)

parseEat :: Parser Instruction
parseEat = do
  string "eat"
  spaces
  target <- many1 anyChar
  eof
  return $ Eat (T.pack target)

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

parseTalkTo :: Parser Instruction
parseTalkTo = do
  string "talk to" <|> string "answer"
  spaces
  target <- many1 anyChar
  eof
  return $ TalkTo (T.pack target)

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

parseCombine :: Parser Instruction
parseCombine = do
  string "put" <|> string "combine"
  spaces
  target1 <- many1 letter
  spaces
  string "in" <|> string "with" <|> string "and"
  spaces
  target2 <- many1 letter
  eof
  return $ Combine (T.pack target1) (T.pack target2)

parseWear :: Parser Instruction
parseWear = do
  string "wear"
  spaces
  target <- many1 letter
  eof
  return $ Wear (T.pack target)

parseRemove :: Parser Instruction
parseRemove = do
  string "remove"
  spaces
  target <- many1 letter
  eof
  return $ Remove (T.pack target)

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
  <|> try parseHelp
  <|> try parseTalkTo
  <|> try parseEat
  <|> try parseCombine
  <|> try parseWear
  <|> try parseRemove
  <|> try parseUndo
  <|> try parseOpen
  <|> try parseSay

-- Parse out the instruction from the given text string
parseInstruction :: Text -> Maybe Instruction
parseInstruction iText =
  case parse instructionParser "" (T.unpack iText) of
    Left e -> Nothing -- error $ show e
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

getPlayerWornEntities :: (MonadState GameState m) => m [Entity]
getPlayerWornEntities = do
  p <- getPlayer
  traverse getEntity (S.toList $ p^.?wearing)

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

enactInstruction (Say content) = do
  liftIO $ TIO.putStrLn $ "You speak aloud: '" <> content <> "'"
  l <- getPlayerLocation
  when ( (l^.?name) == "bedroom" && T.isInfixOf "HOME" (T.toUpper content) && T.isInfixOf "NHS" (T.toUpper content) && T.isInfixOf "DEATH" (T.toUpper content) ) $ do
    addAchievement $ Achievement "Simon Says" "Do you do everything you hear on the radio?"
    setGameOver
    ap <- mkLocation "Astral Plane"
    desc ap (const $ return
      $ "As you recite the mantra on the radio, you lose touch with your corporeal body.\n"
      <> "You feel yourself becoming one with the simulacrum as you continue your chant.\n"
      <> "Hours pass - then days - and your lips chap with thirst. Still you chant.\n"
      <> "Your body expires, but your immortal soul may yet live on in the Hancock Machine.")
    modifyPlayer (set locationID $ Just $ ap^.?entityID)

  deliveryMan <- getEntityByName Human "delivery man"
  when ( (l^.?name) == "hallway" && isJust deliveryMan) $ enactInstruction (TalkTo "delivery man")

enactInstruction Help =
  liftIO
  $ TIO.putStrLn
  $ T.unlines [ "You can 'go north', 'north' or just 'n'."
              , "If nothing is happening, just 'wait'"
              , "'eat' stuff! 'wear' or 'remove' stuff! 'look at' stuff!"
              , "'talk to' the people you meet!"
              , "'turn on' stuff! 'turn off' stuff!"
              , "'put X in Y' or 'combine X with Y' if you think that's a good idea"
              , "'get thing' and 'drop thing', and 'i' or 'inventory' to see what you've got"
              , "Did ya fuck something up? 'undo' to go back a step!"
              ]

enactInstruction (Get target) = do
  p <- getPlayer
  es <- getTargetedEntitiesNearPlayer target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "No " <> target <> " to get."
    es -> do
      let e = head es
      if isJust (e^.locationID) && e^.storable == Storable
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
      case e^.droppable of
        Droppable -> do
           modifyEntity (set locationID (l^.entityID)) (e^.?entityID)
           modifyPlayer (over inventory (fmap (S.delete $ e^.?entityID)))
           incrementClock
           liftIO $ TIO.putStrLn $ "You drop the " <> target
        Undroppable ->
          liftIO $ TIO.putStrLn $ e^.?name <> " cannot be dropped"

enactInstruction (OpenI target) = do
  l <- getPlayerLocation
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "No " <> target <> " to open."
    es -> do
      let e = head es
      case e^.?name of
        "front door" ->
          liftIO $ TIO.putStrLn "This hasn't been opened in years. Government mandate. You wouldn't want that air coming in, anyhow.\nYour voice would carry through it."
        "hatch" ->
          case e^.?openClosed of
            Open -> liftIO $ TIO.putStrLn "The hatch is already open"
            Closed -> liftIO $ TIO.putStrLn "This can only be opened from the outside for deliveries."
        n -> liftIO $ TIO.putStrLn $ n <> " cannot be opened"

enactInstruction (Wear target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Don't know " <> target
    es -> do
      let e = head es
      case e^.wearable of
        Wearable -> do
          liftIO $ TIO.putStrLn $ "You start wearing the " <> (e^.?name)
          modifyPlayer (over wearing (fmap (S.insert $ e^.?entityID)))
          modifyPlayer (over inventory (fmap (S.delete $ e^.?entityID)))
          modifyEntity (set locationID Nothing) (e^.?entityID)
        Unwearable ->
          liftIO $ TIO.putStrLn $ e^.?name <> " cannot be worn"

enactInstruction (Remove target) = do
  es <- filterByTarget target <$> getPlayerWornEntities
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Not wearing " <> target
    es -> do
      let e = head es
      liftIO $ TIO.putStrLn $ "You remove the " <> (e^.?name)
      modifyPlayer (over wearing (fmap (S.delete $ e^.?entityID)))
      modifyPlayer (over inventory (fmap (S.insert $ e^.?entityID)))

enactInstruction (LookAt target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Can't see " <> target
    es -> do
      let e = head es
      d <- getDescription (e^.?entityID)
      liftIO $ TIO.putStrLn d
      -- Report on anything the thing contains
      containedEs <- getEntitiesAt (e^.?entityID)
      unless (null containedEs)
        $ liftIO $ TIO.putStrLn $ "Inside is: " <> T.intercalate ", " ((^.?name) <$> containedEs)

enactInstruction Look = liftIO $ TIO.putStrLn "You look around... some more?"

enactInstruction Inventory = do
  p <- getPlayer
  invMsg <- case S.size (p^.?inventory) of
              0 -> return "Your inventory is empty."
              _ -> do
                es <- getInventoryEntities
                return $ "You have: " <> T.intercalate ", " ((^.?name) <$> es)
  wearMsg <- case S.size (p^.?wearing) of
               0 -> return "You are not wearing anything special."
               _ -> do
                 es <- getPlayerWornEntities
                 return $ "You are wearing: " <> T.intercalate ", " ((^.?name) <$> es)
  liftIO $ TIO.putStrLn $ invMsg <> "\n" <> wearMsg


enactInstruction Wait = do
  incrementClock
  liftIO $ TIO.putStrLn "You wait idly."

enactInstruction (TurnOn target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Can't find " <> target
    es -> do
      let e = head es
      turnOn (e^.?entityID)

enactInstruction (TurnOff target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Can't find " <> target <> " is"
    es -> do
      let e = head es
      turnOff (e^.?entityID)

enactInstruction Undo = do
  hs <- gets (view history)
  case hs of
    [] -> liftIO $ TIO.putStrLn "Can't go back any further"
    (h:_) -> do
      liftIO $ TIO.putStrLn "By concentrating really hard, you turn time backwards a tiny amount"
      put h

enactInstruction (Eat target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Can't find " <> target <> " is"
    es -> do
      let e = head es
      case e^.edible of
        Edible -> do
          p <- getPlayer
          liftIO $ TIO.putStrLn $ "You... you eat the " <> e^.?name
          modifyPlayer (over inventory (fmap (S.delete $ e^.?entityID)))
          removeEntity $ e^.?entityID
          incrementClock
        Inedible -> liftIO $ TIO.putStrLn $ "You try hard, but the " <> (e^.?name) <> " is inedible."

enactInstruction (Combine t1 t2) = do
  es1 <- allValidTargetedEntities t1
  es2 <- allValidTargetedEntities t2
  if null es1
     then liftIO $ TIO.putStrLn $ "Don't know what " <> t1 <> " is"
     else if null es2 then liftIO $ TIO.putStrLn $ "Don't know what " <> t2 <> " is"
     else let e1 = head es1
              e2 = head es2
           in combine (e1^.?entityID) (e2^.?entityID)

enactInstruction (TalkTo target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> liftIO $ TIO.putStrLn $ "Don't know what " <> target <> " is"
    es -> do
      let e = head es
      talkTo (e^.?entityID)

talkTo :: (MonadState GameState m, MonadIO m) => EntityID -> m ()
talkTo eID = do
  e <- getEntity eID
  -- TODO: Vastly improve the conversation router.
  -- TODO: Decouple this game logic by adding a convo-waiting response to the entity
  case e^.?name of
    "delivery man" -> do
      incrementClock
      liftIO $ TIO.putStrLn "\"Fuckin' finally man! What took you? I gotta make hundreds more of these today to get mine!\""
      liftIO $ TIO.putStrLn "The delivery guy swipes a card, throwing open the hatch, slides a ration box through, and leaves hurriedly."
      liftIO $ TIO.putStrLn "The box tips over, spilling some of its contents."
      -- The guy leaves, the hatch opens
      removeAlert "Delivery"
      removeEntity $ e^.?entityID
      hatch <- getOneEntityByName SimpleObj "hatch"
      modifyEntity (set openClosed $ Just Open) (hatch^.?entityID)

      -- Can now get to the street
      hallway <- getLocationByName "hallway"
      street <- getLocationByName "street"
      modifyEntity (set toNorth $ Just (street^.?entityID)) (hallway^.?entityID)

      -- The hatch shuts after 2 more goes
      oldTime <- gets (view clock)
      addWatcher $ do
        hatch <- getOneEntityByName SimpleObj "hatch"
        newTime <- gets (view clock)
        plunger <- getOneEntityByName SimpleObj "plunger"
        when (hatch^.?openClosed == Open && newTime >= oldTime + 1 && (plunger^.locationID) /= (hatch ^.entityID)) $ do
          addAlert "HatchShut" "The hatch on the front door has slammed shut, and won't reopen for another week."
          modifyEntity (set openClosed $ Just Closed) (hatch^.?entityID)

      -- The rations arrive
      paperPlate <- mkSimpleObj "paper plate" ["plate", "paper plate"] (Just $ hallway^.?entityID)
      modifyEntity (set storable Storable) (paperPlate^.?entityID)
      desc paperPlate (const $ return "A paper plate, like we used to use at picnics in the before times.")

      rationBox <- mkSimpleObj "ration box" ["ration box", "box"] (Just $ hallway^.?entityID)
      desc rationBox (const $ return "A brown cardboard box.")
      modifyEntity (set storable Storable) (rationBox^.?entityID)

      rations <- mkSimpleObj "assorted rations" ["rations"] (Just $ hallway^.?entityID)
      desc rations (const $ return "Assorted rations - pouches of dehydrated egg, carbohydrate gunge, that sort of thing.")
      modifyEntity (set storable Storable) (rations^.?entityID)
      modifyEntity (set edible Edible) (rations^.?entityID)

    _ -> liftIO $ TIO.putStrLn $ "Can't talk to " <> e^.?name

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
                    incrementClock
                  On -> liftIO $ TIO.putStrLn "Already chirping away, friend."
       SimpleObj -> case e^.?name of
                      "bath" -> do
                        liftIO $ TIO.putStrLn "You turn the rusty taps, and water floods the rotten tub. It quickly reaches the overflow."
                        modifyEntity (set onOff $ Just On) (e^.?entityID)
                        incrementClock
                      n -> liftIO $ TIO.putStrLn $ "Can't turn on " <> n
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
                    incrementClock
       Radio -> case e^.?onOff of
                  Off -> liftIO $ TIO.putStrLn "The radio is already off."
                  On -> do
                    liftIO $ TIO.putStrLn "There's no off switch, but you dial your way to the most quiet static you can find."
                    modifyEntity (set onOff $ Just Off) (e^.?entityID)
                    incrementClock
       SimpleObj -> case e^.?name of
                      "bath" -> do
                        liftIO $ TIO.putStrLn "You turn off the taps and the water quickly drains through the open plug."
                        modifyEntity (set onOff $ Just Off) (e^.?entityID)
                        incrementClock
                      n -> liftIO $ TIO.putStrLn $ "Can't turn off " <> n
       _ -> liftIO $ TIO.putStrLn "Nothing happens"

combine :: (MonadState GameState m, MonadIO m) => EntityID -> EntityID -> m ()
combine eID1 eID2 = do
  e1 <- getEntity eID1
  e2 <- getEntity eID2
  case (e1^.?name, e2^.?name) of
    ("plunger", "hatch") -> do
      let (plunger, hatch) = (e1, e2)
      if hatch^.?openClosed == Closed
         then liftIO $ TIO.putStrLn "The hatch is closed."
         else do
           liftIO $ TIO.putStrLn "You use the filthy plunger to prop open the delivery hatch."
           modifyEntity (set locationID $ Just (hatch^.?entityID)) (plunger^.?entityID)
           modifyPlayer (over inventory (fmap (S.delete $ plunger^.?entityID)))
    ("paper plate", "an elasticated hairband") -> makeBand e1 e2
    ("an elasticated hairband", "paper plate") -> makeBand e2 e1
    ("alarm clock", "bath") -> do
      liftIO $ TIO.putStrLn "You place the alarm clock into the bath, like a normal person would."
      modifyPlayer (over inventory (fmap (S.delete $ e1^.?entityID)))
      modifyEntity (set locationID $ Just (e2^.?entityID)) (e1^.?entityID)
    _ -> liftIO $ TIO.putStrLn $ "Can't combine " <> (e1^.?name) <> " and " <> (e2^.?name)

-- Make the makeshift facemask
makeBand plate band = do
  liftIO $ TIO.putStrLn "Using your medical expertise and surgical dexterity, you fashion a fucking facemask out of these two unlikely items."
  modifyPlayer (over inventory (fmap (S.delete $ plate^.?entityID)))
  modifyPlayer (over inventory (fmap (S.delete $ band^.?entityID)))
  removeEntity (plate^.?entityID)
  removeEntity (band^.?entityID)
  mask <- mkSimpleObj "makeshift facemask" ["facemask", "mask"] Nothing
  modifyEntity (set wearable Wearable) (mask^.?entityID)
  desc mask (const $ return "A super-safe, military grade, virus-repellant face mask.")
  modifyPlayer (over inventory (fmap (S.insert $ mask^.?entityID)))
