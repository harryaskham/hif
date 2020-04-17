{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module GameState where

import Tools
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

type AlertID = Text
type Alert = Text

type AchievementID = Text
data Achievement = Achievement AchievementID Text

type Stack st = StateT st IO
data GameState =
  GameState
    { _entities :: Map EntityID Entity
    , _descriptions :: Map EntityID (EntityID -> Stack GameState Text)
    , _clock :: Integer
    , _alerts :: Map AlertID Alert
    , _watchers :: [Stack GameState ()]
    , _history :: [GameState]
    , _achievements :: Map AchievementID Achievement
    , _gameOver :: Bool
    , _talkToHandlers :: Map EntityID (Stack GameState ())
    , _sayHandlers :: [Text -> Stack GameState ()]
    , _turnOnHandlers :: Map EntityID (EntityID -> Stack GameState ())
    , _turnOffHandlers :: Map EntityID (EntityID -> Stack GameState ())
    , _combinationHandlers :: Map (EntityID, EntityID) (EntityID -> EntityID -> Stack GameState ())
    , _eatHandlers :: Map EntityID (EntityID -> Stack GameState ())
    , _openHandlers :: Map EntityID (EntityID -> Stack GameState ())
    }
makeLenses ''GameState

mkGameState :: GameState
mkGameState = GameState { _entities=M.empty
                        , _descriptions=M.empty
                        , _clock=0
                        , _alerts=M.empty
                        , _watchers=[]
                        , _history=[]
                        , _achievements=M.empty
                        , _talkToHandlers=M.empty
                        , _gameOver=False
                        , _sayHandlers=[]
                        , _turnOnHandlers=M.empty
                        , _turnOffHandlers=M.empty
                        , _combinationHandlers=M.empty
                        , _eatHandlers=M.empty
                        , _openHandlers=M.empty
                        }

-- Instantiate MTL stack for game
type App = Stack GameState

-- Helpers for concrete workers
type Description = EntityID -> App Text
type Watcher = App ()
type TalkToHandler = App ()
type SayHandler = Text -> App ()
type TurnOnHandler = EntityID -> App ()
type TurnOffHandler = EntityID -> App ()
type CombinationHandler = EntityID -> EntityID -> App ()
type EatHandler = EntityID -> App ()
type OpenHandler = EntityID -> App ()

-- Output text to the screen within the Monad stack
logT :: Text -> App ()
logT = liftIO . TIO.putStrLn

addTalkToHandler :: EntityID -> TalkToHandler -> App ()
addTalkToHandler eID h = modify $ over talkToHandlers (M.insert eID h)

addEatHandler :: EntityID -> EatHandler -> App ()
addEatHandler eID h = modify $ over eatHandlers (M.insert eID h)

addOpenHandler :: EntityID -> OpenHandler -> App ()
addOpenHandler eID h = modify $ over openHandlers (M.insert eID h)

addSayHandler :: SayHandler -> App ()
addSayHandler h = modify $ over sayHandlers (h:)

addTurnOnHandler :: EntityID -> TurnOnHandler -> App ()
addTurnOnHandler eID h = modify $ over turnOnHandlers (M.insert eID h)

addTurnOffHandler :: EntityID -> TurnOffHandler -> App ()
addTurnOffHandler eID h = modify $ over turnOffHandlers (M.insert eID h)

addCombinationHandler :: EntityID -> EntityID -> CombinationHandler -> App ()
addCombinationHandler eID1 eID2 h = do
  modify $ over combinationHandlers (M.insert (eID1, eID2) h)
  modify $ over combinationHandlers (M.insert (eID2, eID1) (flip h))

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
  let (Just e) = eM
  return e

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
removeEntity eID = modify $ \s -> s & entities %~ M.delete eID

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

getInventoryEntities :: App [Entity]
getInventoryEntities = do
  p <- getPlayer
  traverse getEntity (S.toList $ p^.?inventory)

inPlayerInventory :: EntityID -> App Bool
inPlayerInventory eID = do
  p <- getPlayer
  return $ eID `S.member` (p^.?inventory)

getPlayerWornEntities :: App [Entity]
getPlayerWornEntities = do
  p <- getPlayer
  traverse getEntity (S.toList $ p^.?wearing)

filterInventoryByTarget :: Target -> App [Entity]
filterInventoryByTarget t = filterByTarget t <$> getInventoryEntities

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

-- Modify the given entity persisted in the state.
modifyEntity :: (Entity -> Entity) -> EntityID -> App ()
modifyEntity f eID = do
  es <- gets (view entities)
  case M.lookup eID es of
    Just e -> modify $ \s -> s & entities %~ M.insert eID (f e)
    Nothing -> return ()

-- Register the given description function with the entity
addDesc :: EntityID -> Description -> App ()
addDesc eID d = modify $ \s -> s & descriptions %~ M.insert eID d

-- Quick helper to avoid ID usage
desc :: Entity -> Description -> App ()
desc e = addDesc (e^.?entityID)

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

mkHuman :: Name -> [Target] -> EntityID -> App Entity
mkHuman name targets locationID = do
  humanID <- newID Human
  let human = def { _entityID=Just humanID
                  , _name=Just name
                  , _targets=Just $ S.fromList targets
                  , _locationID=Just locationID
                  }
  registerEntity human
  return human

mkLocation :: Name -> App Entity
mkLocation name = do
  locationID <- newID Location
  let location = def { _entityID=Just locationID
                     , _name=Just name
                     , _visited=Just False
                     }
  registerEntity location
  return location

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

describeCurrentTurn :: App Text
describeCurrentTurn = do
  st <- get
  p <- getPlayer
  l <- getEntity $ p^.?locationID
  es <- filter (\e -> entityType e /= Player) <$> getEntitiesAt (l^.?entityID)
  clock <- gets (view clock)
  lDesc <- getDescription $ l^.?entityID
  alertsMap <- gets (view alerts)
  let header = Just $ "\n" <> T.toUpper (l^.?name) <> "\n=========="
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

  return $ T.intercalate "\n" (catMaybes [header, desc, alerts, thingsHere] ++ directions)

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

data InstructionError = InstructionError

-- Handle input, potentially running an instruction and modifying game state.
runInstruction :: Text -> App (Either InstructionError Instruction)
runInstruction instructionText =
  case parseInstruction instructionText of
    (Just i) -> do
      enactInstruction i
      return $ Right i
    Nothing -> return $ Left InstructionError

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

-- Increment game time by one.
incrementClock :: MonadState GameState App => App ()
incrementClock = modify $ over clock (+1)

enactInstruction :: Instruction -> App ()
enactInstruction (Go dir) = do
  l <- getPlayerLocation
  case l^.lensForDir dir of
    Just lID -> do
      modifyPlayer (set locationID (Just lID))
      incrementClock
    Nothing -> logT $ "Cannot travel " <> showt dir <> "."

enactInstruction (Say content) = do
  logT $ "You speak aloud: '" <> content <> "'"
  hs <- gets (view sayHandlers)
  runHandlers hs
    where
      runHandlers [] = return ()
      runHandlers (h:hs) = do
        _ <- h content
        runHandlers hs
  
enactInstruction Help =
  liftIO
  $ TIO.putStrLn
  $ T.unlines [ "You can 'go north', 'north' or just 'n'."
              , "If nothing is happening, just 'wait'"
              , "'eat' stuff! 'wear' or 'remove' stuff! 'look at' stuff!"
              , "'talk to' the people you meet!"
              , "'turn on' stuff! 'turn off' stuff!"
              , "'put X in Y' or 'combine X with Y' if you think that's a good idea"
              , "'say' something to say it out loud"
              , "'get thing' and 'drop thing', and 'i' or 'inventory' to see what you've got"
              , "Did ya fuck something up? 'undo' to go back a step!"
              ]

enactInstruction (Get target) = do
  p <- getPlayer
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> logT $ "No " <> target <> " to get."
    Just e ->
      ifM (inPlayerInventory (e^.?entityID))
          (logT $ "You already have " <> e^.?name)
          (if isJust (e^.locationID) && e^.storable == Storable
                 then do
                   modifyEntity (set locationID Nothing) (e^.?entityID)
                   modifyPlayer (over inventory (fmap (S.insert $ e^.?entityID)))
                   incrementClock
                   logT $ "You get the " <> target
                 else
                   logT $ "Cannot get the " <> (e^.?name))

enactInstruction (Drop target) = do
  l <- getPlayerLocation
  es <- filterInventoryByTarget target
  case es of
    [] -> logT $ "No " <> target <> " to drop."
    es -> do
      let e = head es
      case e^.droppable of
        Droppable -> do
           modifyEntity (set locationID (l^.entityID)) (e^.?entityID)
           modifyPlayer (over inventory (fmap (S.delete $ e^.?entityID)))
           incrementClock
           logT $ "You drop the " <> target
        Undroppable ->
          logT $ e^.?name <> " cannot be dropped"

enactInstruction (OpenI target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> logT $ "No " <> target <> " to open."
    Just e -> do
      hs <- gets (view openHandlers)
      case M.lookup (e^.?entityID) hs of
        Nothing -> logT $ (e^.?name) <> " cannot be opened"
        Just h -> h (e^.?entityID)

enactInstruction (Wear target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> logT $ "Don't know " <> target
    es -> do
      let e = head es
      case e^.wearable of
        Wearable -> do
          logT $ "You start wearing the " <> (e^.?name)
          modifyPlayer (over wearing (fmap (S.insert $ e^.?entityID)))
          modifyPlayer (over inventory (fmap (S.delete $ e^.?entityID)))
          modifyEntity (set locationID Nothing) (e^.?entityID)
        Unwearable ->
          logT $ e^.?name <> " cannot be worn"

enactInstruction (Remove target) = do
  es <- filterByTarget target <$> getPlayerWornEntities
  case es of
    [] -> logT $ "Not wearing " <> target
    es -> do
      let e = head es
      logT $ "You remove the " <> (e^.?name)
      modifyPlayer (over wearing (fmap (S.delete $ e^.?entityID)))
      modifyPlayer (over inventory (fmap (S.insert $ e^.?entityID)))

enactInstruction (LookAt target) = do
  es <- allValidTargetedEntities target
  case es of
    [] -> logT $ "Can't see " <> target
    es -> do
      let e = head es
      d <- getDescription (e^.?entityID)
      logT d
      -- Report on anything the thing contains
      containedEs <- getEntitiesAt (e^.?entityID)
      unless (null containedEs)
        $ logT $ "Inside is: " <> T.intercalate ", " ((^.?name) <$> containedEs)

enactInstruction Look = logT "You look around... some more?"

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
  logT $ invMsg <> "\n" <> wearMsg


enactInstruction Wait = do
  incrementClock
  logT "You wait idly."

enactInstruction (TurnOn target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> logT $ "Can't find " <> target
    Just e -> case e^.onOff of
      Nothing -> logT "Can't turn that on"
      Just onOffState -> do
        hs <- gets (view turnOnHandlers)
        case M.lookup (e^.?entityID) hs of
          Nothing -> logT $ "No way to turn on " <> e^.?name
          Just h -> h (e^.?entityID)

enactInstruction (TurnOff target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> logT $ "Can't find " <> target
    Just e -> case e^.onOff of
      Nothing -> logT "Can't turn that off"
      Just onOffState -> do
        hs <- gets (view turnOffHandlers)
        case M.lookup (e^.?entityID) hs of
          Nothing -> logT $ "No way to turn off " <> e^.?name
          Just h -> h (e^.?entityID)

enactInstruction Undo = do
  hs <- gets (view history)
  case hs of
    [] -> logT "Can't go back any further"
    (h:_) -> do
      logT "By concentrating really hard, you turn time backwards a tiny amount"
      put h

enactInstruction (Eat target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> logT $ "Can't find " <> target
    Just e -> case e^.edible of
      Edible -> do
        hs <- gets (view eatHandlers)
        case M.lookup (e^.?entityID) hs of
          Nothing -> logT $ "No way to eat the " <> e^.?name
          Just h -> h (e^.?entityID)
      Inedible -> logT $ "You try hard, but the " <> (e^.?name) <> " is inedible."

enactInstruction (Combine t1 t2) = do
  eM1 <- oneValidTargetedEntity t1
  eM2 <- oneValidTargetedEntity t2
  if isNothing eM1
     then logT $ "Don't know what " <> t1 <> " is"
     else if isNothing eM2 then logT $ "Don't know what " <> t2 <> " is"
     else let (Just e1) = eM1
              (Just e2) = eM2
           in do
              hs <- gets (view combinationHandlers)
              case M.lookup (e1^.?entityID, e2^.?entityID) hs of
                Nothing -> logT $ "Can't combine " <> (e1^.?name) <> " and " <> (e2^.?name)
                Just h -> h (e1^.?entityID) (e2^.?entityID)

enactInstruction (TalkTo target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> logT $ "Don't know what " <> target <> " is"
    Just e ->
      case e^.talkable of
        Talkable -> do
          hs <- gets (view talkToHandlers)
          fromMaybe
            (logT $ (e^.?name) <> " isn't listening to you.")
            (M.lookup (e^.?entityID) hs)
        Untalkable -> logT $ "Can't talk to " <> e^.?name

addAlert :: AlertID -> Alert -> App ()
addAlert aID a = modify $ \s -> s & alerts %~ M.insert aID a

removeAlert :: AlertID -> App ()
removeAlert aID = modify $ \s -> s & alerts %~ M.delete aID

addAchievement :: Achievement -> App ()
addAchievement a@(Achievement aID aContent) = do
  logT $ "\n***ACHIEVEMENT UNLOCKED***\n" <> aID <> "\n" <> aContent
  modify (\s -> s & achievements %~ M.insert aID a)

hasAchievement :: AchievementID -> App Bool
hasAchievement aID = do
  as <- gets (view achievements)
  return $ M.member aID as

setGameOver :: App ()
setGameOver = modify $ set gameOver True
