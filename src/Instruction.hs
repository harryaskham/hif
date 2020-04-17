{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Instruction where

import Tools
import EntityType
import GameState
import Entity
import Engine

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

lensForDir DirNorth = toNorth
lensForDir DirEast = toEast
lensForDir DirSouth = toSouth
lensForDir DirWest = toWest
lensForDir DirUp = toUp
lensForDir DirDown = toDown

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
  cheevs <- gets (view remainingAchievements)
  cheevMessage <- case S.size cheevs of
                    0 -> return "You got all achievements!"
                    _ -> return $ "\nAchievements still locked:\n" <> T.intercalate "\n" (S.toList cheevs)
  logT $ T.intercalate "\n" [invMsg, wearMsg, cheevMessage]


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
