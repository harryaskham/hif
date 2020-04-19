{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Instruction where

import Tools
import EntityType
import GameState
import Entity
import Engine
import Handler
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

-- Handle input, potentially running an instruction and modifying game state.
runInstruction :: Text -> App ()
runInstruction instructionText = do
  prevState <- get
  case parseInstruction instructionText of
    (Just i) -> do
      -- Run the instruction and store its outcome
      instructionState <- enactInstruction i
      modify $ set lastInstructionState instructionState
      -- End of turn stuff
      runWatchers
      logT =<< describeCurrentTurn
      unless (i == Undo) $ modify $ over history (prevState:)
    Nothing -> logT "Invalid instruction"

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

parseBreak :: Parser Instruction
parseBreak = do
  string "break"
  spaces
  target <- many1 anyChar
  eof
  return $ Break (T.pack target)

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

parseDrink :: Parser Instruction
parseDrink = do
  string "drink"
  spaces
  target <- many1 anyChar
  eof
  return $ Drink (T.pack target)

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

parseUse :: Parser Instruction
parseUse = do
  string "use"
  spaces
  target <- many1 anyChar
  eof
  return $ Use (T.pack target)

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
  string "put" <|> string "combine" <|> string "use"
  spaces
  target1 <- many1 letter
  spaces
  string "in" <|> string "with" <|> string "and" <|> string "on"
  spaces
  target2 <- many1 letter
  eof
  return $ Combine (T.pack target1) (T.pack target2)

parseGive :: Parser Instruction
parseGive = do
  string "give"
  spaces
  target1 <- many1 letter
  spaces
  string "to"
  spaces
  target2 <- many1 letter
  eof
  return $ Give (T.pack target1) (T.pack target2)

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
  -- <|> try parseUse
  <|> try parseTurnOn
  <|> try parseTurnOff
  <|> try parseHelp
  <|> try parseTalkTo
  <|> try parseEat
  <|> try parseDrink
  <|> try parseCombine
  <|> try parseGive
  <|> try parseWear
  <|> try parseRemove
  <|> try parseUndo
  <|> try parseOpen
  <|> try parseBreak
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

-- Enact the instruction, reporting on success or failures status.
enactInstruction :: Instruction -> App (Either InstructionError Instruction)
enactInstruction i@(Go dir) = do
  l <- getPlayerLocation
  case l^.lensForDir dir of
    Just lID -> do
      modifyPlayer (set locationID (Just lID))
      incrementClock
      -- Mark this place as visited
      p <- getPlayer
      modifyEntity (set visited $ Just True) (p^.?locationID)
      return $ Right i
    Nothing -> do
      logT $ "Cannot travel " <> showt dir <> "."
      return $ Left InstructionError

enactInstruction i@(Say content) = do
  logT $ "You speak aloud: '" <> content <> "'"
  hs <- gets (view sayHandlers)
  runHandlers hs
  return $ Right i
    where
      runHandlers [] = return ()
      runHandlers (h:hs) = do
        _ <- h content
        runHandlers hs
  
enactInstruction i@Help = do
  logT
    $ T.unlines [ "You can 'go north', 'north' or just 'n'."
                , "You can also go 'up' and 'down'."
                , "If nothing is happening, just 'wait'"
                , "'eat' and 'drink' stuff! 'wear' or 'remove' stuff! 'look at' stuff! 'use' stuff!"
                , "'use X on Y' is also a thing"
                , "Forget where you are? 'look'!"
                , "'talk to' the people you meet!"
                , "'turn on' stuff! 'turn off' stuff!"
                , "'put X in Y' or 'combine X with Y' if you think that's a good idea"
                , "'break' things if you like"
                , "'give X to Y' works too!"
                , "'say' something to say it out loud"
                , "'get thing' and 'drop thing', and 'i' or 'inventory' to see what you've got"
                , "Did ya fuck something up? 'undo' to go back a step!"
                ]
  return $ Right i

enactInstruction i@(Get target) = do
  p <- getPlayer
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "No " <> target <> " to get."
      return $ Left InstructionError
    Just e ->
      ifM (inPlayerInventory (e^.?entityID))
          (do
            logT $ "You already have " <> e^.?name
            return $ Left InstructionError)
          (if isJust (e^.locationID) && e^.storable == Storable
                 then do
                   modifyEntity (set locationID Nothing) (e^.?entityID)
                   modifyPlayer (over inventory (fmap (S.insert $ e^.?entityID)))
                   incrementClock
                   logT $ "You get the " <> target
                   return $ Right i
                 else do
                   logT $ "Cannot get the " <> (e^.?name)
                   return $ Left InstructionError)

enactInstruction i@(Drop target) = do
  l <- getPlayerLocation
  eM <- oneInventoryTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "No " <> target <> " to drop."
      return $ Left InstructionError
    Just e -> case e^.droppable of
      Droppable -> do
        moveFromInventory e l
        incrementClock
        logT $ "You drop the " <> target
        return $ Right i
      Undroppable -> do
        logT $ e^.?name <> " cannot be dropped"
        return $ Left InstructionError

enactInstruction i@(OpenI target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "No " <> target <> " to open."
      return $ Left InstructionError
    Just e -> do
      hs <- gets (view openHandlers)
      case M.lookup (e^.?entityID) hs of
        Nothing -> do
          logT $ (e^.?name) <> " cannot be opened"
          return $ Left InstructionError
        Just h -> do
          h e
          return $ Right i

enactInstruction i@(Break target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "No " <> target <> " to break."
      return $ Left InstructionError
    Just e -> do
      hs <- gets (view breakHandlers)
      case M.lookup (e^.?entityID) hs of
        Nothing -> do
          logT $ (e^.?name) <> " cannot be broken."
          return $ Left InstructionError
        Just h -> do
          h e
          return $ Right i

enactInstruction i@(Wear target) = do
  eM <- oneInventoryTargetedEntity target
  case eM of
    Nothing -> do
      logT "You don't have that"
      return $ Left InstructionError
    Just e -> case e^.wearable of
      Wearable -> do
        logT $ "You start wearing the " <> (e^.?name)
        modifyPlayer (over wearing (fmap (S.insert $ e^.?entityID)))
        removeFromInventory $ e^.?entityID
        return $ Right i
      Unwearable -> do
        logT $ e^.?name <> " cannot be worn"
        return $ Left InstructionError

enactInstruction i@(Remove target) = do
  es <- filterByTarget target <$> getPlayerWornEntities
  case es of
    [] -> do
      logT $ "Not wearing " <> target
      return $ Left InstructionError
    es -> do
      let e = head es
      logT $ "You remove the " <> (e^.?name)
      modifyPlayer (over wearing (fmap (S.delete $ e^.?entityID)))
      addToInventory $ e^.?entityID
      return $ Right i

enactInstruction i@(LookAt target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT "Can't see that"
      return $ Left InstructionError
    Just e -> do
      d <- getDescription (e^.?entityID)
      logT d
      containedEs <- getEntitiesAt (e^.?entityID)
      unless (null containedEs)
        $ logT $ "Inside is: " <> T.intercalate ", " ((^.?name) <$> containedEs)
      return $ Right i

enactInstruction i@Look = do
  logT =<< describeCurrentTurn
  return $ Right i

enactInstruction i@Inventory = do
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
  cheevsGot <- fmap fst . M.toList <$> gets (view achievements)
  cheevsLeft <- gets (view remainingAchievements)
  let cheevGotMessage = "\nAchievements unlocked:\n" <> T.intercalate "\n" cheevsGot
      cheevLeftMessage = case S.size cheevsLeft of
                    0 -> "You got all achievements!"
                    _ -> "\nAchievements still locked:\n" <> T.intercalate "\n" (S.toList cheevsLeft)
  logT $ T.intercalate "\n" [invMsg, wearMsg, cheevGotMessage, cheevLeftMessage]
  return $ Right i

enactInstruction i@Wait = do
  incrementClock
  logT "You wait idly."
  return $ Right i

enactInstruction i@(Use target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "Can't find " <> target
      return $ Left InstructionError
    Just e -> case e^.usable of
      Unusable -> do
        logT $ "Can't use " <> e^.?name
        return $ Left InstructionError
      Usable -> do
        hs <- gets (view useHandlers)
        case M.lookup (e^.?entityID) hs of
          Nothing -> do
            logT $ "No way to use " <> e^.?name
            return $ Left InstructionError
          Just h -> do
            h e
            return $ Right i

enactInstruction i@(TurnOn target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "Can't find " <> target
      return $ Left InstructionError
    Just e -> case e^.onOff of
      Nothing -> do
        logT "Can't turn that on"
        return $ Left InstructionError
      Just onOffState -> do
        hs <- gets (view turnOnHandlers)
        case M.lookup (e^.?entityID) hs of
          Nothing -> do
            logT $ "No way to turn on " <> e^.?name
            return $ Left InstructionError
          Just h -> do
            h e
            return $ Right i

enactInstruction i@(TurnOff target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "Can't find " <> target
      return $ Left InstructionError
    Just e -> case e^.onOff of
      Nothing -> do
        logT "Can't turn that off"
        return $ Left InstructionError
      Just onOffState -> do
        hs <- gets (view turnOffHandlers)
        case M.lookup (e^.?entityID) hs of
          Nothing -> do
            logT $ "No way to turn off " <> e^.?name
            return $ Left InstructionError
          Just h -> do
            h e
            return $ Right i

enactInstruction i@Undo = do
  hs <- gets (view history)
  case hs of
    [] -> do
      logT "Can't go back any further"
      return $ Left InstructionError
    (h:_) -> do
      logT "By concentrating really hard, you turn time backwards a tiny amount"
      put h
      return $ Right i

enactInstruction i@(Eat target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "Can't find " <> target
      return $ Left InstructionError
    Just e -> case e^.edible of
      Edible -> do
        hs <- gets (view eatHandlers)
        case M.lookup (e^.?entityID) hs of
          Nothing -> do
            logT $ "No way to eat the " <> e^.?name
            return $ Left InstructionError
          Just h -> do
            h e
            return $ Right i
      Inedible -> do
        logT $ "You try hard, but the " <> (e^.?name) <> " is inedible."
        return $ Left InstructionError

enactInstruction i@(Drink target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "Can't find " <> target
      return $ Left InstructionError
    Just e -> case e^.edible of
      Edible -> do
        hs <- gets (view drinkHandlers)
        case M.lookup (e^.?entityID) hs of
          Nothing -> do
            logT $ "No way to drink the " <> e^.?name
            return $ Left InstructionError
          Just h -> do
            h e
            return $ Right i
      Inedible -> do
        logT $ "The " <> (e^.?name) <> " cannot be drunk."
        return $ Left InstructionError

enactInstruction i@(Combine t1 t2) = do
  eM1 <- oneValidTargetedEntity t1
  eM2 <- oneValidTargetedEntity t2
  if isNothing eM1
     then do
       logT $ "Don't know what " <> t1 <> " is"
       return $ Left InstructionError
     else if isNothing eM2 then do
       logT $ "Don't know what " <> t2 <> " is"
       return $ Left InstructionError
     else let (Just e1) = eM1
              (Just e2) = eM2
           in do
              hs <- gets (view combinationHandlers)
              case M.lookup (e1^.?entityID, e2^.?entityID) hs of
                Nothing -> do
                  logT $ "Can't combine " <> (e1^.?name) <> " and " <> (e2^.?name)
                  return $ Left InstructionError
                Just h -> do
                  h e1 e2
                  return $ Right i

enactInstruction i@(Give t1 t2) = do
  eM1 <- oneValidTargetedEntity t1
  eM2 <- oneValidTargetedEntity t2
  if isNothing eM1
     then do
       logT $ "Don't know what " <> t1 <> " is"
       return $ Left InstructionError
     else if isNothing eM2 then do
       logT $ "Don't know what " <> t2 <> " is"
       return $ Left InstructionError
     else let (Just e1) = eM1
              (Just e2) = eM2
           in do
              hs <- gets (view giveHandlers)
              case M.lookup (e1^.?entityID, e2^.?entityID) hs of
                Nothing -> do
                  logT $ "Can't give " <> (e1^.?name) <> " to " <> (e2^.?name)
                  return $ Left InstructionError
                Just h -> do
                  h e1 e2
                  return $ Right i

enactInstruction i@(TalkTo target) = do
  eM <- oneValidTargetedEntity target
  case eM of
    Nothing -> do
      logT $ "Don't know what " <> target <> " is"
      return $ Left InstructionError
    Just e ->
      case e^.talkable of
        Talkable -> do
          hs <- gets (view talkToHandlers)
          case M.lookup (e^.?entityID) hs of
            Nothing -> do
              logT $ (e^.?name) <> " isn't listening to you."
              return $ Left InstructionError
            Just h -> do
              h
              return $ Right i
        Untalkable -> do
          logT $ "Can't talk to " <> e^.?name
          return $ Left InstructionError
