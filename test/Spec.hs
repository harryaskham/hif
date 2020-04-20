{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Tools
import EntityType
import GameState
import CovidGame
import FourthGame
import Engine
import Entity
import Instruction
import InstructionType

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.State
import Control.Lens
import Data.Either
import Data.Maybe

-- Run the game with the given commands
withCmds :: [Text] -> App ()
withCmds [] = return ()
withCmds (c:cs) = do
  logT $ "> " <> c
  runInstruction c
  withCmds cs

checkPreds :: App () -> [Text] -> [App Bool] -> Expectation
checkPreds setup cs ps = do
  let g :: App Bool
      g = do
            setup
            withCmds cs
            all (==True) <$> sequence ps
  (fst <$> runStateT g mkGameState) `shouldReturn` True

main :: IO ()
main = hspec do
  describe "CovidGame" do
    it "wins the regular way" do
      checkPreds
        buildCovidGame
        [ "get band"
        , "n"
        , "e"
        , "get plunger"
        , "w"
        , "talk to man"
        , "put plunger in hatch"
        , "combine plate and band"
        , "wear mask"
        , "n"
        ]
        [ gets (view gameOver) ]

    it "gets the bad ending" do
      checkPreds
        buildCovidGame
        [ "n"
        , "wait"
        , "wait"
        , "wait"
        , "wait"
        , "talk to man"
        , "n"
        ]
        [ gets (view gameOver) ]

    it "gets the astral ending" do
      checkPreds
        buildCovidGame
        [ "say STAY AT HOME. REMEMBER THE NHS. DEFY DEATH." ]
        [ gets (view gameOver)
        , hasAchievement "Simon Says"
        ]

    it "gets wet clock cheev" do
      checkPreds
        buildCovidGame
        [ "get clock"
        , "n"
        , "e"
        , "put clock in bath"
        , "turn on bath"
        ]
        [ hasAchievement "Big Wet Clock" ]

    it "gets ration cheev" do
      checkPreds
        buildCovidGame
        [ "n"
        , "wait"
        , "wait"
        , "wait"
        , "wait"
        , "talk to man"
        , "eat rations"
        ]
        [ hasAchievement "Eyes Bigger Than Belly" ]

    it "cant talk to man before arrival" do
      checkPreds
        buildCovidGame
        [ "n"
        , "talk to man"
        ]
        [ (==Left InstructionError) <$> gets (view lastInstructionState) ]

    it "can talk to the man after arrival" do
      checkPreds
        buildCovidGame
        [ "n"
        , "wait"
        , "wait"
        , "wait"
        , "wait"
        , "talk to man"
        ]
        [ (==Right (TalkTo "man")) <$> gets (view lastInstructionState) ]

    it "cant talk to the man twice" do
      checkPreds
        buildCovidGame
        [ "n"
        , "wait"
        , "wait"
        , "wait"
        , "wait"
        , "talk to man"
        , "talk to man"
        ]
        [ (==Left InstructionError) <$> gets (view lastInstructionState) ]

  describe "FourthGame" do
    it "can do a bunch of things to the item" do
      sequence_
        $ checkPreds buildFourthGame
        <$> [ [ "n", "get item" ]
            , [ "n", "talk to item" ]
            , [ "n", "eat item" ]
            , [ "n", "drink item" ]
            , [ "n", "use item" ]
            , [ "n", "turn on item" ]
            , [ "n", "turn off item" ]
            ]
        <*> pure [ isRight <$> gets (view lastInstructionState) ]

    it "can get the loop" do
      checkPreds
        buildFourthGame
        [ "n"
        , "w"
        , "talk to monk"
        , "say wooooo"
        , "wait"
        ]
        [ getOneEntityByName SimpleObj "loop of thread" >>= inPlayerInventory ]

    it "cant break the loop elsewhere" do
      checkPreds
        buildFourthGame
        [ "n"
        , "w"
        , "talk to monk"
        , "say wooooo"
        , "wait"
        , "break loop"
        ]
        [ getOneEntityByName SimpleObj "loop of thread" >>= inPlayerInventory ]

    it "can break the loop with the terrified man" do
      checkPreds
        buildFourthGame
        [ "n"
        , "w"
        , "talk to monk"
        , "say wooooo"
        , "wait"
        , "e"
        , "u"
        , "s"
        , "break loop"
        , "talk to man"
        ]
        [ isNothing <$> getEntityByName SimpleObj "loop of thread"
        , getOneEntityByName SimpleObj "cleaver" >>= inPlayerInventory
        ]

    it "can get the pen" do
      checkPreds
        buildFourthGame
        [ "n"
        , "w"
        , "look at trees"
        , "get paw"
        , "talk to monk"
        , "say wooooo"
        , "wait"
        , "e"
        , "u"
        , "s"
        , "break loop"
        , "talk to man"
        , "n"
        , "e"
        , "talk to woman"
        , "give cleaver to woman"
        , "give paw to woman"
        ]
        [ isNothing <$> getEntityByName SimpleObj "cleaver"
        , isNothing <$> getEntityByName SimpleObj "paw"
        , getOneEntityByName SimpleObj "marker pen" >>= inPlayerInventory
        ]

    it "can reach the grating but cant pass" do
      checkPreds
        buildFourthGame
        [ "n"
        , "w"
        , "look at trees"
        , "get paw"
        , "talk to monk"
        , "say wooooo"
        , "wait"
        , "e"
        , "u"
        , "s"
        , "break loop"
        , "talk to man"
        , "n"
        , "e"
        , "talk to woman"
        , "give cleaver to woman"
        , "give paw to woman"
        , "look at cupboard"
        , "get outfit"
        , "w"
        , "d"
        , "w"
        , "talk to monk"
        , "d"
        ]
        [ (==Left InstructionError) <$> gets (view lastInstructionState) ]

    it "can win" do
      checkPreds
        buildFourthGame
        [ "n"
        , "w"
        , "look at trees"
        , "get paw"
        , "talk to monk"
        , "say wooooo"
        , "wait"
        , "e"
        , "u"
        , "look at shelves"
        , "get book"
        , "s"
        , "break loop"
        , "talk to man"
        , "n"
        , "e"
        , "talk to woman"
        , "give cleaver to woman"
        , "give paw to woman"
        , "look at cupboard"
        , "get outfit"
        , "w"
        , "d"
        , "w"
        , "talk to monk"
        , "remove clothes"
        , "wear outfit"
        , "use pen on book"
        , "d"
        ]
        [ gets (view gameOver) ]

    it "gets model student" do
      checkPreds
        buildFourthGame
        [ "n"
        , "w"
        , "look at trees"
        , "get paw"
        , "talk to monk"
        , "say wooooo"
        , "wait"
        , "e"
        , "u"
        , "look at shelves"
        , "get book"
        , "s"
        , "break loop"
        , "talk to man"
        , "n"
        , "e"
        , "talk to woman"
        , "give cleaver to woman"
        , "give paw to woman"
        , "look at cupboard"
        , "get outfit"
        , "remove clothes"
        , "wear outfit"
        , "use pen on book"
        , "w"
        , "d"
        , "w"
        , "talk to monk"
        , "d"
        ]
        [ gets (view gameOver) ]

    it "has no delay if you win in the garden" do
      checkPreds
        buildFourthGame
        [ "n"
        , "w"
        , "look at trees"
        , "get paw"
        , "talk to monk"
        , "say wooooo"
        , "wait"
        , "e"
        , "u"
        , "look at shelves"
        , "get book"
        , "s"
        , "break loop"
        , "talk to man"
        , "n"
        , "e"
        , "talk to woman"
        , "give cleaver to woman"
        , "give paw to woman"
        , "look at cupboard"
        , "get outfit"
        , "w"
        , "d"
        , "w"
        , "talk to monk"
        , "remove clothes"
        , "wear outfit"
        , "use pen on book"
        , "d"
        ]
        [ gets (view gameOver) ]

    it "does a full cheev run" do
      checkPreds
        buildFourthGame
        [ "n"
        , "get item"
        , "talk to item"
        , "drink item"
        , "eat item"
        , "turn on item"
        , "turn off item"
        , "w"
        , "look at trees"
        , "get paw"
        , "talk to monk"
        , "say fuck whatever is on your mind"
        , "wait"
        , "e"
        , "u"
        , "look at shelves"
        , "get book"
        , "s"
        , "break loop"
        , "talk to man"
        , "n"
        , "e"
        , "talk to woman"
        , "use cleaver on woman"
        , "use cleaver on self"
        , "w"
        , "d"
        , "w"
        , "use cleaver on trees"
        , "use cleaver on monk"
        , "e"
        , "u"
        , "e"
        , "give cleaver to woman"
        , "give paw to woman"
        , "look at cupboard"
        , "get outfit"
        , "w"
        , "d"
        , "w"
        , "remove clothes"
        , "wear outfit"
        , "use pen on book"
        , "talk to monk"
        , "d"
        , "i"
        ]
        [ null <$> gets (view remainingAchievements) ]
