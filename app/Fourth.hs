module Main where

import qualified HIF.Games.FourthGame as FG
import HIF.Runner

main :: IO ()
main = runWithBuilder FG.buildGame
