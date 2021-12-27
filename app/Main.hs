module Main where

import qualified HIF.Games.ExampleGame as EG
import qualified HIF.Games.FourthGame as FG
import HIF.Runner

main :: IO ()
--main = runWithBuilder EG.buildGame
main = runWithBuilder FG.buildGame
