module Main where

import qualified HIF.Games.ExampleGame as EG
import HIF.Runner

main :: IO ()
main = runWithBuilder EG.buildGame
