module Main where

import HIF.Runner
import qualified HIF.Games.ExampleGame as EG

main :: IO ()
main = runWithBuilder EG.buildGame
