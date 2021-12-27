module Main where

import qualified HIF.Games.CovidGame as CG
import HIF.Runner

main :: IO ()
main = runWithBuilder CG.buildGame
