{-# LANGUAGE OverloadedStrings #-}

module Main where

import HIF.Runner
import qualified HIF.CovidGame as CG
import qualified HIF.FourthGame as FG
import qualified HIF.ExampleGame as EG

main :: IO ()
main = runWithBuilder EG.buildGame
