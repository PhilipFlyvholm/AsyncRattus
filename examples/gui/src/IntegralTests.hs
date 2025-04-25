{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Main where

import WidgetRattus
import WidgetRattus.Widgets
import WidgetRattus.Behaviour

import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

integralTests :: C VStack
integralTests = do
  time <- elapsedTime
  time' <- integral 0 () (WidgetRattus.Behaviour.map (box realToFrac) time)
  let shouldBe = WidgetRattus.Behaviour.map (box (\t -> fromRational ((toRational t)^2)/2)) time
  originalLbl <- mkLabel time
  resultLbl <- mkLabel (WidgetRattus.Behaviour.map (box toText) time')

  shouldLbl <- mkLabel (WidgetRattus.Behaviour.map (box toText) shouldBe)
  -- UI
  mkConstVStack $ originalLbl :* resultLbl :* shouldLbl

main :: IO ()
main = runApplication integralTests