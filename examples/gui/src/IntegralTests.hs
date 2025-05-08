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
  let time = elapsedTime
  let time' = integral 0 (WidgetRattus.Behaviour.map (box realToFrac) time)
  let derivativeTime = derivative time'

  let shouldBe = WidgetRattus.Behaviour.map (box (\t -> fromRational ((toRational t)^2)/2)) time
  originalLbl <- mkLabel (WidgetRattus.Behaviour.map (box (\t -> "Input (Time):               " <> toText t)) time)
  resultLbl <- mkLabel (WidgetRattus.Behaviour.map (box (\t -> "Result (Integral):        " <> toText t)) time')
  shouldLbl <- mkLabel (WidgetRattus.Behaviour.map (box (\t -> "Should be:                    " <> toText t)) shouldBe)
  
  -- Re-derivative
  derivativeLbl <- mkLabel (WidgetRattus.Behaviour.map (box (\t -> "Integral -> Derivative: " <> toText t)) derivativeTime)
  -- UI
  mkConstVStack $ originalLbl :* resultLbl :* shouldLbl :* derivativeLbl

main :: IO ()
main = runApplication integralTests