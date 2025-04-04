{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import WidgetRattus
import WidgetRattus.Widgets
import WidgetRattus.Behaviour
import WidgetRattus.Event
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

nominalToInt :: NominalDiffTime -> Int
nominalToInt x = floor $ toRational x

intToNominal :: Int -> NominalDiffTime
intToNominal x = fromInteger (toInteger x)

timeFrom :: Int -> NominalDiffTime -> C (Beh (NominalDiffTime :* Int))
timeFrom max d = do
      timeBeh <- elapsedTime
      let addTime = WidgetRattus.Behaviour.map (box (\t -> t+d :* max)) timeBeh
      return $ stopWith (box (\(a :* _) -> if nominalToInt a >= max then Just' (intToNominal max :* max) else Nothing')) addTime

timerExample :: C VStack
timerExample = do
      let initialMax = 5
      elapsedTime <- timeFrom initialMax 0

      -- Slider
      maxSlider <- mkSlider initialMax (constK 1) (constK 100)
      let maxBeh = sldCurr maxSlider
      let maxChangeEv = sliderOnChange maxSlider

      -- Reset button
      resetBtn <- mkButton $ mkConstText "Reset timer"
      let resetTrigger = btnOnClickEv resetBtn

      -- Input WidgetRattus.Events
      let resetEv :: Ev(NominalDiffTime :* Int -> C (Beh (NominalDiffTime :* Int)))
            = WidgetRattus.Event.map (box (\ _ (_ :* max) -> timeFrom max 0)) resetTrigger

      let maxEv :: (Ev(NominalDiffTime :* Int -> C (Beh (NominalDiffTime :* Int))))
            = WidgetRattus.Event.map (box (\newMax (currentTime :* _) -> timeFrom newMax currentTime)) maxChangeEv

      let combinedInput = WidgetRattus.Event.interleave (box (\_ m -> m)) resetEv maxEv

      let timer = switchR' elapsedTime combinedInput

      -- UI
      text <- mkLabel (WidgetRattus.Behaviour.map (box (\(t :* _) -> "Current: " <> toText (nominalToInt t))) timer)
      maxText <- mkLabel (WidgetRattus.Behaviour.map (box (\max -> "Max: " <> toText max)) maxBeh)
      mkConstVStack $ maxSlider :* maxText:* text :* resetBtn 


main :: IO ()
main = runApplication timerExample