{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import WidgetRattus
import WidgetRattus.Signal (Sig ((:::)))
import WidgetRattus.Widgets
import WidgetRattus.Behaviour
import WidgetRattus.Event
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

elapsedTime' :: C (NominalDiffTime -> Beh NominalDiffTime)
elapsedTime' =
  do
    startTime <- time
    return (\f -> Beh (Fun () (box (\_ currentTime -> (f + diffTime currentTime startTime) :*  Just' ())) ::: never))

timerExample :: C VStack
timerExample = do
  -- Time
  startElapsedTime <- elapsedTime
  
  -- Buttons
  startBtn <- mkButton (mkConstText "Start")
  let startEv = btnOnClick startBtn
  stopBtn <- mkButton (mkConstText "Stop")
  let stopEv = btnOnClick stopBtn
  
  -- Start and stop WidgetRattus.Events
  let startTime :: Ev (NominalDiffTime -> Beh NominalDiffTime) =
        mkEv' (box (delay (let _ = adv (unbox startEv) in elapsedTime')))
  let stopTime :: Ev (NominalDiffTime -> Beh NominalDiffTime) =
        mkEv (box (delay (let _ = adv (unbox stopEv) in const . K)))

  let combinedInput = WidgetRattus.Event.interleave (box (\x _ -> x)) startTime stopTime
  let stopWatchSig = switchR (constK 0) combinedInput

  -- UI
  timeLabName <- mkLabel (mkConstText "Current Time:")
  swLabName <- mkLabel (mkConstText "Elapsed Time:")

  timeLab <- mkLabel startElapsedTime
  stopWatchLab <- mkLabel stopWatchSig
  
  time <- mkConstHStack (timeLabName :* timeLab)
  sw <- mkConstHStack (swLabName :* stopWatchLab)
  buttons <- mkConstHStack (startBtn :* stopBtn)
  mkConstVStack (time :* sw :* buttons)

main :: IO ()
main = runApplication timerExample