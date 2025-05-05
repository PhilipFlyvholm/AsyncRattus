{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import WidgetRattus
import WidgetRattus.Widgets
import WidgetRattus.Event
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

rewriteTests :: C VStack
rewriteTests = do
    -- Button
    counterBtn <- mkButton $ mkConstText "Increment"
    let counterEv = scan (box (\n _ -> n + 1 :: Int)) 0 $ btnOnClickEv counterBtn

    let filterMapResult = filter (box (> 5)) (WidgetRattus.Event.map (box (* 2)) counterEv)
    filterMapResultLabel <- mkLabel $ stepper 0 filterMapResult

    let mapFilterResult = WidgetRattus.Event.map (box (* 2)) (filter (box (> 5)) counterEv)
    mapFilterResultLabel <- mkLabel $ stepper 0 mapFilterResult

    let filterFilterResult = filter (box (< 10)) (filter (box (> 5)) counterEv)
    filterFilterResultLabel <- mkLabel $ stepper 0 filterFilterResult

    -- UI
    lbl <- mkLabel $ stepper 0 counterEv
    mkConstVStack $ lbl :* counterBtn :* filterMapResultLabel :* mapFilterResultLabel :* filterFilterResultLabel

main :: IO ()
main = runApplication rewriteTests