{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import WidgetRattus.Widgets
import WidgetRattus
import WidgetRattus.Behaviour
import WidgetRattus.Event

nominalToInt :: NominalDiffTime -> Int
nominalToInt x = floor $ toRational x

intToNominal :: Int -> NominalDiffTime
intToNominal x = fromInteger (toInteger x)

timeFrom :: Int -> Time -> Beh NominalDiffTime
timeFrom max startTime =
  stopWith (box (\t -> if t >= intToNominal max then Just' (intToNominal max) else Nothing')) (WidgetRattus.Behaviour.map (box (`diffTime` startTime)) timeBehaviour)


window :: C VStack
window = do
  let initialMax = 5
  -- Reset button
  resetBtn <- mkButton $ mkConstText "Reset timer"
  let resetTrigger = btnOnClickEv resetBtn

  -- Slider
  maxSlider <- mkSlider initialMax (constK 1) (constK 100)
  let maxBeh = sliderCurrent maxSlider
  let maxChangeEv = WidgetRattus.Event.map (box (Prelude.const ())) $ sliderOnChange maxSlider

  startTime <- time
  let timeWithMax = WidgetRattus.Behaviour.zipWith (box (:*)) timeBehaviour maxBeh
  let timer = WidgetRattus.Event.trigger (box (\_ (t :* max) _ -> timeFrom max t)) (WidgetRattus.Event.interleave (box (\_ _ -> ())) resetTrigger maxChangeEv) timeWithMax

  let timer' = switchR (timeFrom initialMax startTime) timer

  text <- mkLabel (WidgetRattus.Behaviour.map (box (\t -> "Current: " <> toText (nominalToInt t))) timer')
  maxText <- mkLabel (WidgetRattus.Behaviour.map (box (\max -> "Max: " <> toText max)) maxBeh)
  mkConstVStack $ maxSlider :* maxText :* text :* resetBtn

main :: IO ()
main = runApplication window