{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Widgets
import WidgetRattus
import Behaviour
import qualified Event
import Event (switchR)

nominalToInt :: NominalDiffTime -> Int
nominalToInt x = floor $ toRational x

intToNominal :: Int -> NominalDiffTime
intToNominal x = fromInteger (toInteger x)

timeFrom :: Int -> Time -> Beh NominalDiffTime
timeFrom max startTime =
  stop (box (\t -> t > intToNominal max)) (Behaviour.map (box (`diffTime` startTime)) timeBehaviour)


window :: C VStack'
window = do
  let initialMax = 5
  -- Reset button
  resetBtn <- mkButton' $ mkConstText "Reset timer"
  let resetTrigger = btnOnClickEv resetBtn

  -- Slider
  maxSlider <- mkSlider' initialMax (constK 1) (constK 100)
  let maxBeh = sldCurr maxSlider
  let maxChangeEv = Event.map (box (Prelude.const ())) $ sliderOnChange maxSlider

  startTime <- time
  let timeWithMax = Behaviour.zipWith (box (:*)) timeBehaviour maxBeh
  let timer = Event.trigger (box (\_ (t :* max) _ -> timeFrom max t)) (Event.interleave (box (\_ _ -> ())) resetTrigger maxChangeEv) timeWithMax

  let timer' = switchR (timeFrom initialMax startTime) timer

  text <- mkLabel' (Behaviour.map (box (\t -> "Current: " <> toText (nominalToInt t))) timer')
  maxText <- mkLabel' (Behaviour.map (box (\max -> "Max: " <> toText max)) maxBeh)
  mkConstVStack' $ maxSlider :* maxText :* text :* resetBtn

main :: IO ()
main = runApplication' window