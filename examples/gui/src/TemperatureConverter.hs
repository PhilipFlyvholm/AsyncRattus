{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}

import WidgetRattus
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text hiding (zipWith, filter, map, all)
import Data.Text.Read
import WidgetRattus.Widgets
import WidgetRattus.Behaviour
import WidgetRattus.Event

celsiusToFahrenheit :: Int -> Int
celsiusToFahrenheit t = t * 9 `div` 5 + 32

fahrenheitToCelsius :: Int -> Int
fahrenheitToCelsius t = (t - 32) * 5 `div` 9

isNumber :: Text -> Maybe' Int
isNumber "" = Just' 0
isNumber t =
    case signed decimal t of
        Right (t', "") -> Just' t'
        _ -> Nothing'

window :: C HStack
window = do
    -- TextFields
    tfF1 <- mkTextField "32"
    tfC1 <- mkTextField "0"

    -- Input WidgetRattus.Events
    let fEvent = WidgetRattus.Event.filterMap (box isNumber) (textFieldOnInput tfF1)
    let cEvent = WidgetRattus.Event.filterMap (box isNumber) (textFieldOnInput tfC1)

    -- ConvertWidgetRattus.Events
    let convertFtoC = WidgetRattus.Event.map (box fahrenheitToCelsius) fEvent
    let convertCtoF = WidgetRattus.Event.map (box celsiusToFahrenheit) cEvent

    -- Result WidgetRattus.Behaviour.
    let c = stepper 0 (interleave (box (\x _ -> x)) cEvent convertFtoC)
    let f = stepper 32 (interleave (box (\x _ -> x)) fEvent convertCtoF)

    -- Bind input to TextFields
    let tfF2 = setInputBehTF tfF1 (WidgetRattus.Behaviour.map (box toText) f)
    let tfC2 = setInputBehTF tfC1 (WidgetRattus.Behaviour.map (box toText) c)

    -- UI
    fLabel <- mkLabel $ mkConstText "Fahrenheit"
    cLabel <- mkLabel $ mkConstText "Celsius"

    fStack <- mkConstVStack (tfF2 :* fLabel)
    cStack <- mkConstVStack (tfC2 :* cLabel)
    mkConstHStack (fStack :* cStack)


main :: IO ()
main = runApplication window