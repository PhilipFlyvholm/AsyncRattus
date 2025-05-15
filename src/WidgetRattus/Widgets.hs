{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module WidgetRattus.Widgets
  ( Displayable (..),
    IsWidget (..),
    Widgets (..),
    Widget,
    HStack,
    VStack,
    TextDropdown,
    tddCurr,
    tddEvent,
    tddList,
    Popup,
    popCurr,
    popEvent,
    popChild,
    Slider,
    sldCurr,
    sldEvent,
    sldMin,
    sldMax,
    Button,
    btnContent,
    btnClick,
    Label,
    labText,
    TextField,
    tfContent,
    tfInput,
    mkButton,
    mkTextField,
    setInputBehTF,
    mkLabel,
    mkHStack,
    mkConstHStack,
    mkVStack,
    mkConstVStack,
    mkTextDropdown,
    mkPopup,
    mkSlider,
    mkProgressBar,
    btnOnClick,
    btnOnClickEv,
    textFieldOnInput,
    runApplication,
    sliderOnChange,
    sliderCurrent,
    mkConstText
  )
where

import Control.Concurrent hiding (Chan)
import Data.IntSet as IntSet
import Data.Text
import qualified Monomer as M
import System.IO.Unsafe
import WidgetRattus
import WidgetRattus.Behaviour
import WidgetRattus.Event
import WidgetRattus.InternalPrimitives
import WidgetRattus.Signal
import WidgetRattus.Widgets.InternalTypes
import Prelude

-- The identity function.
instance Displayable Text where
  display x = x

-- Convert Int to Text via String.
instance Displayable Int where
  display x = toText x

instance Displayable Time where
  display = toText

instance Displayable NominalDiffTime where
  display = toText

-- Functions for constructing Async Rattus widgets.
mkButton :: (Displayable a) => Beh a -> C Button
mkButton t = do
  c <- chan
  t' <- discretize t
  return Button {btnContent = t', btnClick = c}

mkTextField :: Text -> C TextField
mkTextField txt = do
  c <- chan
  let (EvDense d) = mkEv (box (wait c))
  let beh = txt ::: d
  return TextField {tfContent = beh, tfInput = c}

mkLabel :: (Displayable a) => Beh a -> C Label
mkLabel t = do
  t' <- discretize t
  return Label {labText = t'}

class Widgets ws where
  toWidgetList :: ws -> List Widget

instance {-# OVERLAPPABLE #-} (IsWidget w) => Widgets w where
  toWidgetList w = [mkWidget w]

instance {-# OVERLAPPING #-} (Widgets w, Widgets v) => Widgets (w :* v) where
  toWidgetList (w :* v) = toWidgetList w +++ toWidgetList v

instance {-# OVERLAPPING #-} (Widgets w) => Widgets (List w) where
  toWidgetList w = concatMap' toWidgetList w

mkHStack :: (IsWidget a) => Beh (List a) -> C HStack
mkHStack wl = do
  wl' <- discretize wl
  return (HStack wl')

mkConstHStack :: (Widgets ws) => ws -> C HStack
mkConstHStack w = mkHStack (constK (toWidgetList w))

mkVStack :: (IsWidget a) => Beh (List a) -> C VStack
mkVStack wl = do

  wl' <- discretize wl
  return (VStack wl')

mkConstVStack :: (Widgets ws) => ws -> C VStack
mkConstVStack w = mkVStack (constK (toWidgetList w))

mkTextDropdown :: Beh (List Text) -> Text -> C TextDropdown
mkTextDropdown opts init = do
  c <- chan
  let beh = init ::: mkSig (box (wait c))
  opts' <- discretize opts
  return TextDropdown {tddCurr = beh, tddEvent = c, tddList = opts'}

mkPopup :: Ev Bool -> Beh Widget -> C Popup
mkPopup b w = do
  c <- chan
  w' <- discretize w
  let changeEvent = mkEv (box (wait c))
  visibility <- discretize (WidgetRattus.Event.stepper False $ WidgetRattus.Event.interleave (box Prelude.const) b changeEvent)
  return Popup {popCurr = visibility, popEvent = c, popChild = w'}

mkSlider :: Int -> Beh Int -> Beh Int -> C Slider
mkSlider start min max = do
  c <- chan
  let curr = start ::: mkSig (box (wait c))
  min' <- discretize min
  max' <- discretize max
  return Slider {sldCurr = curr, sldEvent = c, sldMin = min', sldMax = max'}

mkProgressBar :: Beh Int -> Beh Int -> Beh Int -> C Slider
mkProgressBar min max curr = do
  c <- chan
  boundedCurrent <- discretize $ WidgetRattus.Behaviour.zipWith (box Prelude.min) curr max
  min' <- discretize min
  max' <- discretize max
  return Slider {sldCurr = boundedCurrent, sldEvent = c, sldMin = min', sldMax = max'}

-- Helper function that takes a Button and returns a boxed delayed computation.
-- The delayed computation is defined from the buttons input channel.
btnOnClick :: Button -> Box (O ())
btnOnClick btn =
  let ch = btnClick btn
   in box (wait ch)

-- Function that constructs a delayed signal from a Button.
btnOnClickEv :: Button -> Ev ()
btnOnClickEv b = mkEv (btnOnClick b)

-- Creates a new textfield whose contents are determined by
-- the input signal.
-- Therefore user input will only be shown if the input signal
-- ticks in response to user input on the textfield.
-- Note: the input TF and output TF share an input channel
-- Hence if both are part of a GUI they will be written to simultaneously
setInputBehTF :: TextField -> Beh Text -> C TextField
setInputBehTF tf beh = do
  beh' <- discretize beh
  return $ tf {tfContent = beh'}

-- Helper function that takes a TextField and returns a boxed delayed computation.
-- The delayed computation is defined from the Textfields input channel.
textFieldOnInput :: TextField -> Ev Text
textFieldOnInput tf =
  let ch = tfInput tf
   in mkEv (box (wait ch))

sliderOnChange :: Slider -> Ev Int
sliderOnChange s =
  let ch = sldEvent s
  in mkEv (box (wait ch))

sliderCurrent :: Slider -> Beh Int
sliderCurrent s =
  let cur = sldCurr s
  in Beh (WidgetRattus.Signal.map (box (\a -> K a)) cur)
  

mkConstText :: String -> Beh Text
mkConstText s = constK (pack s)

-- Function which creates a timed event. Associated clock will be part of the AppModel.
mkTimerEvent :: Int -> (AppEvent -> IO ()) -> IO ()
mkTimerEvent n cb = (threadDelay n >> cb (AppEvent (Chan n) ())) >> return ()

-- runApplication takes as input a widget and starts the GUI applicaiton
-- by calling Monomer's startApp function.
{-# ANN runApplication AllowLazyData #-}
runApplication :: (IsWidget a) => C a -> IO ()
runApplication (C w) = do
  w' <- w (OneInput 0 ())
  -- s -> appEventHandler -> appUIBuilder -> appConfig -> IO
  M.startApp (AppModel w' emptyClock) handler builder config
  where
    builder _ (AppModel w _) =
      unsafePerformIO (do
          let (C node) = mkWidgetNode w
          node (OneInput 0 ()))
        `M.styleBasic` [M.padding 3]
    handler _ _ (AppModel w cl) (AppEvent (Chan ch) d) =
      let inp = OneInput ch d
       in unsafePerformIO $ do
            progressPromoteStoreAtomic inp
            let (w', cl') = progressAndNext inp w
            let activeTimers = if ch > 0 then IntSet.delete ch cl else cl
            let newTimers = IntSet.filter (> 0) cl' `IntSet.difference` activeTimers
            let timers = Prelude.map (M.Producer . mkTimerEvent) (IntSet.toList newTimers)
            return (M.Model (AppModel w' (newTimers `IntSet.union` activeTimers)) : M.Request M.RenderOnce : timers)
    config =
      [ M.appWindowTitle "GUI Application",
        M.appTheme M.lightTheme,
        M.appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        M.appInitEvent (AppEvent (Chan 1) ())
      ]
