{-# LANGUAGE GADTs #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module WidgetRattus.Widgets.InternalTypes where

import Data.Text
import qualified Monomer as M
import WidgetRattus
import WidgetRattus.Behaviour
import WidgetRattus.InternalPrimitives

{-# ANN module AllowLazyData #-}

-- The Displayable typeclass is used to define the display function.
-- The display function is used to convert a datatype to Text.
class (Stable a) => Displayable a where
  display :: a -> Text

-- The AppModel datatype used to contain the Widget passed to runApplication.
-- The associated clock is a set of timers.
-- Any timers created with mkTimerEvent will be added to the clock.
data AppModel where
  AppModel :: (IsWidget a) => !a -> !Clock -> AppModel

instance (Eq AppModel) where
  _ == _ = False

-- AppEvent data type used to convert channels into events.
data AppEvent where
  AppEvent :: !(Chan a) -> !a -> AppEvent

-- The IsWidget typeclass is used to define the mkWidgetNode function.
class (Continuous a) => IsWidget a where
  mkWidgetNode :: a -> Time -> (M.WidgetNode AppModel AppEvent)

  mkWidget :: a -> Widget
  mkWidget w = Widget w (constK True)

  setEnabled :: a -> Beh Bool -> Widget
  setEnabled = Widget

-- Custom data types for widgets.
data Widget where
  Widget :: (IsWidget a) => !a -> !(Beh Bool) -> Widget

data HStack where
  HStack :: (IsWidget a) => !(Beh (List a)) -> HStack

data VStack where
  VStack :: (IsWidget a) => !(Beh (List a)) -> VStack

data TextDropdown = TextDropdown {tddCurr :: !(Beh Text), tddEvent :: !(Chan Text), tddList :: !(Beh (List Text))}

data Popup = Popup {popCurr :: !(Beh Bool), popEvent :: !(Chan Bool), popChild :: !(Beh Widget)}

data Slider = Slider {sldCurr :: !(Beh Int), sldEvent :: !(Chan Int), sldMin :: !(Beh Int), sldMax :: !(Beh Int)}

data Button where
  Button :: (Displayable a) => {btnContent :: !(Beh a), btnClick :: !(Chan ())} -> Button

data Label where
  Label :: (Displayable a) => {labText :: !(Beh a)} -> Label

data TextField = TextField {tfContent :: !(Beh Text), tfInput :: !(Chan Text)}

-- Template Haskell code for generating instances of Continous.
continuous ''Button
continuous ''TextField
continuous ''Label
continuous ''Widget
continuous ''HStack
continuous ''VStack
continuous ''TextDropdown
continuous ''Popup
continuous ''Slider

-- isWidget Instance declerations for Widgets.
-- Here widgget data types are passed to Monomer constructors.
instance IsWidget Button where
  mkWidgetNode Button {btnContent = txt, btnClick = click} t =
    let txt' = current txt t
    in M.button (display txt') (AppEvent click ())

instance IsWidget TextField where
  mkWidgetNode TextField {tfContent = txt, tfInput = inp} t =
    let txt' = current txt t
    in M.textFieldV txt' (AppEvent inp)

instance IsWidget Label where
  mkWidgetNode Label {labText = txt} t =
    let txt' = current txt t
    in M.label (display txt')


instance IsWidget HStack where
      mkWidgetNode (HStack cur) t =
        let cur' = current cur t
            children = fmap (\x -> mkWidgetNode x t) cur'
        in M.hstack_ [ M.childSpacing_ 2] (reverse' children)

instance IsWidget VStack where
      mkWidgetNode (VStack cur) t =
        let cur' = current cur t
            children = fmap (\x -> mkWidgetNode x t) cur'
        in M.vstack_ [ M.childSpacing_ 2] (reverse' children)

instance IsWidget TextDropdown where
  mkWidgetNode TextDropdown {tddList = opts, tddCurr = curr, tddEvent = ch} t = 
      let opts' = current opts t
          curr' = current curr t
      in M.textDropdownV curr' (AppEvent ch) opts'

instance IsWidget Popup where
  mkWidgetNode Popup {popCurr = curr, popEvent = ch, popChild = child} t =
      let curr' = current curr t
          child' = current child t
          childNode = mkWidgetNode child' t
      in M.popupV curr' (AppEvent ch) childNode

instance IsWidget Slider where
  mkWidgetNode Slider {sldCurr = curr, sldEvent = ch, sldMin = min, sldMax = max} t =
      let curr' = current curr t
          min' = current min t
          max' = current max t
      in M.hsliderV curr' (AppEvent ch) min' max'

instance IsWidget Widget where
  mkWidgetNode (Widget w b) t = 
    let 
        e = current b t
        child = mkWidgetNode w t
    in M.nodeEnabled child e

  mkWidget w = w

  setEnabled (Widget w _) es = Widget w es