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
import WidgetRattus.Signal

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
  mkWidgetNode Button {btnContent = Beh ((txt ::: _)), btnClick = click} t =
    let txt' = apply txt t
    in M.button (display txt') (AppEvent click ())

instance IsWidget TextField where
  mkWidgetNode TextField {tfContent = Beh (txt ::: _), tfInput = inp} t =
    let txt' = apply txt t
    in M.textFieldV txt' (AppEvent inp)

instance IsWidget Label where
  mkWidgetNode Label {labText = Beh ((txt ::: _))} t =
    let txt' = apply txt t
    in M.label (display txt')


instance IsWidget HStack where
      mkWidgetNode (HStack (Beh (cur:::_))) t =
        let cur' = apply cur t
            children = fmap (\x -> mkWidgetNode x t) cur'
        in M.hstack_ [ M.childSpacing_ 2] (reverse' children)

instance IsWidget VStack where
      mkWidgetNode (VStack (Beh (cur:::_))) t =
        let cur' = apply cur t
            children = fmap (\x -> mkWidgetNode x t) cur'
        in M.vstack_ [ M.childSpacing_ 2] (reverse' children)

instance IsWidget TextDropdown where
  mkWidgetNode TextDropdown {tddList = Beh(opts ::: _), tddCurr = Beh (curr ::: _), tddEvent = ch} t = 
      let opts' = apply opts t
          curr' = apply curr t
      in M.textDropdownV curr' (AppEvent ch) opts'

instance IsWidget Popup where
  mkWidgetNode Popup {popCurr = Beh (curr ::: _), popEvent = ch, popChild = Beh (child ::: _)} t =
      let curr' = apply curr t
          child' = apply child t
          childNode = mkWidgetNode child' t
      in M.popupV curr' (AppEvent ch) childNode

instance IsWidget Slider where
  mkWidgetNode Slider {sldCurr = Beh (curr ::: _), sldEvent = ch, sldMin = Beh (min ::: _), sldMax = Beh (max ::: _)} t =
      let curr' = apply curr t
          min' = apply min t
          max' = apply max t
      in M.hsliderV curr' (AppEvent ch) min' max'

instance IsWidget Widget where
  mkWidgetNode (Widget w (Beh (e ::: _))) t = 
    let e' = apply e t
        child = mkWidgetNode w t
    in M.nodeEnabled child e'

  mkWidget w = w

  setEnabled (Widget w _) es = Widget w es