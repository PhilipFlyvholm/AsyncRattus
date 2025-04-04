{-# LANGUAGE OverloadedLists #-}{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import WidgetRattus
import WidgetRattus.Widgets
import WidgetRattus.Behaviour
import WidgetRattus.Event

nums :: List Int
nums = [0 .. 9]

data Op = Plus | Minus | Equals | Reset

compute :: (Int :* Op :* Bool -> Maybe' (Int :* Op) -> Int :* Op :* Bool)
compute (n :* op     :* _)  Nothing'            = n :* op :* False
compute  _                 (Just' (_ :* Reset)) = 0 :* Reset :* True
compute (n :* Plus   :* _) (Just' (m :* op))    = (n + m) :* op :* True
compute (n :* Minus  :* _) (Just' (m :* op))    = (n - m) :* op :* True
compute (_ :* Equals :* _) (Just' (m :* op))    = m :* op :* True
compute (_ :* Reset  :* _) (Just' (m :* op))    = m :* op :* True

calculatorExample :: C VStack
calculatorExample = do
  -- Buttons
  numBtns :: List Button <-
    mapM (mkButton . constK) nums

  let [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9] = numBtns
  resetBut <- mkButton (mkConstText "C")
  addBut <- mkButton (mkConstText "+")
  subBut <- mkButton (mkConstText "-")
  eqBut <- mkButton (mkConstText "=")

  -- WidgetRattus.Event to construct numbers
  let numClicks :: List (Ev (Int -> Int)) =
        zipWith' (\b n -> WidgetRattus.Event.map (box (\_ x -> x * 10 + n)) (btnOnClickEv b)) numBtns nums

  -- WidgetRattus.Event to reset the current number to 0, after clicking an operator button
  let resetEv :: Ev (Int -> Int) =
        WidgetRattus.Event.map (box (\_ _ -> 0)) $
          interleaveAll (box (\a _ -> a)) $
            map' btnOnClickEv [addBut, subBut, eqBut, resetBut]

  -- Combine WidgetRattus.Events to construct the number WidgetRattus.Event
  let evList = resetEv :! numClicks :: List (Ev (Int -> Int))
  let combinedEvs = interleaveAll (box (\a _ -> a)) evList

  -- Number WidgetRattus.Event (i.e. the multidigit number that has been constructed)
  let numberEv :: Ev Int =
        scan (box (\a f -> f a)) 0 combinedEvs

  -- Operator WidgetRattus.Event
  let opEv :: Ev Op =
          interleaveAll (box (\a _ -> a)) $
            map'
              (\(op :* btn) -> WidgetRattus.Event.map (box (\_ -> op)) (btnOnClickEv btn))
              [Plus :* addBut, Minus :* subBut, Equals :* eqBut, Reset :* resetBut]

  -- WidgetRattus.Event consisting of an operand (i.e. a number) @n@ and an
  -- operator @op@. @n@ is the value of @numberEv@ just before
  -- clicking an operator button, and op is taken from opEv
  let operand :: Ev (Maybe' (Int :* Op)) =
        triggerM (box (\op n -> Just' (n :* op))) opEv (stepper 0 (buffer 0 numberEv))

  -- The result WidgetRattus.Event consisting of a number n that is the result
  -- of the current computation, an operator op that still needs to
  -- applied to n and a Boolean b that indicates whether we have
  -- just calculated n (and thus n should be displayed)
  let resultEv :: Ev (Int :* Op :* Bool) =
        scan (box compute) (0 :* Plus :* True) operand

  -- -- The WidgetRattus.Behaviour.that should be displayed
  let displayBeh :: Beh Int =
        WidgetRattus.Behaviour.zipWith (box (\(n :* _ :* b) m -> if b then n else m)) (WidgetRattus.Event.stepper (0 :* Plus :* False) resultEv) (WidgetRattus.Event.stepper 0 numberEv)

  -- UI
  result <- mkLabel displayBeh
  operators <- mkConstVStack (resetBut :* addBut :* subBut :* eqBut)
  row1 <- mkConstHStack (b7 :* b8 :* b9)
  row2 <- mkConstHStack (b4 :* b5 :* b6)
  row3 <- mkConstHStack (b1 :* b2 :* b3)

  numbers <- mkConstVStack (row1 :* row2 :* row3 :* b0)

  input <- mkConstHStack (numbers :* operators)
  mkConstVStack (result :* input)

main :: IO ()
main = runApplication calculatorExample