{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module WidgetRattus.Event
  ( Ev (..),
    map,
    stepper,
    trigger,
    triggerM,
    interleave,
    interleaveAll,
    scan,
    filterMap,
    filter,
    switchS,
    switchSM,
    switchR,
    buffer,
    mkEv,
    mkEv',
  )
where

import Data.IntMap ()
import WidgetRattus
import WidgetRattus.Behaviour hiding (map)
import WidgetRattus.Signal hiding (buffer, interleave, interleaveAll, map, scan, switchR, switchS, trigger, triggerM)
import Prelude hiding (filter, map)

data Ev a
  = EvDense !(O (Sig a))
  | EvSparse !(O (Sig (Maybe' a)))

unwrapEv :: Ev a -> O (Sig a)
unwrapEv (EvDense e) = e

mkEv :: Box (O a) -> Ev a
mkEv a =
  EvDense $ delay (adv (unbox a) ::: unwrapEv (mkEv a))

mkEv' :: Box (O (C a)) -> Ev a
mkEv' b =
  EvDense
    ( delayC
        ( delay
            ( do
                a <- adv (unbox b)
                return (a ::: unwrapEv (mkEv' b))
            )
        )
    )

mapMaybe :: Box (a -> b) -> Maybe' a -> Maybe' b
mapMaybe f (Just' a) =
  Just' $ unbox f a
mapMaybe _ Nothing' = Nothing'

map :: Box (a -> b) -> Ev a -> Ev b
map f (EvDense sig) =
  EvDense
    ( delay
        ( let (x ::: xs) = adv sig
           in unbox f x
                ::: unwrapEv (WidgetRattus.Event.map f (EvDense xs))
        )
    )
map f (EvSparse sig) =
  EvSparse
    ( delay
        ( let (x ::: xs) = adv sig
           in mapMaybe f x
                ::: let (EvSparse rest) = WidgetRattus.Event.map f (EvSparse xs)
                     in rest
        )
    )

stepper :: (Stable a) => a -> Ev a -> Beh a
stepper initial event =
  Beh (K initial ::: delay (adv (aux initial event)))
  where
    aux :: (Stable a) => a -> Ev a -> O (Sig (Fun a))
    aux _ (EvDense ev) =
      delay (let (x ::: xs) = adv ev in K x ::: delay (adv (aux x (EvDense xs))))
    aux initial (EvSparse ev) =
      delay
        ( let (x ::: xs) = adv ev
           in case x of
                Just' x' ->
                  K x' ::: delay (adv (aux x' (EvSparse xs)))
                Nothing' -> K initial ::: delay (adv (aux initial (EvSparse xs)))
        )

trigger :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> Ev c
trigger f event behaviour = EvSparse (trig f event behaviour)
  where
    trig :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> O (Sig (Maybe' c))
    trig f' (EvSparse as) (Beh (b ::: bs)) =
      delayC $
        delay
          ( let choice = select as bs
             in ( do
                    t <- time
                    return
                      ( case choice of
                          Fst (a' ::: as') bs' ->
                            let rest = trig f' (EvSparse as') (Beh (b ::: bs'))
                             in case a' of
                                  Just' a'' -> Just' (unbox f' a'' (apply b t)) ::: rest
                                  Nothing' -> Nothing' ::: rest
                          Snd as' bs' -> Nothing' ::: trig f' (EvSparse as') (Beh bs')
                          Both (a' ::: as') (b' ::: bs') ->
                            let rest = trig f' (EvSparse as') (Beh (b' ::: bs'))
                             in case a' of
                                  Just' a'' -> Just' (unbox f' a'' (apply b' t)) ::: rest
                                  Nothing' -> Nothing' ::: rest
                      )
                )
          )
    trig f' (EvDense as) (Beh (b ::: bs)) =
      delayC $
        delay
          ( let d = select as bs
             in ( do
                    t <- time
                    return
                      ( case d of
                          Fst (a' ::: as') bs' -> Just' (unbox f' a' (apply b t)) ::: trig f' (EvDense as') (Beh (b ::: bs'))
                          Snd as' bs' -> Nothing' ::: trig f' (EvDense as') (Beh bs')
                          Both (a' ::: as') (b' ::: bs') -> Just' (unbox f' a' (apply b' t)) ::: trig f' (EvDense as') (Beh (b' ::: bs'))
                      )
                )
          )

triggerM :: (Stable b) => Box (a -> b -> Maybe' c) -> Ev a -> Beh b -> Ev (Maybe' c)
triggerM f event behaviour = EvDense (trig f event behaviour)
  where
    trig :: (Stable b) => Box (a -> b -> Maybe' c) -> Ev a -> Beh b -> O (Sig (Maybe' c))
    trig f' (EvDense as) (Beh (b ::: bs)) =
      delayC $
        delay
          ( let d = select as bs
             in ( do
                    t <- time
                    return
                      ( case d of
                          Fst (a' ::: as') bs' -> unbox f' a' (apply b t) ::: trig f' (EvDense as') (Beh (b ::: bs'))
                          Snd as' bs' -> Nothing' ::: trig f' (EvDense as') (Beh bs')
                          Both (a' ::: as') (b' ::: bs') -> unbox f' a' (apply b' t) ::: trig f' (EvDense as') (Beh (b' ::: bs'))
                      )
                )
          )
    trig f' (EvSparse as) (Beh (b ::: bs)) =
      delayC $
        delay
          ( let d = select as bs
             in ( do
                    t <- time
                    return
                      ( case d of
                          Fst (Just' a' ::: as') bs' -> unbox f' a' (apply b t) ::: trig f' (EvSparse as') (Beh (b ::: bs'))
                          Fst (Nothing' ::: as') bs' -> Nothing' ::: trig f' (EvSparse as') (Beh (b ::: bs'))
                          Snd as' bs' -> Nothing' ::: trig f' (EvSparse as') (Beh bs')
                          Both (Just' a' ::: as') (b' ::: bs') -> unbox f' a' (apply b' t) ::: trig f' (EvSparse as') (Beh (b' ::: bs'))
                          Both (Nothing' ::: as') (b' ::: bs') -> Nothing' ::: trig f' (EvSparse as') (Beh (b' ::: bs'))
                      )
                )
          )

denseToSparse :: O (Sig a) -> O (Sig (Maybe' a))
denseToSparse ev =
  delay
    ( let (x ::: xs) = adv ev
       in Just' x ::: denseToSparse xs
    )

{-# ANN interleave AllowRecursion #-}
interleave :: Box (a -> a -> a) -> Ev a -> Ev a -> Ev a
interleave f (EvDense xs) (EvDense ys) = EvDense (aux f xs ys)
  where
    aux f xs ys =
      delay
        ( case select xs ys of
          Fst (x ::: xs') ys' -> (x ::: aux f xs' ys')
          Snd xs' (y ::: ys') -> (y ::: aux f xs' ys')
          Both (x ::: xs') (y ::: ys') -> unbox f x y ::: aux f xs' ys'
      )
interleave f (EvSparse xs) (EvSparse ys) = EvSparse (aux f xs ys)
  where
    aux f xs ys =
      delay
        ( case select xs ys of
          Fst (x ::: xs') ys' -> (x ::: aux f xs' ys')
          Snd xs' (y ::: ys') -> (y ::: aux f xs' ys')
          Both (Just' x ::: xs') (Just' y ::: ys') -> Just' (unbox f x y) ::: aux f xs' ys'
          Both (_ ::: xs') (_ ::: ys') -> Nothing' ::: aux f xs' ys'
      )
interleave f (EvSparse xs) (EvDense ys) = interleave f (EvSparse xs) (EvSparse (denseToSparse ys))
interleave f (EvDense xs) (EvSparse ys) = interleave f (EvSparse (denseToSparse xs)) (EvSparse ys)

{-# ANN interleaveAll AllowRecursion #-}
interleaveAll :: Box (a -> a -> a) -> List (Ev a) -> Ev a
interleaveAll _ Nil = error "interleaveAll: List must be nonempty"
interleaveAll _ [s] = s
interleaveAll f (x :! xs) = interleave f x (interleaveAll f xs)

scan :: (Stable b) => Box (b -> a -> b) -> b -> Ev a -> Ev b
scan f acc (EvDense ev) =
  EvDense $
    delay
      ( let (x ::: xs) = adv ev
            acc' = unbox f acc x
            EvDense rest = scan f acc' (EvDense xs)
         in acc' ::: rest
      )
scan f acc (EvSparse ev) =
  EvSparse $
    delay
      ( let (x ::: xs) = adv ev
         in case x of
              Just' x' ->
                let acc' = unbox f acc x'
                    EvSparse rest = scan f acc' (EvSparse xs)
                 in Just' acc' ::: rest
              Nothing' ->
                let EvSparse rest = scan f acc (EvSparse xs)
                 in Just' acc ::: rest
      )

filterMap :: Box (a -> Maybe' b) -> Ev a -> Ev b
filterMap f (EvDense ev) =
  EvSparse
    ( delay
        ( let (x ::: xs) = adv ev
              (EvSparse rest) = filterMap f (EvDense xs)
           in unbox f x ::: rest
        )
    )
filterMap f (EvSparse ev) =
  EvSparse
    ( delay
        ( let (x ::: xs) = adv ev
              (EvSparse rest) = filterMap f (EvSparse xs)
           in case x of
                Just' x' -> unbox f x' ::: rest
                Nothing' -> Nothing' ::: rest
        )
    )

filter :: Box (a -> Bool) -> Ev a -> Ev a
filter f = filterMap (box (\x -> if unbox f x then Just' x else Nothing'))

switchS :: (Stable a) => Beh a -> O (a -> Beh a) -> Beh a
switchS (Beh (x ::: xs)) d =
  let rest =
        delayC
          ( delay
              ( let ticker = select xs d
                 in do
                      t <- time
                      return
                        ( case ticker of
                            Fst xs' d' -> unwrap $ switchS (Beh xs') d'
                            Snd _ f -> unwrap $ f (apply x t)
                            Both _ f -> unwrap $ f (apply x t)
                        )
              )
          )
   in Beh (x ::: rest)

switchSM :: (Stable a) => Beh a -> O (Maybe' (a -> Beh a)) -> Beh a
switchSM (Beh (x ::: xs)) d =
  let rest =
        delayC
          ( delay
              ( let ticker = select xs d
                 in do
                      t <- time
                      return
                        ( case ticker of
                            Fst xs' d' -> unwrap $ switchSM (Beh xs') d'
                            Snd _ (Just' f) -> unwrap $ f (apply x t)
                            Snd xs' Nothing' -> x ::: xs'
                            Both _ (Just' f) -> unwrap $ f (apply x t)
                            Both xs' Nothing' -> xs'
                        )
              )
          )
   in Beh (x ::: rest)

switchR :: (Stable a) => Beh a -> Ev (a -> Beh a) -> Beh a
switchR beh (EvDense steps) =
  switchS beh (delay (let step ::: steps' = adv steps in (\x -> switchR (step x) (EvDense steps'))))
switchR beh (EvSparse steps) =
  switchSM
    beh
    ( delay
        ( let step ::: steps' = adv steps
           in case step of
                Just' a -> Just' (\x -> switchR (a x) (EvSparse steps'))
                Nothing' -> Nothing'
        )
    )

buffer :: (Stable a) => a -> Ev a -> Ev a
buffer x (EvDense ys) = EvDense (delay (let (y ::: ys') = adv ys in (x ::: let (EvDense rest) = buffer y (EvDense ys') in rest)))
buffer x (EvSparse ys) =
  EvDense
    ( delay
        ( let (y ::: ys') = adv ys
           in case y of
                Just' y' -> x ::: let (EvDense rest) = buffer y' (EvSparse ys') in rest
                Nothing' -> x ::: let (EvDense rest) = buffer x (EvSparse ys') in rest
        )
    )

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

{-# NOINLINE [1] map #-}

{-# NOINLINE [1] filter #-}

{-# RULES
"ev.map/ev.map" forall f g xs.
  map f (map g xs) =
    map (box (unbox f . unbox g)) xs
"ev.map/ev.filter" forall f g xs.
  map f (filter g xs) =
    filterMap (box (\x -> if unbox g x then Just' (unbox f x) else Nothing')) xs
"ev.filter/ev.map" forall f g xs.
  filter f (map g xs) =
    filterMap (box (\x -> if (unbox f . unbox g) x then Just' $ unbox g x else Nothing')) xs
"ev.filter/ev.filter" forall f g xs.
  filter f (filter g xs) =
    filterMap (box (\x -> if unbox f x && unbox g x then Just' x else Nothing')) xs
  #-}
