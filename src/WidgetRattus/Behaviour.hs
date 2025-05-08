{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module WidgetRattus.Behaviour where

-- ( Beh (..),
--   Fun (..),
--   apply,
--   WidgetRattus.Behaviour.map,
--   mapF,
--   unwrap,
--   const,
--   constK,
--   timeBehaviour,
--   sampleInterval,
--   discretize,
--   elapsedTime,
--   withTime,
--   switch,
--   zipWith,
--   zipWith3,
--   stop,
--   stopWith,
--   integral,
--   derivative,
-- )

import WidgetRattus
import WidgetRattus.InternalPrimitives (Continuous (..), InputValue (OneInput), O (Delay), adv', advC', clockUnion, inputInClock)
import WidgetRattus.Signal hiding (const, derivative, integral, stop, switch, zipWith, zipWith3)
import Prelude hiding (const, map, zipWith, zipWith3)

data Fun a where
  K :: !a -> Fun a
  Fun :: (Stable s) => !s -> !(Box (s -> Time -> (a :* Maybe' s))) -> Fun a

continuous ''Fun

apply :: Fun a -> (Time -> a)
apply (K a) = \_ -> a
apply (Fun s f) = \t -> let (a :* _) = unbox f s t in a

mapF :: Box (a -> b) -> Fun a -> Fun b
mapF f (K a) = K (unbox f a)
mapF f (Fun s f') = Fun s (box (\s t -> let (a :* s') = unbox f' s t in (unbox f a :* s')))

type SBeh a = Sig (Fun a)
instance Stable (SBeh a)

newtype Beh a = Beh (Time -> SBeh a)

unwrap :: Beh a -> Time -> Sig (Fun a)
unwrap (Beh a) t = a t

const :: Fun a -> Beh a
const x = Beh (\_ -> x ::: never)

constK :: a -> Beh a
constK x = Beh (\_ -> K x ::: never)

current :: Beh a -> Time -> a
current (Beh as) t =
  let (x ::: xs) = as t
   in apply x t

future :: Beh a -> Time -> O (SBeh a)
future (Beh as) t =
  let (_ ::: xs) = as t in xs

timeBehaviour :: Beh Time
timeBehaviour = const (Fun () (box (\s t -> t :* Just' s)))

map :: Box (a -> b) -> Beh a -> Beh b
map f (Beh as) =
  Beh (\t -> run (as t))
  where
    run (x ::: xs) =
      mapF f x ::: delay (run (adv xs))

sampleInterval :: O ()
sampleInterval = timer 20000

discretize :: Beh a -> C (Sig a)
discretize (Beh as) = do
  t <- time
  let sBeh = as t
  run sBeh
  where
    run :: SBeh a -> C (Sig a)
    run (K x ::: xs) = do
      let rest = delayC $ delay (let x' = adv xs in run x')
      return $ x ::: rest
    run (Fun s f ::: xs) = discretizeFun s f xs
      where
        discretizeFun :: (Stable s) => s -> Box (s -> Time -> (a :* Maybe' s)) -> O (Sig (Fun a)) -> C (Sig a)
        discretizeFun s f xs = do
          t <- time
          let (cur :* s') = unbox f s t

          let rest =
                case s' of
                  Just' s'' ->
                    delayC $
                      delay
                        ( case select xs sampleInterval of
                            Fst x _ -> run x
                            Snd beh' _ -> run (Fun s'' f ::: beh')
                            Both x _ -> run x
                        )
                  Nothing' -> delayC $ delay (let sig = adv xs in run sig)

          return (cur ::: rest)

elapsedTime :: Beh NominalDiffTime
elapsedTime = Beh (\startTime -> Fun () (box (\s currentTime -> diffTime currentTime startTime :* Just' s)) ::: never)

withTime :: O (Time -> a) -> O a
withTime delayed =
  delayC $ delay (let f = adv delayed in do f <$> time)

switch :: Beh a -> O (Beh a) -> Beh a
switch (Beh as) d =
  Beh (\t -> run (as t) (withTime (delay (\t -> let (Beh d') = (adv d) in d' t))))
  where
    run :: SBeh a -> O (SBeh a) -> SBeh a
    run (x ::: xs) d =
      x
        ::: ( delay
                ( case select xs d of
                    Fst xs' d' -> run xs' d'
                    Snd _ d' -> d'
                    Both _ d' -> d'
                )
            )

-- -- | This function is a variant of combines the values of two signals
-- -- using the function argument. @zipWith f xs ys@ produces a new value
-- -- @unbox f x y@ whenever @xs@ or @ys@ produce a new value, where @x@
-- -- and @y@ are the current values of @xs@ and @ys@, respectively.
-- --
-- -- Example:
-- --
-- -- >                      xs:  1 2 3     2
-- -- >                      ys:  1     0 5 2
-- -- > zipWith (box (+)) xs ys:  2 3 4 3 8 4
zipWith :: forall a b c. (Stable a, Stable b) => Box (a -> b -> c) -> Beh a -> Beh b -> Beh c
zipWith f (Beh as) (Beh bs) =
  Beh (\t -> run (as t) (bs t))
  where
    run :: (Stable a, Stable b) => SBeh a -> SBeh b -> SBeh c
    run (x ::: xs) (y ::: ys) =
      ( app x y
          ::: delay
            ( ( case select xs ys of
                  Fst xs' lys -> run (xs') (y ::: lys)
                  Snd lxs ys' -> run (x ::: lxs) (ys')
                  Both xs' ys' -> run (xs') (ys')
              )
            )
      )
      where
        app (K x') (K y') = K (unbox f x' y')
        app (Fun xs x') (Fun ys y') =
          Fun
            (xs :* ys)
            ( box
                ( \(s :* s') t ->
                    let (a :* xs') = unbox x' s t
                        (b :* ys') = unbox y' s' t
                        left = unbox f a b
                     in case (xs' :* ys') of
                          (Just' xs'' :* Just' ys'') -> left :* Just' (xs'' :* ys'')
                          _ -> left :* Nothing'
                )
            )
        app (Fun xs x') (K y') =
          Fun
            xs
            ( box
                ( \s t ->
                    let (a :* xs') = unbox x' s t
                        left = unbox f a y'
                     in left :* xs'
                )
            )
        app (K x') (Fun ys y') =
          Fun
            ys
            ( box
                ( \s t ->
                    let (b :* ys') = unbox y' s t
                        left = unbox f x' b
                     in left :* ys'
                )
            )

-- -- | Variant of 'zipWith' with three behaviours.
zipWith3 :: forall a b c d. (Stable a, Stable b, Stable c) => Box (a -> b -> c -> d) -> Beh a -> Beh b -> Beh c -> Beh d
zipWith3 f as bs cs = WidgetRattus.Behaviour.zipWith (box (\f' x -> unbox f' x)) cds cs
  where
    cds :: Beh (Box (c -> d))
    cds = WidgetRattus.Behaviour.zipWith (box (\a b -> box (\c -> unbox f a b c))) as bs

stop :: Box (a -> Bool) -> Beh a -> Beh a
stop p (Beh as) = Beh (\t -> run (as t))
  where
    run (K x ::: xs) = K x ::: if unbox p x then never else delay (run (adv xs))
    run (Fun s f ::: xs) =
      Fun
        s
        ( box
            ( \s' t ->
                let (a :* s'') = unbox f s' t
                 in let b = unbox p a
                     in (if b then a :* Nothing' else a :* s'')
            )
        )
        ::: delay (run (adv xs))

stopWith :: Box (a -> Maybe' a) -> Beh a -> Beh a
stopWith p (Beh as) = Beh (\t -> run (as t))
  where
    run (K x ::: xs) =
      case unbox p x of
        Just' a -> K a ::: never
        Nothing' -> K x ::: delay (run (adv xs))
    run (Fun s f ::: xs) =
      Fun
        s
        ( box
            ( \s' t ->
                let (a :* s'') = unbox f s' t
                 in case unbox p a of
                      Just' a' -> a' :* Nothing'
                      Nothing' -> a :* s''
            )
        )
        ::: delay (run (adv xs))

integral :: Float -> Beh Float -> Beh Float
integral initial (Beh as) =
  Beh (\t -> run initial (as t) t)
  where
    run :: Float -> SBeh Float -> Time -> SBeh Float
    run cur (K a ::: xs) t =
      let rest =
            delayC
              ( delay
                  ( do
                      t' <- time
                      let tDiff = diffTime t' t
                      let r = cur + a * fromRational (toRational tDiff)
                      let result = run r (adv xs) t
                      return result
                  )
              )
          curF =
            Fun
              ()
              ( box
                  ( \s t' ->
                      let tDiff = diffTime t' t
                          dt = fromRational (toRational tDiff)
                       in cur + a * dt :* Just' s
                  )
              )
       in curF ::: rest
    run cur (Fun s f ::: xs) t = integralFun cur s f xs
      where
        integralFun :: forall s. (Stable s) => Float -> s -> Box (s -> Time -> (Float :* Maybe' s)) -> O (Sig (Fun Float)) -> SBeh Float
        integralFun cur s f xs =
          let rest =
                delayC
                  ( delay
                      ( do
                          t' <- time
                          let tDiff = diffTime t' t
                          let dt = fromRational (toRational tDiff)
                          let (v :* _) = unbox f s t'
                          return (run (cur + v * dt) (adv xs) t)
                      )
                  )
              curF =
                Fun
                  (cur :* t :* s)
                  ( box
                      ( \(last :* t :* s) t' ->
                          let tDiff = diffTime t' t
                              dt = fromRational (toRational tDiff)
                              (v :* s') = unbox f s t'
                           in case s' of
                                Just' s'' -> last + v * dt :* Just' (last + v * dt :* t' :* s'')
                                _ -> v + v * dt :* Nothing'
                      )
                  )
           in (curF ::: rest)

derivative :: Beh Float -> Beh Float
derivative (Beh as) =
  Beh (\t -> let (x ::: xs) = as t in der (apply x t) (x ::: xs) t)
  where
    der :: Float -> SBeh (Float) -> Time -> SBeh (Float)
    der last (Fun s f ::: xs) t = derFun last s f xs
      where
        derFun :: forall s. (Stable s) => Float -> s -> Box (s -> Time -> (Float :* Maybe' s)) -> O (SBeh Float) -> SBeh Float
        derFun last s f xs =
          let rest =
                delayC
                  ( delay
                      ( do
                          t' <- time
                          let (v :* _) = unbox f s t'
                          return $ der v (adv xs) t'
                      )
                  )
              curF =
                Fun (last :* t :* s) $
                  box
                    ( \(last :* t :* s) t' ->
                        let tDiff = diffTime t' t
                            dt = fromRational (toRational tDiff)
                            (v :* s') = unbox f s t
                         in case s' of
                              Just' s'' -> (v - last) / dt :* Just' (v :* t' :* s'')
                              Nothing' -> (v - last) / dt :* Nothing'
                    )
           in (curF ::: rest)
    der last (K x ::: xs) t =
      let rest =
            delayC
              ( delay
                  ( do
                      t' <- time
                      return $ der x (adv xs) t'
                  )
              )
          curF =
            Fun (last :* t) $
              box
                ( \(last :* t) t' ->
                    let tDiff = diffTime t' t
                        dt = fromRational (toRational tDiff)
                     in if x /= last then (x - last) / dt :* Just' (x :* t') else 0 :* Nothing'
                )
       in (curF ::: rest)

instance (Continuous a) => Continuous (Beh a) where
  progressInternal inp (Beh as) =
    let t = advC' (time) inp
        (x ::: xs@(Delay cl _)) = as t
     in if inputInClock inp cl
          then Beh (\t -> adv' xs inp)
          else progressInternal inp (Beh as)

  progressAndNext inp b@(Beh _) =
    let d = advC' (discretize b) inp
        (d', cl') = progressAndNext inp d
     in (Beh (\_ -> WidgetRattus.Signal.map (box (\a -> K a)) d'), cl')

  nextProgress (Beh as) =
    let t = advC' (time) (OneInput 0 ())
     in nextProgress (as t)

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

-- {-# NOINLINE [1] map #-}

-- {-# NOINLINE [1] const #-}

-- {-# NOINLINE [1] constK #-}

-- {-# NOINLINE [1] switch #-}

-- {-# RULES
-- "beh.map/beh.map" forall f g xs.
--   WidgetRattus.Behaviour.map f (WidgetRattus.Behaviour.map g xs) =
--     WidgetRattus.Behaviour.map (box (unbox f . unbox g)) xs
-- "beh.constK/beh.map" forall (f :: (Stable b) => Box (a -> b)) x.
--   WidgetRattus.Behaviour.map f (constK x) =
--     let x' = unbox f x in constK x'
-- "beh.const/beh.switch" forall x xs.
--   switch (const x) xs =
--     Beh (x ::: delay (unwrap (adv xs)))
-- "beh.constK/beh.switch" forall x xs.
--   switch (constK x) xs =
--     Beh (K x ::: delay (unwrap (adv xs)))
--   #-}
