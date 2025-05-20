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

module WidgetRattus.Behaviour
  ( Beh (..),
    Fun (..),
    apply,
    WidgetRattus.Behaviour.map,
    mapF,
    unwrap,
    const,
    constK,
    timeBehaviour,
    sampleInterval,
    discretize,
    elapsedTime,
    withTime,
    switch,
    zipWith,
    zipWith3,
    stop,
    stopWith,
    integral,
    derivative,
  )
where

import WidgetRattus
import WidgetRattus.InternalPrimitives (Continuous (..))
import WidgetRattus.Signal hiding (const, derivative, integral, stop, switch, zipWith, zipWith3)
import Prelude hiding (const, map, zipWith, zipWith3)

-- | Time function type as described in Elliot's paper. This is modified to hold a state.
-- Either it is a constant value, or a function that takes a state and a time and returns a value and a new state.
data Fun a where
  K :: !a -> Fun a
  Fun :: (Stable s) => !s -> !(Box (s -> Time -> (a :* Maybe' s))) -> Fun a

continuous ''Fun

-- | The apply function as presented in Elliot's paper. The function simplifies accessing the value of Fun types
-- transforming a Fun a type into a Time -> a type. Thus you can use the apply function to get the value of a Fun type at a specific time.
apply :: Fun a -> (Time -> a)
apply (K a) = \_ -> a
apply (Fun s f) = \t -> let (a :* _) = unbox f s t in a

-- | The mapF function is used for mapping Fun types.
-- This is useful for applying a function on a Fun type without applying time.
mapF :: Box (a -> b) -> Fun a -> Fun b
mapF f (K a) = K (unbox f a)
mapF f (Fun s f') = Fun s (box (\s t -> let (a :* s') = unbox f' s t in (unbox f a :* s')))

-- | The behaviour type, which is a signal of the type Fun a.
newtype Beh a = Beh (Sig (Fun a))

-- | Helper function to unwrap the Beh type and get the underlying signal.
unwrap :: Beh a -> Sig (Fun a)
unwrap (Beh a) = a

-- | Helper function to make a behaviour that never ticks.
const :: Fun a -> Beh a
const x = Beh (x ::: never)

-- | Helper function to make a constant behaviour that never ticks.
constK :: a -> Beh a
constK x = Beh (K x ::: never)

-- | Identity function for Beh
timeBehaviour :: Beh Time
timeBehaviour = const (Fun () (box (\s t -> t :* Just' s)))

-- | Apply a function to the value of a behaviour.
map :: Box (a -> b) -> Beh a -> Beh b
map f (Beh (x ::: xs)) = Beh (mapF f x ::: delay (unwrap $ WidgetRattus.Behaviour.map f (Beh (adv xs))))

-- | Sample interval used in discretize.
sampleInterval :: O ()
sampleInterval = timer 20000

-- | Discretize a behaviour. This function is used to convert a continuous behaviour into a discrete signal.
discretize :: Beh a -> C (Sig a)
discretize (Beh (K x ::: xs)) = do
  let rest = delayC $ delay (let x' = adv xs in discretize (Beh x'))
  return $ x ::: rest
discretize (Beh (Fun s f ::: xs)) = discretizeFun s f xs
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
                        Fst x _ -> discretize (Beh x)
                        Snd beh' _ -> discretize (Beh (Fun s'' f ::: beh'))
                        Both x _ -> discretize (Beh x)
                    )
              Nothing' -> delayC $ delay (let sig = adv xs in discretize (Beh sig))

      return (cur ::: rest)

-- | This function is used to get the elapsed time since the start of the program.
elapsedTime :: C (Beh NominalDiffTime)
elapsedTime = do
  startTime <- time
  return $ Beh (Fun () (box (\s currentTime -> diffTime currentTime startTime :* Just' s)) ::: never)

-- | The withTime function, applies the current time to a delayed computation.
-- It takes a delayed value of type O (Time -> a) and produces a delayed result of type O a.
-- This is done by advancing the delayed function, retrieving the current time from the
-- C monad, and applying the time to the function. The use of delayC eliminates
-- the C monad, yielding a pure delayed value. Look in @trigger@ for an example of this.
withTime :: O (Time -> a) -> O a
withTime delayed =
  delayC $ delay (let f = adv delayed in do f <$> time)

-- | This function is used to switch between two behaviours. It takes a behaviour and a delayed
-- behaviour. When the delayed behaviour ticks, it will switch to this behaviour.
switch :: Beh a -> O (Beh a) -> Beh a
switch (Beh (x ::: xs)) d =
  Beh $
    x
      ::: delay
        ( case select xs d of
            Fst xs' d' -> unwrap $ WidgetRattus.Behaviour.switch (Beh xs') d'
            Snd _ (Beh d') -> d'
            Both _ (Beh d') -> d'
        )

-- | This function combines the values of two signals
-- using the function argument. @zipWith f xs ys@ produces a new value
-- @unbox f x y@ whenever @xs@ or @ys@ produce a new value, where @x@
-- and @y@ are the current values of @xs@ and @ys@, respectively.
--
-- Example:
--
-- >                      xs:  1 2 3     2
-- >                      ys:  1     0 5 2
-- > zipWith (box (+)) xs ys:  2 3 4 3 8 4
zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Beh a -> Beh b -> Beh c
zipWith f (Beh (x ::: xs)) (Beh (y ::: ys)) =
  Beh
    ( app x y
        ::: delay
          ( let (Beh rest) =
                  ( case select xs ys of
                      Fst xs' lys -> WidgetRattus.Behaviour.zipWith f (Beh xs') (Beh (y ::: lys))
                      Snd lxs ys' -> WidgetRattus.Behaviour.zipWith f (Beh (x ::: lxs)) (Beh ys')
                      Both xs' ys' -> WidgetRattus.Behaviour.zipWith f (Beh xs') (Beh ys')
                  )
             in rest
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

-- | Variant of 'zipWith' with three behaviours.
zipWith3 :: forall a b c d. (Stable a, Stable b, Stable c) => Box (a -> b -> c -> d) -> Beh a -> Beh b -> Beh c -> Beh d
zipWith3 f as bs cs = WidgetRattus.Behaviour.zipWith (box (\f' x -> unbox f' x)) cds cs
  where
    cds :: Beh (Box (c -> d))
    cds = WidgetRattus.Behaviour.zipWith (box (\a b -> box (\c -> unbox f a b c))) as bs

-- | Stops as soon as the the predicate becomes true for the current
-- value. That is, @stop (box p) xs@ first behaves as @xs@, but as
-- soon as @f x = True@ for some (current or future) value @x@ of
-- @xs@, then it behaves as @const x@.
stop :: Box (a -> Bool) -> Beh a -> Beh a
stop p (Beh b) = Beh (run b)
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

-- | Variant of 'stop', which uses a Maybe' instead of a boolean.
stopWith :: Box (a -> Maybe' a) -> Beh a -> Beh a
stopWith p (Beh b) = Beh (run b)
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

-- | @integral x xs@ computes the integral of the behaviour @xs@ with the
-- constant @x@. For example, if @xs@ is the velocity of an object,
-- the behaviour @integral 0 xs@ describes the distance travelled by that
-- object.
integral :: Float -> Beh Float -> C (Beh Float)
integral cur (Beh (K a ::: xs)) = do
  t <- time
  let rest =
        delayC
          ( delay
              ( do
                  t' <- time
                  let tDiff = diffTime t' t
                  let r = cur + a * fromRational (toRational tDiff)
                  let result = integral r (Beh (adv xs))
                  unwrap <$> result
              )
          )
  let curF =
        case a of
          0 -> K cur
          a ->
            Fun
              ()
              ( box
                  ( \s t' ->
                      let tDiff = diffTime t' t
                          dt = fromRational (toRational tDiff)
                       in cur + a * dt :* Just' s
                  )
              )
  return (Beh (curF ::: rest))
integral cur (Beh (Fun s f ::: xs)) = integralFun cur s f xs
  where
    integralFun :: forall s. (Stable s) => Float -> s -> Box (s -> Time -> (Float :* Maybe' s)) -> O (Sig (Fun Float)) -> C (Beh Float)
    integralFun cur s f xs =
      do
        t <- time
        let rest =
              delayC
                ( delay
                    ( do
                        t' <- time
                        let tDiff = diffTime t' t
                        let dt = fromRational (toRational tDiff)
                        let (v :* _) = unbox f s t'
                        unwrap <$> integral (cur + v * dt) (Beh (adv xs))
                    )
                )
        let curF =
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
        return $ Beh (curF ::: rest)

-- | Compute the derivative of a behaviour. For example, if @xs@ is the
-- velocity of an object, the behaviour @derivative xs@ describes the
-- acceleration travelled by that object.
derivative :: Beh Float -> C (Beh Float)
derivative (Beh (x ::: xs)) = do
  t <- time
  Beh <$> der (apply x t) (x ::: xs)
  where
    der :: Float -> Sig (Fun Float) -> C (Sig (Fun Float))
    der last (Fun s f ::: xs) = derFun last s f xs
      where
        derFun :: forall s. (Stable s) => Float -> s -> Box (s -> Time -> (Float :* Maybe' s)) -> O (Sig (Fun Float)) -> C (Sig (Fun Float))
        derFun last s f xs = do
          t <- time
          let rest =
                delayC
                  ( delay
                      ( do
                          t' <- time
                          let (v :* _) = unbox f s t'
                          der v (adv xs)
                      )
                  )
          let curF =
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
          return (curF ::: rest)
    der last (K x ::: xs) = do
      t <- time
      let rest = delayC (delay (do der x (adv xs)))
      let curF =
            Fun (last :* t) $
              box
                ( \(last :* t) t' ->
                    let tDiff = diffTime t' t
                        dt = fromRational (toRational tDiff)
                     in if x /= last then (x - last) / dt :* Just' (x :* t') else 0 :* Nothing'
                )
      return (curF ::: rest)

instance (Continuous a) => Continuous (Beh a) where
  progressInternal inp (Beh sig) = Beh (progressInternal inp sig)
  progressAndNext inp (Beh sig) =
    let (sig', cl) = progressAndNext inp sig
     in (Beh sig', cl)
  nextProgress (Beh sig) = nextProgress sig

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

{-# NOINLINE [1] map #-}

{-# NOINLINE [1] const #-}

{-# NOINLINE [1] constK #-}

{-# NOINLINE [1] switch #-}

{-# RULES
"beh.map/beh.map" forall f g xs.
  WidgetRattus.Behaviour.map f (WidgetRattus.Behaviour.map g xs) =
    WidgetRattus.Behaviour.map (box (unbox f . unbox g)) xs
"beh.constK/beh.map" forall (f :: (Stable b) => Box (a -> b)) x.
  WidgetRattus.Behaviour.map f (constK x) =
    let x' = unbox f x in constK x'
"beh.const/beh.switch" forall x xs.
  switch (const x) xs =
    Beh (x ::: delay (unwrap (adv xs)))
"beh.constK/beh.switch" forall x xs.
  switch (constK x) xs =
    Beh (K x ::: delay (unwrap (adv xs)))
  #-}
