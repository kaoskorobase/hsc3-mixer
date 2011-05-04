{-# LANGUAGE Arrows, RankNTypes #-}
module Data.Signal.SF (
    module Data.Signal.SF.Base
  , module Data.Signal.SF.Event
  , (>=-)
  , (-=>)
  , (>--)
  , (-->)
  -- , initially
  , tag
  , never
  , once
  , filter
  , hold
  , accumE
  , accum
  , accumM
  , stepper
  , scanl
  , edge
  , sample
  , sample_
  , switch
  , switch'
  , rswitch
  , rswitch'
  , pswitch
  , rpswitch
) where

import           Control.Arrow
-- import           Control.Arrow.Operations
-- import           Control.Arrow.Transformer (lift)
-- import           Control.Arrow.Transformer.Reader
import           Control.Applicative
import           Control.CCA.Types
import           Control.Monad.Fix (MonadFix)
import           Data.Signal.SF.Base
import           Data.Signal.SF.Event
import           Data.Traversable (Traversable)
import qualified Data.Traversable as Col
import           Prelude hiding (filter, init, scanl)
import           Debug.Trace

-- Segment set combinators
-- * select segments from sound file based on time, features
-- * construct linear sequences (for time based manipulation) or sets (for feature based manipulation)

-- ====================================================================
-- Signal functions

-- | Transform initial input value.
(>=-) :: (a -> a) -> SF m a b -> SF m a b
f >=- (SF tf) = SF (\a0 -> tf (f a0))

-- | Transform initial output value.
(-=>) :: Monad m => (b -> b) -> SF m a b -> SF m a b
f -=> (SF tf) = SF (\a0 -> do { (b0, tf') <- tf a0 ; return (f b0, tf') })

-- | Override initial input value.
(>--) :: a -> SF m a b -> SF m a b
-- (>--) a0 (SF f) = SF (\_ -> f a0)
(>--) a0 = (>=-) (const a0)

-- | Override initial output value.
--
-- Initialization operator (cf. Lustre/Lucid Synchrone).
--
-- TODO: equivalent to 'delay' in 'ArrowCircuit'?
(-->) :: Monad m => b -> SF m a b -> SF m a b
-- (-->) b0 (SF tf) = SF (\a0 -> (b0, fst (tf a0)))
(-->) b0 = (-=>) (const b0)

-- | Override initial value of input signal.
-- initially :: a -> SF a a
-- initially = (--> identity)

-- ====================================================================
-- Event sources

-- | Event source that never occurs.
never :: Monad m => SF m a (Event b)
never = pure NoEvent

-- | Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
once :: Monad m => b -> SF m a (Event b)
once b0 = (Event b0 --> never)

-- ====================================================================
-- Event modifiers

-- | Replace event value.
tag :: Monad m => b -> SF m (Event a) (Event b)
tag b = arr (b <$)

-- | asdjhaskjdh .
tagList :: Monad m => [b] -> SF m (Event a) (Event b)
tagList bs = SF (tf bs)
    where
        tf [] _             = return (NoEvent, pure NoEvent)
        tf bs NoEvent       = return (NoEvent, SF (tf bs))
        tf (b:bs) (Event _) = return (Event b, SF (tf bs))

-- | Filter out events that don't satisfy a predicate.
filter :: Monad m => (a -> Bool) -> SF m (Event a) (Event a)
filter p = arr f
    where
        f e@(Event a) = if (p a) then e else NoEvent
        f NoEvent     = NoEvent

-- ====================================================================
-- Event/signal conversion

-- | Zero order hold.
hold :: MonadFix m => a -> SF m (Event a) a
hold a0 = scanl f a0
    where
        f a NoEvent   = a
        f _ (Event a) = a

-- | Signal to event
edge :: MonadFix m => SF m Bool (Event Bool)
edge = scanl f (False, NoEvent) >>> arr snd
    where
        f (False, _) False = (False, NoEvent)
        f (False, _) True  = (True, Event True)
        f (True, _)  False = (False, Event False)
        f (True, _)  True  = (True, NoEvent)

sample :: Monad m => SF m (a, Event b) (Event (a, b))
sample = arr (\(a, e) -> event NoEvent (\b -> Event (a, b)) e)

sample_ :: Monad m => SF m (a, Event b) (Event a)
sample_ = sample >>> arr (fmap fst)

-- ====================================================================
-- Accumulators

-- | Accumulate from an initial value and an update event.
accumE :: Monad m => a -> SF m (Event (a -> a)) (Event a)
accumE a0 = SF (tf a0)
    where
        tf a NoEvent   = return (NoEvent, SF (tf a))
        tf a (Event f) = let a' = f a in a' `seq` return (Event a', SF (tf a'))

accumM :: Monad m => m a -> SF m (Event (a -> m a)) a
accumM m0 = SF tf0
    where
        tf0 e = do
            a0 <- m0
            tf a0 e
        tf a NoEvent   = return (a, SF (tf a))
        tf a (Event f) = do
            a' <- f a
            a' `seq` return (a', SF (tf a'))

-- | Accumulate from an initial value and an event carrying new values.
stepper :: Monad m => a -> SF m (Event a) a
stepper a0 = SF (tf a0)
    where
        tf a NoEvent   = return (a, SF (tf a))
        tf _ (Event a) = return (a, SF (tf a))

accum :: MonadFix m => a -> SF m (Event (a -> a)) a
accum a0 = scanl g a0
    where
        g a NoEvent   = a
        g a (Event f) = f a

scanl :: MonadFix m => (b -> a -> b) -> b -> SF m a b
scanl f b0 =
    proc a -> do
        rec
            b <- init b0 -< f b a
        returnA -< b

-- -- | Mealy-style state machine, given initial value and transition
-- -- function.  Carries along event data.  See also 'mealy_'.
-- mealy :: (b -> a -> b) -> b -> SF m a (a, b)
-- mealy b0 f = scanl g (a0, s0)
--     where
--         b0         = error "mealy: no initial value"
--         g (_, s) a = (a, f s a)
-- 
-- -- | Mealy-style state machine, given initial value and transition
-- -- function.  Forgetful version of 'mealy'.
-- mealy_ :: s -> (s -> s) -> SF m a s
-- mealy_ s0 f = mealy s0 f >>> arr snd

countDown :: ArrowInit a => Int -> a () Int
countDown x = 
    proc _ -> do
      rec 
          i <- init x -< i - 1
      returnA -< i

countUp :: ArrowInit a => a () Int
countUp = 
    proc _ -> do
      rec 
         i <- init 0 -< i + 1
      returnA -< i

-- ====================================================================
-- Switching combinators

-- | Lazy (delayed) switch.
switch :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
switch sf f = SF (g sf)
    where
        g sf a = do
            r <- runSF sf a
            case r of
                ((b, NoEvent), sf') -> return (b, SF (g sf'))
                ((b, Event c), _)   -> return (b, f c)

-- | Eager (immediate) switch.
switch' :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
switch' sf f = SF (g sf)
    where
        g sf a = do
            r <- runSF sf a
            case r of
                ((b, NoEvent), sf') -> return (b, SF (g sf'))
                ((_, Event c), _)   -> runSF (f c) a

-- | Recurring switch.
rswitch :: Monad m => SF m a b -> SF m (a, Event (SF m a b)) b
rswitch sf = switch (first sf) ((second (const NoEvent) >=-) . rswitch)

-- | Recurring switch.
rswitch' :: Monad m => SF m a b -> SF m (a, Event (SF m a b)) b
rswitch' sf = switch' (first sf) ((second (const NoEvent) >=-) . rswitch')

-- | Parallel switch parameterized on the routing function. This is the most
-- general switch from which all other (non-delayed) switches in principle
-- can be derived. The signal function collection is spatially composed in
-- parallel and run until the event signal function has an occurrence. Once
-- the switching event occurs, all signal function are "frozen" and their
-- continuations are passed to the continuation function, along with the
-- event value.
-- rf .........	Routing function: determines the input to each signal function
--		in the collection. IMPORTANT! The routing function has an
--		obligation to preserve the structure of the signal function
--		collection.
-- sfs0 .......	Signal function collection.
-- sfe0 .......	Signal function generating the switching event.
-- k .......... Continuation to be invoked once event occurs.
-- Returns the resulting signal function.
--
-- !!! Could be optimized on the event source being SFArr, SFArrE, SFArrEE
--
pswitch :: (Monad m, Traversable col) =>
       (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF m b c)
    -> SF m (a, col c) (Event d)
    -> (col (SF m b c) -> d -> SF m a (col c))
    -> SF m a (col c)
pswitch rf sfs0 sfe0 k = SF tf0
    where
        tf0 a0 = do
            let bsfs0 = rf a0 sfs0
            sfcs0 <- Col.mapM (\(b0, sf0) -> runSF sf0 b0) bsfs0
            let cs0 = fmap fst sfcs0
                sfs = fmap snd sfcs0
            r <- runSF sfe0 (a0, cs0)
            case r of
                (NoEvent, sfe) -> return (cs0, pswitch rf sfs sfe k)
                (Event d0, _)  -> runSF (k sfs0 d0) a0

-- Recurring parallel switch parameterized on the routing function.
-- rf .........	Routing function: determines the input to each signal function
--		in the collection. IMPORTANT! The routing function has an
--		obligation to preserve the structure of the signal function
--		collection.
-- sfs ........	Initial signal function collection.
-- Returns the resulting signal function.

rpswitch :: (Monad m, Traversable col) =>
       (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF m b c) -> SF m (a, Event (col (SF m b c) -> col (SF m b c))) (col c)
rpswitch rf sfs =
    pswitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
        second (const NoEvent) >=- rpswitch rf (f sfs')
