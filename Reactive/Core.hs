-- Copyright (c)2011, Heinrich Apfelmus
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
-- 
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
-- 
--     * Neither the name of Heinrich Apfelmus nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------

    TODO:
    What should we do with the variants involving time-varying functions?
    Should they get the same, or a different name?
    
    For example:
    
    map   ::          (a -> b) -> Event a -> Event b
    apply :: Behavior (a -> b) -> Event a -> Event b 
    
    filter  ::          (a -> Bool) -> Event a -> Event a
    filterB :: Behavior (a -> Bool) -> Event a -> Event a 


    accumulate  doesn't need a  Behavior  variant!
    ->  accumulate ($) b $ apply behavior event

    TODO:
    At some point, we probably need a function to dynamically switch
    between events, something like this
    
        join :: Event (Event a) -> Event a

    Not sure about this particular functions,
    but the point is that event handlers are being registered,
    and also *unregisterered* while the program is running.
    At the moment, everything is set up statically.

------------------------------------------------------------------------------}

module Reactive.Core (
    -- * Events
    -- $Event
    Event, never, fromEventSource, reactimateE,
    mapE, mapM, filter, filterChanges, partition,
    union, merge, orderedDuplicate,
    traceEvent,
    
    -- * Behaviors
    -- $Behavior
    Behavior, behavior, always, initial, changes, mapB, applyB, applyE,
    accumulate', accumulateChange, accumulateM, accumulateChangeM,
    mapAccum, zip, zipWith,
    
    -- * The @Change@ data type
    Change(..), isChange, isKeep,
    
    -- * Event Sources
    -- $EventSource
    EventSource(..), Prepare, newEventSource, fire,
    
    -- * Internal
    testCounter, testApply
    ) where

import Prelude hiding (map, mapM, filter, zip, zipWith)
import Control.Applicative
import Control.Concurrent
import Control.Monad ((<=<), liftM, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe
import System.IO

import Debug.Trace

{-----------------------------------------------------------------------------  
    Prepare
------------------------------------------------------------------------------}

-- | The 'Prepare' monad is just a type synonym for 'IO'.
-- The idea is that the event flow is set up in the 'Prepare' monad;
-- all 'Prepare' actions should be called
-- during the program initialization, but not while the event loop
-- is running.
type Prepare m a = m a

{-----------------------------------------------------------------------------  
    EventSource - "I'll call you back"
------------------------------------------------------------------------------}
{-$EventSource
    
    After having read all about 'Event's and 'Behavior's,
    you want to hook things up to an existing event-based framework,
    like @wxHaskell@ or @Gtk2Hs@.
    How do you do that?
    
    'EventSource's are a small bookkeeping device that helps you with that.
    Basically, they store event handlers. Often, you can just obtain them from
    corresponding bookkeeping devices from your framework,
    but sometimes you have to create your own 'EventSource'
    and use the 'fire' function to hook it into the framework.
    Event sources are also useful for testing.
    
    After creating an 'EventSource',
    you can finally obtain an 'Event' via the `fromEventSource' function.
-}


-- | An 'EventSource' is a facility where you can register
-- callback functions, aka event handlers.
-- 'EventSource's are the precursor of proper 'Event's.
data EventSource m a = EventSource {
                    -- | Replace all event handlers by this one.
                      setEventHandler :: (a -> m ()) -> Prepare m ()
                    -- | Retrieve the currently registered event handler.
                    , getEventHandler :: Prepare m (a -> m ()) }

-- add an additional event handler to the source
addEventHandler :: Monad m => EventSource m a -> (a -> m ()) -> Prepare m ()
addEventHandler es f = do
    g <- getEventHandler es
    setEventHandler es (\a -> g a >> f a)

-- | Fire the event handler of an event source manually.
-- Useful for hooking into external event sources.
fire :: Monad m => EventSource m a -> a -> m ()
fire es a = getEventHandler es >>= ($ a)
    -- here, the purpose of the Prepare monad is intentionally violated

-- | Create a new store for callback functions.
-- They have to be fired manually with the 'fire' function.
newEventSource :: MonadIO m => Prepare m (EventSource m a)
newEventSource = do
    handlerRef <- liftIO $ newIORef (const $ return ())
    return $ EventSource
        { setEventHandler = liftIO . writeIORef handlerRef
        , getEventHandler = liftIO (readIORef handlerRef) }

{-----------------------------------------------------------------------------
    Event
------------------------------------------------------------------------------}
{-$Event

The 'Event' type constructor is one of the cornerstones of the present
approach to functional reactive programmings.
It represents a stream of values as they occur in time.

-}


-- who would have thought that the implementation is this simple
type AddHandler m a = (a -> m ()) -> Prepare m ()

{- | @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event a = [(Time,a)]

Note that this is a semantic model;
the type is not actually implement that way,
but you can often treat it as if it where.
In particular, most of the subsequent operations
will be explained in terms of this model.

-}
data Event m a    = Never
                  | Event { addHandler :: AddHandler m a }

-- smart constructor, ensures proper sharing
mkEvent :: MonadIO m => AddHandler m a -> Prepare m (Event m a)
mkEvent h = share $ Event { addHandler = h }
    -- What happens when  unsafePerformIO  is accidentally exectued twice?
    -- In that case, work will be duplicated as there will be two
    -- buffers (event sources) for one and the same event.
    -- But this is the same as the situation without any sharing at all,
    -- so there's no harm done.
    -- There might be a problem with executing IO actions twice, though.
    -- \h -> liftIO $ unsafePerformIO $ share $ Event { addHandler = h }
    where
    -- Cache the value of an event,
    -- so that it's not recalculated for multiple consumers
    share :: MonadIO m => Event m a -> Prepare m (Event m a)
    share e1 = do
        es2 <- newEventSource
        addHandler e1 (fire es2) -- sharing happens through call-by-need
        return $ fromEventSource es2

-- | Derive an 'Event' from an 'EventSource'.
-- Apart from 'never', this is the only way to construct events.
fromEventSource :: Monad m => EventSource m a -> Event m a
fromEventSource s = Event { addHandler = addEventHandler s }

-- | Schedule an IO event to be executed whenever it happens.
-- This is the only way to observe events.
-- Semantically, you could write it as something like this
--
-- > reactimate ((time,action):es) = atTime time action >> reactimate es 
-- 
-- The 'Prepare' monad indicates that you should call this function
-- during program initialization only.
reactimateE :: Monad m => Event m (m a) -> Prepare m ()
reactimateE Never = return ()
reactimateE e     = addHandler e (\m -> m >> return ())

-- | The value 'never' denotes the event that never happens.
-- We can model it as the empty stream of events, @never = []@.
never :: Event m a
never = Never

-- | The 'Functor' instance allows you to map the values of type 'a'.
-- Semantically,
-- 
-- > fmap f ((time,a):es) = (time, f a) : fmap f es
-- instance Functor (Event m) where
--     fmap f Never = Never
--     fmap f e     = mkEvent addHandler'
--         where addHandler' g = addHandler e (g . f)
mapE :: MonadIO m => (a -> b) -> Event m a -> Prepare m (Event m b)
mapE _ Never     = return Never
mapE f e         = mkEvent addHandler'
    where addHandler' g = addHandler e (g . f)

-- | Version of 'fmap' that performs an 'IO' action for each event occurence.
mapM :: MonadIO m => (a -> m b) -> Event m a -> Prepare m (Event m b)
mapM f Never = return Never
mapM f e     = mkEvent addHandler'
    where addHandler' g = addHandler e (g <=< f)

-- | Merge two event streams of the same type. Semantically, we have
-- 
-- > union ((time1,a1):es1) ((time2,a2):es2)
-- >    | time1 < time2 = (time1,a1) : union es1 ((time2,a2):es2)
-- >    | time1 > time2 = (time2,a2) : union ((time1,a1):es1) es2
-- >    | otherwise     = ... -- either of the previous two cases
-- 
-- Note that the order of events that happen simultaneously is /undefined/.
-- This is not a problem most of the time,
-- but sometimes you have to force a certain order.
-- In that case, you have to combine this with the 'orderedDuplicate' function. 
union :: MonadIO m => Event m a -> Event m a -> Prepare m (Event m a)
union Never e2    = return e2
union e1    Never = return Event { addHandler = addHandler e1} -- need to be lazy here
union e1    e2    = mkEvent addHandler'
    where addHandler' g = addHandler e1 g >> addHandler e2 g
    -- FIXME: union and recursion
    -- Sometimes, events depend on themselves recursively.
    -- This is were things get hairy.
    -- Problem: Checking whether an event is Never may result in a black hole
    -- For now, union is left-biased. Maye it should always return
    -- Event anyway.

-- | The 'Monoid' instance allows you to merge event streams,
-- see the 'union' function below.
-- 
-- > mempty  = never
-- > mappend = union
-- instance Monoid (Event m a) where
--     mempty  = never
--     mappend = union

-- | Merge two event streams that have differen types. Semantically, we have
-- 
-- > merge e1 e2 = fmap Left e1 `union` fmap Right e2
merge :: MonadIO m => Event m a -> Event m b -> Prepare m (Event m (Either a b))
merge e1 e2 = do
    e1' <- mapE Left e1
    e2' <- mapE Right e2
    e1' `union` e2'

-- | Duplicate an event stream while paying attention to ordering.
-- Events from the first duplicate (and anything derived from them)
-- will always happen
-- before the events from the second duplicate.
-- Use this function to fine-tune the order of events.
orderedDuplicate :: MonadIO m => Event m a -> Prepare m (Event m a, Event m a)
{-# NOINLINE orderedDuplicate #-}
orderedDuplicate Never = return (never, never)
orderedDuplicate e     = do
    -- unsafePerformIO $ do      -- should be safe, though, only for sharing
        es1 <- newEventSource
        es2 <- newEventSource
        addHandler e $ \a -> fire es1 a >> fire es2 a
        return (fromEventSource es1, fromEventSource es2)

-- | Pass all events that fulfill the predicate, discard the rest. Semantically,
-- 
-- > filter p es = [(time,a) | (time,a) <- es, p a]
filter :: MonadIO m => (a -> Bool) -> Event m a -> Prepare m (Event m a)
filter p Never = return Never
filter p e     = mkEvent addHandler'
    where addHandler' g = addHandler e $ \a -> when (p a) (g a)

-- | Unpacks event values of the form @Change _@ and discards
-- everything else.
filterChanges :: MonadIO m => Event m (Change a) -> Prepare m (Event m a)
filterChanges e = mapE (\(Change x) -> x) =<< filter isChange e

partition :: MonadIO m => (a -> Bool) -> Event m a -> Prepare m (Event m a, Event m a)
partition f e = do
    e1 <- filter f e
    e2 <- filter (not.f) e
    return (e1, e2)

-- | Debugging helper. Prints the first argument and the value of the event
-- whenever it happens to 'stderr'.
traceEvent :: (Show a, MonadIO m) => String -> Event m a -> Prepare m (Event m a)
traceEvent s = mapM (\a -> liftIO (hPutStrLn stderr (s ++ " : " ++ show a)) >> return a)

{-----------------------------------------------------------------------------
    Behavior
------------------------------------------------------------------------------}
{-
FIXME: exporting  initial  to users might cause space leaks
where the initial value is retained long beyond the point where
it was consumed.
However, if we want the user to implement optimized behaviors
himself, like  TimeGraphic , we have to provide a mechanism
similar to this one.
Alternative: keep current value in a IORef. This will eliminate
this particular space leak? Probably not. I think it's fine the way it is.
-}

{-$Behavior

The 'Behavior' type constructor is the other cornerstone of the
present approach to functional reactive programming.
It represents a value that changes with time.

-}

{-| @Behavior a@ represents a value in time. Think of it as

> type Behavior a = Time -> a

However, note that this model misses an important point:
we only allow /piecewise constant/ functions.
Continuous behaviors like

> badbehavior = \time -> 2*time

cannot be implemented.

-}
data Behavior m a = Behavior {
    initial :: a,       -- ^ The value that the behavior initially has.
    changes :: Event m a
        -- ^ An event stream recording how the behavior changes
        -- Remember that behaviors are piecewise constant functions.
    }

-- | Smart constructor. Supply an initial value and a sequence of changes.
-- In particular,
-- 
-- > initial (behavior a es) = a
-- > changes (behavior a es) = es
behavior :: a -> Event m a -> Behavior m a
behavior = Behavior

-- | The constant behavior. Semantically,
-- 
-- > always a = \time -> a
always :: a -> Behavior m a
always a = Behavior { initial = a, changes = never }

    -- trigger an event whenever the value changes.
-- changes :: Behavior a -> Event a

-- | Version of 'accumulate' that involves the 'Change' data type
-- and performs an 'IO' action to update the value.
-- 
-- It is recommended that you use the 'accumulate' function from
-- 'Reactive.Classes' to pick types automatically.
accumulateChangeM :: MonadIO m => (b -> a -> m (Change a)) -> a -> Event m b -> Prepare m (Behavior m a)
accumulateChangeM f a eb    = do
    case eb of
        Never -> return Behavior { initial = a
                                 , changes = Never }
        _ -> do
            ref <- liftIO $ newIORef a
            eb' <- mkEvent (addHandler' ref)
            return Behavior { initial = a
                            , changes = eb' }
    where
        addHandler' ref g = addHandler eb (handler ref g)
    -- we need a global state
    -- FIXME: NOINLINE pragma!
    -- {-# NOINLINE ref #-}
    -- ref = unsafePerformIO $ newIORef a
        handler ref g = \b -> do
            a   <- liftIO $ readIORef ref    -- read old value
            ma' <- f b a            -- accumulate
            case ma' of
                Keep      -> return ()
                Change a' -> do
                    liftIO $ writeIORef ref $! a'    -- use new value
                    g a'

{- | The most important way to create behaviors.
The 'accumulate'' function is similar to a strict left fold, 'foldl''.
It starts with an initial value and combines it with incoming events.
For example, semantically
 
> accumulate' (++) "x" [(time1,"y"),(time2,"z")]
>    = behavior "x" [(time1,"yx"),(time2,"zyx")]
 
Note that the accumulated value is evaluated /strictly/.
This prevents space leaks.

It is recommended that you use the 'accumulate' function from
'Reactive.Classes' to pick types automatically.
-}
accumulate' :: MonadIO m => (b -> a -> a) -> a -> Event m b -> Prepare m (Behavior m a)
accumulate' f = accumulateChangeM (\b a -> return . Change $ f b a)

-- | Version of 'accumulate' that involves the 'Change' data type.
-- Use the 'Keep' constructor to indicate that the incoming event 
-- hasn't changed the value. No change event will be propagated in that case.
-- 
-- It is recommended that you use the 'accumulate' function from
-- 'Reactive.Classes' to pick types automatically.
accumulateChange :: MonadIO m => (b -> a -> Change a) -> a -> Event m b -> Prepare m (Behavior m a)
accumulateChange f = accumulateChangeM (\b a -> return $ f b a)


-- | Version of 'accumulate' that performs an 'IO' action to update the value.
--     
-- It is recommended that you use the 'accumulate' function from
-- 'Reactive.Classes' to pick types automatically.
accumulateM :: MonadIO m => (b -> a -> m a) -> a -> Event m b -> Prepare m (Behavior m a)
accumulateM f = accumulateChangeM (\b a -> liftM Change $ f b a)
    -- Note: IO would be unsound without sharing!


-- | The 'Functor' instance allows you to map the values of type @a@.
-- Semantically, 
-- 
-- > fmap f behavior = \time -> f (behavior time)
-- instance Functor (Behavior m) where
--     fmap f b = Behavior
--         { initial = f (initial b), changes = fmap f (changes b) }
mapB :: MonadIO m => (a -> b) -> Behavior m a -> Prepare m (Behavior m b)
mapB f b = do
    e' <- mapE f (changes b)
    return Behavior { initial = f (initial b)
                    , changes = e' }

-- | The 'Applicative' instance is one most of the most important ways
-- to combine behaviors. Semantically,
-- 
-- > pure a    = always a
-- > bf <*> bx = \time -> bf time $ bx time 
-- instance Applicative (Behavior m) where
--     pure a    = always a
--     
--     -- optimize the cases where the event never fires
--     (Behavior f Never) <*> bx = fmap (f $) bx
--     bf <*> (Behavior x Never) = fmap ($ x) bf
--     bf <*> bx                 = fmap (uncurry ($)) $
--         accumulate' go (initial bf, initial bx) (changes bf `merge` changes bx)
--         where
--         go (Left  f') (f,x) = (f',x)
--         go (Right x') (f,x) = (f,x')


-- optimize the cases where the event never fires
applyB :: MonadIO m => Behavior m (a -> b) -> Behavior m a -> Prepare m (Behavior m b)
applyB (Behavior f Never) bx = mapB (f $) bx
applyB bf (Behavior x Never) = mapB ($ x) bf
applyB bf bx                 = mapB (uncurry ($)) =<< accumulate' go (initial bf, initial bx) =<< changes bf `merge` changes bx
    where
        go (Left  f') (f,x) = (f',x)
        go (Right x') (f,x) = (f,x')

    -- store the occurences of an event in a behavior
-- latch :: Event a -> Behavior (Maybe a)
-- latch = accumulate' (\a _ -> Just a) Nothing

-- | Map events while threading state.
-- Similar to the standard 'mapAccumL' function.
mapAccum :: MonadIO m => (acc -> x -> (acc,y)) -> acc -> Event m x -> Prepare m (Behavior m acc, Event m y)
mapAccum f acc Never = return (always acc, never) 
mapAccum f acc xs    = do
    result <- accumulate' (\x (acc,_) -> f acc x) (acc,undefined) xs
    b <- mapB fst result
    e <- mapE snd (changes result)
    return (b, e)

-- | The most important way to combine behaviors and events.
-- The 'apply' function applies a time-varying function to a stream of events.
-- Semantically,
-- 
-- > apply bf es = [(time, bf time a) | (time, a) <- es]
-- 
-- (Theoretically inclined people might
-- be wondering whether we could achieve the same effect with
-- the 'Applicative' instance. The answer is no, the semantics of
-- 'apply' and '<*>' are subtly different. That's why we need to distinguish
-- between behaviors and events.)
applyE :: MonadIO m => Behavior m (a -> b) -> Event m a -> Prepare m (Event m b)
applyE (Behavior f Never) ex    = mapE f ex
applyE bf                 Never = return Never
applyE bf                 ex    =
    filterChanges . snd =<< mapAccum go (initial bf) =<< changes bf `merge` ex
    where
    go _ (Left  f) = (f, Keep)
    go f (Right x) = (f, Change $ f x)

zipWith :: MonadIO m => (a -> b -> c) -> Behavior m a -> Behavior m b -> Prepare m (Behavior m c)
zipWith f a b = flip applyB b =<< mapB f a

zip :: MonadIO m => Behavior m a -> Behavior m b -> Prepare m (Behavior m (a, b))
zip = zipWith (,)

{-----------------------------------------------------------------------------
    Change
------------------------------------------------------------------------------}
{- | Data type to indicate that a value has changed.
Used in conjunction with the 'accumulate' functions.

This is basically the @Maybe@ type with a different name.
Using a different name improves program readability
and makes it easier to automatically select the right 'accumulate'
function by type, see the 'Reactive.Classes' module.
-}
data Change a =
      Keep          -- ^ Signals that the value has not changed.
    | Change a      -- ^ Indicates a change to some value of type @a@.
    deriving (Eq, Show, Read)

instance Functor Change where
    fmap _ Keep       = Keep
    fmap f (Change a) = Change (f a)

-- | The 'isChange' function returns 'True' iff its argument is of the form @Change _@.
isChange :: Change a -> Bool
isChange (Change _) = True
isChange _          = False

-- | The 'isKeep' function returns 'True' iff its argument is of the form @Keep@.
isKeep :: Change a -> Bool
isKeep Keep = True
isKeep _    = False

{-----------------------------------------------------------------------------
    Test examples
    
    The examples return event sources that you can fire.
------------------------------------------------------------------------------}
testCounter :: MonadIO m => Prepare m (EventSource m Int)
testCounter = do
    es <- newEventSource
    let e = fromEventSource es
    reactimateE . changes =<< mapB (liftIO . print) =<< accumulate' (+) 0 e
    return es

-- test the  apply  function
testApply :: MonadIO m => Prepare m (EventSource m Int, EventSource m Int)
testApply = do
    es1 <- newEventSource
    let e1 = fromEventSource es1
    
    es2 <- newEventSource
    let e2 = fromEventSource es2

    reactimateE =<< mapE (liftIO . print) =<< flip applyE e1 =<< mapB (+) (Behavior 0 e1)
    return (es1, es2)
