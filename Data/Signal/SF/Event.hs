module Data.Signal.SF.Event (
    Event(..)
  , event
  , eventToMaybe
  , maybeToEvent
  , mergeBy
  , merge
  , split
) where

import Control.Applicative
import Control.Monad (MonadPlus(..))

data Event a = NoEvent | Event a deriving (Eq, Read, Show)

event :: b -> (a -> b) -> Event a -> b
event b _ NoEvent   = b
event _ f (Event a) = f a

eventToMaybe :: Event a -> Maybe a
eventToMaybe = event Nothing Just

maybeToEvent :: Maybe a -> Event a
maybeToEvent = maybe NoEvent Event

mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeBy _       NoEvent      NoEvent      = NoEvent
mergeBy _       le@(Event _) NoEvent      = le
mergeBy _       NoEvent      re@(Event _) = re
mergeBy resolve (Event l)    (Event r)    = Event (resolve l r)

merge :: Show a => Event a -> Event a -> Event a
merge NoEvent NoEvent      = NoEvent
merge le@(Event _) NoEvent = le
merge NoEvent re@(Event _) = re
merge e1@(Event _) e2@(Event _)  = error $ "merge: Simultaneous event occurrence: " ++ show e1 ++ " " ++ show e2

split :: Event (a, b) -> (Event a, Event b)
split NoEvent        = (NoEvent, NoEvent)
split (Event (a, b)) = (Event a, Event b)

instance Functor Event where
    fmap _ NoEvent   = NoEvent
    fmap f (Event a) = Event (f a)

instance Applicative Event where
    pure = Event
    NoEvent   <*> _         = NoEvent
    _         <*> NoEvent   = NoEvent
    (Event f) <*> (Event a) = Event (f a)

instance Alternative Event where
    empty = NoEvent
    NoEvent <|> p = p
    Event a <|> _ = Event a

instance Monad Event where
    (Event x) >>= k = k x
    NoEvent   >>= _ = NoEvent

    (Event _) >>  k = k
    NoEvent   >>  _ = NoEvent

    return          = Event
    fail _          = NoEvent

instance MonadPlus Event where
   mzero = NoEvent

   NoEvent `mplus` ys = ys
   xs      `mplus` _  = xs
