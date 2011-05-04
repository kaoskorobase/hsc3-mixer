{-# LANGUAGE BangPatterns, CPP, DoRec, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

-- Copyright (c) 2008-2009 Paul Hudak <paul.hudak@yale.edu> 
-- 
-- This software is provided 'as-is', without any express or implied
-- warranty. In no event will the authors be held liable for any damages
-- arising from the use of this software.
-- 
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
-- 
-- 1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software. If you use this software
--    in a product, an acknowledgment in the product documentation would
--    be appreciated but is not required.
-- 
-- 2. Altered source versions must be plainly marked as such, and must not
--    be misrepresented as being the original software.
-- 
-- 3. This notice may not be removed or altered from any source
--    distribution.

module Data.Signal.SF.Base (
    SF(..)
  -- , run
  -- , unfold
  -- , nth
  -- , nth'
) where

#if __GLASGOW_HASKELL__ >= 610
import Control.Category
import Prelude hiding ((.), init, exp)
#else
import Prelude hiding (init, exp)
#endif

import Control.Applicative
import Control.Arrow
--import Control.CCA.CCNF
import Control.CCA.Types
-- import Control.CCA.ArrowP
import Control.Monad.Fix (MonadFix)
import Debug.Trace

newtype SF m a b = SF { runSF :: (a -> m (b, SF m a b)) }

instance Monad m => Functor (SF m a) where
	fmap f g = g >>> arr f

instance Monad m => Applicative (SF m a) where
    pure    = arr . const
    f <*> g = f &&& g >>> arr (uncurry ($))

-- instance ArrowInitP SF p

instance Monad m => Category (SF m) where
    id = SF h where h x = return (x, SF h)
    g . f = SF (h f g)
        where
            h f g x = do
                (y, f') <- runSF f x
                (z, g') <- runSF g y
                return (z, SF (h f' g'))

instance Monad m => Arrow (SF m) where
    arr f = g
        where g = SF (\x -> return (f x, g))
    first f = SF (g f)
        where
            g f (x, z) = do
                (y, f') <- runSF f x
                return ((y, z), SF (g f'))
    f &&& g = SF (h f g)
        where
            h f g x = do
                (y, f') <- runSF f x
                (z, g') <- runSF g x 
                return ((y, z), SF (h f' g'))
    f *** g = SF (h f g)
        where
            h f g x = do
                (y, f') <- runSF f (fst x)
                (z, g') <- runSF g (snd x) 
                return ((y, z), SF (h f' g'))

instance MonadFix m => ArrowLoop (SF m) where
    loop sf = SF (g sf)
        where
            g f x = do
                rec { ((y, z), f') <- runSF f (x, z) }
                return (y, SF (g f'))

instance Monad m => ArrowChoice (SF m) where
   left sf = SF (g sf)
       where 
         g f x = case x of
                   Left a -> do
                       (y, f') <- runSF f a
                       return (Left y, SF (g f'))
                   Right b -> return (Right b, SF (g f))

instance MonadFix m => ArrowInit (SF m) where
    init i = SF (f i)
        where f i x = return (i, SF (f x))
  -- loopD i g = SF (f i g)
  --   where
  --     f i g x = 
  --       let ((y, i'), g') = runSF g (x, i)
  --       in (y, SF (f i' g'))
  -- loopD i g = SF (f i)
  --     where
  --         f i x = 
  --             let (y, i') = g (x, i)
  --             in (y, SF (f i'))
  -- loopB i g = SF (f i g)
  --   where
  --     f i g x = 
  --       let ((y, (z, i')), g') = runSF g (x, (z, i))
  --       in (y, SF (f i' g'))

-- run :: SF a b -> [a] -> [b]
-- run (SF f) (x:xs) =
--   let (y, f') = f x 
--   in y `seq` f' `seq` (y : run f' xs)
-- 
-- unfold :: SF () a -> [a]
-- unfold = flip run inp
--   where inp = () : inp
-- 
-- 
-- nth :: Int -> SF () a -> a
-- nth n (SF f) = x `seq` if n == 0 then x else nth (n - 1) f'
--   where (x, f') = f ()
-- 
-- nth' :: Int -> (b, ((), b) -> (a, b)) -> a
-- nth' !n (i, f) = n `seq` i `seq` f `seq` aux n i
--   where
--     aux !n !i = x `seq` i' `seq` if n == 0 then x else aux (n-1) i'
--       where (x, i') = f ((), i)
