{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
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
    Reactive-Banana
------------------------------------------------------------------------------}
module Reactive.Classes (
    -- $doc
    ReactiveSyntax(..)
  , ReactiveAccumulate(..)
    ) where

import Control.Monad.IO.Class (MonadIO)
import Reactive.Core

{-$doc
This module provides a syntactically convenient 'accumulate' function.
This is an extra module because it uses type class extensions.
-}

-- | Convenient type class for automatically
-- selecting the right 'accumulate' function by type.
class MonadIO m => ReactiveAccumulate m b t where
    accumulate :: (a -> b -> t) -> b -> Event m a -> Prepare m (Behavior m b)

instance MonadIO m => ReactiveAccumulate m b b where
    accumulate = accumulate'
instance MonadIO m => ReactiveAccumulate m b (Change b) where
    accumulate = accumulateChange
instance MonadIO m => ReactiveAccumulate m b (m b) where
    accumulate = accumulateM
instance MonadIO m => ReactiveAccumulate m b (m (Change b)) where
    accumulate = accumulateChangeM

class MonadIO m => ReactiveSyntax m t where
    map :: (a -> b) -> t a -> Prepare m (t b)
    apply :: Behavior m (a -> b) -> t a -> Prepare m (t b)
    reactimate :: t (m a) -> Prepare m ()

instance MonadIO m => ReactiveSyntax m (Event m) where
    map = mapE
    apply = applyE
    reactimate = reactimateE
instance MonadIO m => ReactiveSyntax m (Behavior m) where
    map = mapB
    apply = applyB
    reactimate = reactimateE . changes
