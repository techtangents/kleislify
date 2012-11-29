-------------------------------------------------------------------------
-- |
-- Module      :  Control.Kleislify
-- Copyright   :  (c) Dylan Just, 2011
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  dylan@techtangents.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Precomposition and postcomposition of functors and monads.
--
-- Variants of Control.Arrow functions, specialised to kleislis.
-- Avoids boxing into Kleisli values.


module Control.Kleislify where

import Control.Monad.Instances
import Control.Monad

infixr 1 ^=>, =>^, ^<=, <=^
infixr 1 ^->, ->^, ^<-, <-^

-- | precomposition of a monad with a pure function.
--   Equivalent to 'Control.Arrow.^>>'
--   Equivalent to 'flip (.)'
(^=>) :: Monad m => (b -> c) -> (c -> m d) -> b -> m d
(^=>) = flip (.)

-- | precomposition of a functor with a pure function.
--   Equivalent to 'flip (.)'
(^->) :: Functor f => (b -> c) -> (c -> f d) -> b -> f d
(^->) = flip (.)

-- | postcomposition of a monad with a pure function.
--   Equivalent to 'Control.Arrow.>>^'
(=>^) :: Monad m => (b -> m c) -> (c -> d) -> b -> m d
(=>^) = flip (^<=)

-- | postcomposition of a functor with a pure function.
--   Equivalent to 'Control.Arrow.>>^'
(->^) :: Functor f => (b -> f c) -> (c -> d) -> b -> f d
(->^) = flip (^<-)

-- | precomposition of a monad with a pure function (right-to-left variant).
--   Equivalent to 'Control.Arrow.<<^'
--   Equivalent to '.'
(<=^) :: Monad m => (c -> m d) -> (b -> c) -> (b -> m d)
(<=^) = (.)

-- | precomposition of a functor with a pure function (right-to-left variant).
--   Equivalent to 'Control.Arrow.<<^'
--   Equivalent to '.'
(<-^) :: Functor f => (c -> f d) -> (b -> c) -> (b -> f d)
(<-^) = (.)

-- | postcomposition of a monad with a pure function (right-to-left variant).
--   Equivalent to 'Control.Arrow.^<<'
(^<=) :: Monad m => (c -> d) -> (b -> m c) -> b -> m d
(^<=) = liftM . liftM

-- | postcomposition of a functor with a pure function (right-to-left variant).
--   Equivalent to 'Control.Arrow.^<<'
(^<-) :: Functor f => (c -> d) -> (b -> f c) -> b -> f d
(^<-) = fmap . fmap
