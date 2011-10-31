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
-- Variants of Control.Arrow functions, specialised to kleislis. 
-- Avoids boxing into Kleisli values.

module Control.Kleislify where

import Control.Arrow

infixr 1 ^=>, =>^
infixr 1 ^<=, <=^

-- | Kleisli precomposition of a monad with a pure function. 
--   Equivalent to 'Control.Arrow.^>>'
(^=>) :: Monad m => (b -> c)  -> (c -> m d) -> b -> m d 
(^=>) f k = runKleisli $ f ^>> (Kleisli k)

-- | Kleisli postcomposition of a monad with a pure function. 
--   Equivalent to 'Control.Arrow.>>^'
(=>^) :: Monad m => (b -> m c) -> (c -> d) -> b -> m d
(=>^) k f = runKleisli $ Kleisli k >>^ f

-- | Kleisli precomposition of a monad with a pure function (right-to-left variant). 
--   Equivalent to 'Control.Arrow.<<^'
(<=^) :: Monad m => (c -> m d) -> (b -> c) -> (b -> m d)
(<=^) = flip (^=>)

-- | Kleisli postcomposition of a monad with a pure function (right-to-left variant).
--   Equivalent to 'Control.Arrow.^<<'
(^<=) :: Monad m => (c -> d) -> (b -> m c) -> b -> m d
(^<=) = flip (=>^)

