{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Tuple.Strict.Lens.Iso
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : FlexibleContexts, MPTC
--
-- This module exports 'Control.Lens.Iso.Swapped' and
-- 'Control.Lens.Iso.Strict' instances for 'T1' through 'T9'
--
module Data.Tuple.Strict.Lens.Iso
( Swapped(..)
, Strict(..)
) where

import Control.Lens.Iso (Swapped(..), Strict(..), iso)

import Data.Functor.Identity
import Data.Tuple.Strict


-- ---------------------------------------------------------------- --
-- Swapped instances

instance Swapped T2 where
  swapped = iso sswap sswap
  {-# inline swapped #-}

instance Swapped (T3 x) where
  swapped = iso f f
    where
      f (T3 x a b) = T3 x b a
  {-# inline swapped #-}

instance Swapped (T4 x y) where
  swapped = iso f f
    where
      f (T4 x y a b) = T4 x y b a
  {-# inline swapped #-}

instance Swapped (T5 x y z) where
  swapped = iso f f
    where
      f (T5 x y z a b) = T5 x y z b a
  {-# inline swapped #-}

instance Swapped (T6 x y z w) where
  swapped = iso f f
    where
      f (T6 x y z w a b) = T6 x y z w b a
  {-# inline swapped #-}

instance Swapped (T7 x y z w v) where
  swapped = iso f f
    where
      f (T7 x y z w v a b) = T7 x y z w v b a
  {-# inline swapped #-}

instance Swapped (T8 x y z w v u) where
  swapped = iso f f
    where
      f (T8 x y z w v a b c) = T8 x y z w v a c b
  {-# inline swapped #-}

instance Swapped (T9 a b c d e f g) where
  swapped = iso k k
    where
      k (T9 a b c d e f g h i) = T9 a b c d e f g i h
  {-# inline swapped #-}

-- ---------------------------------------------------------------- --
-- Strict instances

instance Strict (Identity a) (T1 a) where
  strict = iso (\(Identity a) -> T1 a) (\(T1 a) -> Identity a)
  {-# inline strict #-}

instance Strict (a,b) (T2 a b) where
  strict = iso (\(a,b) -> T2 a b) (\(T2 a b) -> (a,b))
  {-# inline strict #-}

instance Strict (a,b,c) (T3 a b c) where
  strict = iso (\(a,b,c) -> T3 a b c) (\(T3 a b c) -> (a,b,c))
  {-# inline strict #-}

instance Strict (a,b,c,d) (T4 a b c d) where
  strict = iso (\(a,b,c,d) -> T4 a b c d) (\(T4 a b c d) -> (a,b,c,d))
  {-# inline strict #-}

instance Strict (a,b,c,d,e) (T5 a b c d e) where
  strict = iso (\(a,b,c,d,e) -> T5 a b c d e) (\(T5 a b c d e) -> (a,b,c,d,e))
  {-# inline strict #-}

instance Strict (a,b,c,d,e,f) (T6 a b c d e f) where
  strict = iso (\(a,b,c,d,e,f) -> T6 a b c d e f) (\(T6 a b c d e f) -> (a,b,c,d,e,f))
  {-# inline strict #-}

instance Strict (a,b,c,d,e,f,g) (T7 a b c d e f g) where
  strict = iso (\(a,b,c,d,e,f,g) -> T7 a b c d e f g) (\(T7 a b c d e f g) -> (a,b,c,d,e,f,g))
  {-# inline strict #-}

instance Strict (a,b,c,d,e,f,g,h) (T8 a b c d e f g h) where
  strict = iso (\(a,b,c,d,e,f,g,h) -> T8 a b c d e f g h) (\(T8 a b c d e f g h) -> (a,b,c,d,e,f,g,h))
  {-# inline strict #-}

instance Strict (a,b,c,d,e,f,g,h,i) (T9 a b c d e f g h i) where
  strict = iso
    (\(a,b,c,d,e,f,g,h,i) -> T9 a b c d e f g h i)
    (\(T9 a b c d e f g h i) -> (a,b,c,d,e,f,g,h,i))
  {-# inline strict #-}
