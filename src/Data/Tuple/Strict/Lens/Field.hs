{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Tuple.Strict.Lens.Field
-- Copyright    : (c) 2020-2021 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : FlexibleContexts, MPTC
--
-- This module exports 'Control.Lens.Field.Field1' through
-- 'Control.Lens.Field.Field19' instances for 'T1' through
-- 'T19'.
--
module Data.Tuple.Strict.Lens.Field
( Field1(..)
, Field2(..)
, Field3(..)
, Field4(..)
, Field5(..)
, Field6(..)
, Field7(..)
, Field8(..)
, Field9(..)
, Field10(..)
, Field11(..)
, Field12(..)
, Field13(..)
, Field14(..)
, Field15(..)
, Field16(..)
, Field17(..)
, Field18(..)
, Field19(..)
) where


import Control.Lens

import Data.Tuple.Strict

-- ---------------------------------------------------------------- --
-- Field1 optics

instance Field1 (T1 a) (T1 a') a a' where
  _1 k ~(T1 a) = T1 <$> k a

instance Field1 (T2 a b) (T2 a' b) a a' where
  _1 k ~(T2 a b) = k a <&> \a' -> (T2 a' b)
  {-# inline _1 #-}

instance Field1 (T3 a b c) (T3 a' b c) a a' where
  _1 k ~(T3 a b c) = k a <&> \a' -> (T3 a' b c)
  {-# inline _1 #-}

instance Field1 (T4 a b c d) (T4 a' b c d) a a' where
  _1 k ~(T4 a b c d) = k a <&> \a' -> (T4 a' b c d)
  {-# inline _1 #-}

instance Field1 (T5 a b c d e) (T5 a' b c d e) a a' where
  _1 k ~(T5 a b c d e) = k a <&> \a' -> (T5 a' b c d e)
  {-# inline _1 #-}

instance Field1 (T6 a b c d e f) (T6 a' b c d e f) a a' where
  _1 k ~(T6 a b c d e f) = k a <&> \a' -> (T6 a' b c d e f)
  {-# inline _1 #-}

instance Field1 (T7 a b c d e f g) (T7 a' b c d e f g) a a' where
  _1 k ~(T7 a b c d e f g) = k a <&> \a' -> (T7 a' b c d e f g)
  {-# inline _1 #-}

instance Field1 (T8 a b c d e f g h) (T8 a' b c d e f g h) a a' where
  _1 k ~(T8 a b c d e f g h) = k a <&> \a' -> (T8 a' b c d e f g h)
  {-# inline _1 #-}

instance Field1 (T9 a b c d e f g h i) (T9 a' b c d e f g h i) a a' where
  _1 k ~(T9 a b c d e f g h i) = k a <&> \a' -> (T9 a' b c d e f g h i)
  {-# inline _1 #-}

instance Field1 (T10 a b c d e f g h i j) (T10 a' b c d e f g h i j) a a' where
  _1 k ~(T10 a b c d e f g h i j) = k a <&> \a' -> (T10 a' b c d e f g h i j)
  {-# inline _1 #-}

instance Field1 (T11 a b c d e f g h i j kk) (T11 a' b c d e f g h i j kk) a a' where
  _1 k ~(T11 a b c d e f g h i j kk) = k a <&> \a' -> (T11 a' b c d e f g h i j kk)
  {-# inline _1 #-}

instance Field1 (T12 a b c d e f g h i j kk l) (T12 a' b c d e f g h i j kk l) a a' where
  _1 k ~(T12 a b c d e f g h i j kk l) = k a <&> \a' -> (T12 a' b c d e f g h i j kk l)
  {-# inline _1 #-}

instance Field1 (T13 a b c d e f g h i j kk l m) (T13 a' b c d e f g h i j kk l m) a a' where
  _1 k ~(T13 a b c d e f g h i j kk l m) = k a <&> \a' -> (T13 a' b c d e f g h i j kk l m)
  {-# inline _1 #-}

instance Field1 (T14 a b c d e f g h i j kk l m n) (T14 a' b c d e f g h i j kk l m n) a a' where
  _1 k ~(T14 a b c d e f g h i j kk l m n) = k a <&> \a' -> (T14 a' b c d e f g h i j kk l m n)
  {-# inline _1 #-}

instance Field1 (T15 a b c d e f g h i j kk l m n o) (T15 a' b c d e f g h i j kk l m n o) a a' where
  _1 k ~(T15 a b c d e f g h i j kk l m n o) = k a <&> \a' -> (T15 a' b c d e f g h i j kk l m n o)
  {-# inline _1 #-}

instance Field1 (T16 a b c d e f g h i j kk l m n o p) (T16 a' b c d e f g h i j kk l m n o p) a a' where
  _1 k ~(T16 a b c d e f g h i j kk l m n o p) = k a <&> \a' -> (T16 a' b c d e f g h i j kk l m n o p)
  {-# inline _1 #-}

instance Field1 (T17 a b c d e f g h i j kk l m n o p q) (T17 a' b c d e f g h i j kk l m n o p q) a a' where
  _1 k ~(T17 a b c d e f g h i j kk l m n o p q) = k a <&> \a' -> (T17 a' b c d e f g h i j kk l m n o p q)
  {-# inline _1 #-}

instance Field1 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a' b c d e f g h i j kk l m n o p q r) a a' where
  _1 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k a <&> \a' -> (T18 a' b c d e f g h i j kk l m n o p q r)
  {-# inline _1 #-}

instance Field1 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a' b c d e f g h i j kk l m n o p q r s) a a' where
  _1 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k a <&> \a' -> (T19 a' b c d e f g h i j kk l m n o p q r s)
  {-# inline _1 #-}

-- ---------------------------------------------------------------- --
-- Field2 optics

instance Field2 (T2 a b) (T2 a b') b b' where
  _2 k ~(T2 a b) = k b <&> \b' -> (T2 a b')
  {-# inline _2 #-}

instance Field2 (T3 a b c) (T3 a b' c) b b' where
  _2 k ~(T3 a b c) = k b <&> \b' -> (T3 a b' c)
  {-# inline _2 #-}

instance Field2 (T4 a b c d) (T4 a b' c d) b b' where
  _2 k ~(T4 a b c d) = k b <&> \b' -> (T4 a b' c d)
  {-# inline _2 #-}

instance Field2 (T5 a b c d e) (T5 a b' c d e) b b' where
  _2 k ~(T5 a b c d e) = k b <&> \b' -> (T5 a b' c d e)
  {-# inline _2 #-}

instance Field2 (T6 a b c d e f) (T6 a b' c d e f) b b' where
  _2 k ~(T6 a b c d e f) = k b <&> \b' -> (T6 a b' c d e f)
  {-# inline _2 #-}

instance Field2 (T7 a b c d e f g) (T7 a b' c d e f g) b b' where
  _2 k ~(T7 a b c d e f g) = k b <&> \b' -> (T7 a b' c d e f g)
  {-# inline _2 #-}

instance Field2 (T8 a b c d e f g h) (T8 a b' c d e f g h) b b' where
  _2 k ~(T8 a b c d e f g h) = k b <&> \b' -> (T8 a b' c d e f g h)
  {-# inline _2 #-}

instance Field2 (T9 a b c d e f g h i) (T9 a b' c d e f g h i) b b' where
  _2 k ~(T9 a b c d e f g h i) = k b <&> \b' -> (T9 a b' c d e f g h i)
  {-# inline _2 #-}

instance Field2 (T10 a b c d e f g h i j) (T10 a b' c d e f g h i j) b b' where
  _2 k ~(T10 a b c d e f g h i j) = k b <&> \b' -> (T10 a b' c d e f g h i j)
  {-# inline _2 #-}

instance Field2 (T11 a b c d e f g h i j kk) (T11 a b' c d e f g h i j kk) b b' where
  _2 k ~(T11 a b c d e f g h i j kk) = k b <&> \b' -> (T11 a b' c d e f g h i j kk)
  {-# inline _2 #-}

instance Field2 (T12 a b c d e f g h i j kk l) (T12 a b' c d e f g h i j kk l) b b' where
  _2 k ~(T12 a b c d e f g h i j kk l) = k b <&> \b' -> (T12 a b' c d e f g h i j kk l)
  {-# inline _2 #-}

instance Field2 (T13 a b c d e f g h i j kk l m) (T13 a b' c d e f g h i j kk l m) b b' where
  _2 k ~(T13 a b c d e f g h i j kk l m) = k b <&> \b' -> (T13 a b' c d e f g h i j kk l m)
  {-# inline _2 #-}

instance Field2 (T14 a b c d e f g h i j kk l m n) (T14 a b' c d e f g h i j kk l m n) b b' where
  _2 k ~(T14 a b c d e f g h i j kk l m n) = k b <&> \b' -> (T14 a b' c d e f g h i j kk l m n)
  {-# inline _2 #-}

instance Field2 (T15 a b c d e f g h i j kk l m n o) (T15 a b' c d e f g h i j kk l m n o) b b' where
  _2 k ~(T15 a b c d e f g h i j kk l m n o) = k b <&> \b' -> (T15 a b' c d e f g h i j kk l m n o)
  {-# inline _2 #-}

instance Field2 (T16 a b c d e f g h i j kk l m n o p) (T16 a b' c d e f g h i j kk l m n o p) b b' where
  _2 k ~(T16 a b c d e f g h i j kk l m n o p) = k b <&> \b' -> (T16 a b' c d e f g h i j kk l m n o p)
  {-# inline _2 #-}

instance Field2 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b' c d e f g h i j kk l m n o p q) b b' where
  _2 k ~(T17 a b c d e f g h i j kk l m n o p q) = k b <&> \b' -> (T17 a b' c d e f g h i j kk l m n o p q)
  {-# inline _2 #-}

instance Field2 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b' c d e f g h i j kk l m n o p q r) b b' where
  _2 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k b <&> \b' -> (T18 a b' c d e f g h i j kk l m n o p q r)
  {-# inline _2 #-}

instance Field2 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b' c d e f g h i j kk l m n o p q r s) b b' where
  _2 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k b <&> \b' -> (T19 a b' c d e f g h i j kk l m n o p q r s)
  {-# inline _2 #-}

-- ---------------------------------------------------------------- --
-- Field3 optics

instance Field3 (T3 a b c) (T3 a b c') c c' where
  _3 k ~(T3 a b c) = k c <&> \c' -> (T3 a b c')
  {-# inline _3 #-}

instance Field3 (T4 a b c d) (T4 a b c' d) c c' where
  _3 k ~(T4 a b c d) = k c <&> \c' -> (T4 a b c' d)
  {-# inline _3 #-}

instance Field3 (T5 a b c d e) (T5 a b c' d e) c c' where
  _3 k ~(T5 a b c d e) = k c <&> \c' -> (T5 a b c' d e)
  {-# inline _3 #-}

instance Field3 (T6 a b c d e f) (T6 a b c' d e f) c c' where
  _3 k ~(T6 a b c d e f) = k c <&> \c' -> (T6 a b c' d e f)
  {-# inline _3 #-}

instance Field3 (T7 a b c d e f g) (T7 a b c' d e f g) c c' where
  _3 k ~(T7 a b c d e f g) = k c <&> \c' -> (T7 a b c' d e f g)
  {-# inline _3 #-}

instance Field3 (T8 a b c d e f g h) (T8 a b c' d e f g h) c c' where
  _3 k ~(T8 a b c d e f g h) = k c <&> \c' -> (T8 a b c' d e f g h)
  {-# inline _3 #-}

instance Field3 (T9 a b c d e f g h i) (T9 a b c' d e f g h i) c c' where
  _3 k ~(T9 a b c d e f g h i) = k c <&> \c' -> (T9 a b c' d e f g h i)
  {-# inline _3 #-}

instance Field3 (T10 a b c d e f g h i j) (T10 a b c' d e f g h i j) c c' where
  _3 k ~(T10 a b c d e f g h i j) = k c <&> \c' -> (T10 a b c' d e f g h i j)
  {-# inline _3 #-}

instance Field3 (T11 a b c d e f g h i j kk) (T11 a b c' d e f g h i j kk) c c' where
  _3 k ~(T11 a b c d e f g h i j kk) = k c <&> \c' -> (T11 a b c' d e f g h i j kk)
  {-# inline _3 #-}

instance Field3 (T12 a b c d e f g h i j kk l) (T12 a b c' d e f g h i j kk l) c c' where
  _3 k ~(T12 a b c d e f g h i j kk l) = k c <&> \c' -> (T12 a b c' d e f g h i j kk l)
  {-# inline _3 #-}

instance Field3 (T13 a b c d e f g h i j kk l m) (T13 a b c' d e f g h i j kk l m) c c' where
  _3 k ~(T13 a b c d e f g h i j kk l m) = k c <&> \c' -> (T13 a b c' d e f g h i j kk l m)
  {-# inline _3 #-}

instance Field3 (T14 a b c d e f g h i j kk l m n) (T14 a b c' d e f g h i j kk l m n) c c' where
  _3 k ~(T14 a b c d e f g h i j kk l m n) = k c <&> \c' -> (T14 a b c' d e f g h i j kk l m n)
  {-# inline _3 #-}

instance Field3 (T15 a b c d e f g h i j kk l m n o) (T15 a b c' d e f g h i j kk l m n o) c c' where
  _3 k ~(T15 a b c d e f g h i j kk l m n o) = k c <&> \c' -> (T15 a b c' d e f g h i j kk l m n o)
  {-# inline _3 #-}

instance Field3 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c' d e f g h i j kk l m n o p) c c' where
  _3 k ~(T16 a b c d e f g h i j kk l m n o p) = k c <&> \c' -> (T16 a b c' d e f g h i j kk l m n o p)
  {-# inline _3 #-}

instance Field3 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c' d e f g h i j kk l m n o p q) c c' where
  _3 k ~(T17 a b c d e f g h i j kk l m n o p q) = k c <&> \c' -> (T17 a b c' d e f g h i j kk l m n o p q)
  {-# inline _3 #-}

instance Field3 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c' d e f g h i j kk l m n o p q r) c c' where
  _3 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k c <&> \c' -> (T18 a b c' d e f g h i j kk l m n o p q r)
  {-# inline _3 #-}

instance Field3 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c' d e f g h i j kk l m n o p q r s) c c' where
  _3 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k c <&> \c' -> (T19 a b c' d e f g h i j kk l m n o p q r s)
  {-# inline _3 #-}

-- ---------------------------------------------------------------- --
-- Field4 optics

instance Field4 (T4 a b c d) (T4 a b c d') d d' where
  _4 k ~(T4 a b c d) = k d <&> \d' -> (T4 a b c d')
  {-# inline _4 #-}

instance Field4 (T5 a b c d e) (T5 a b c d' e) d d' where
  _4 k ~(T5 a b c d e) = k d <&> \d' -> (T5 a b c d' e)
  {-# inline _4 #-}

instance Field4 (T6 a b c d e f) (T6 a b c d' e f) d d' where
  _4 k ~(T6 a b c d e f) = k d <&> \d' -> (T6 a b c d' e f)
  {-# inline _4 #-}

instance Field4 (T7 a b c d e f g) (T7 a b c d' e f g) d d' where
  _4 k ~(T7 a b c d e f g) = k d <&> \d' -> (T7 a b c d' e f g)
  {-# inline _4 #-}

instance Field4 (T8 a b c d e f g h) (T8 a b c d' e f g h) d d' where
  _4 k ~(T8 a b c d e f g h) = k d <&> \d' -> (T8 a b c d' e f g h)
  {-# inline _4 #-}

instance Field4 (T9 a b c d e f g h i) (T9 a b c d' e f g h i) d d' where
  _4 k ~(T9 a b c d e f g h i) = k d <&> \d' -> (T9 a b c d' e f g h i)
  {-# inline _4 #-}

instance Field4 (T10 a b c d e f g h i j) (T10 a b c d' e f g h i j) d d' where
  _4 k ~(T10 a b c d e f g h i j) = k d <&> \d' -> (T10 a b c d' e f g h i j)
  {-# inline _4 #-}

instance Field4 (T11 a b c d e f g h i j kk) (T11 a b c d' e f g h i j kk) d d' where
  _4 k ~(T11 a b c d e f g h i j kk) = k d <&> \d' -> (T11 a b c d' e f g h i j kk)
  {-# inline _4 #-}

instance Field4 (T12 a b c d e f g h i j kk l) (T12 a b c d' e f g h i j kk l) d d' where
  _4 k ~(T12 a b c d e f g h i j kk l) = k d <&> \d' -> (T12 a b c d' e f g h i j kk l)
  {-# inline _4 #-}

instance Field4 (T13 a b c d e f g h i j kk l m) (T13 a b c d' e f g h i j kk l m) d d' where
  _4 k ~(T13 a b c d e f g h i j kk l m) = k d <&> \d' -> (T13 a b c d' e f g h i j kk l m)
  {-# inline _4 #-}

instance Field4 (T14 a b c d e f g h i j kk l m n) (T14 a b c d' e f g h i j kk l m n) d d' where
  _4 k ~(T14 a b c d e f g h i j kk l m n) = k d <&> \d' -> (T14 a b c d' e f g h i j kk l m n)
  {-# inline _4 #-}

instance Field4 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d' e f g h i j kk l m n o) d d' where
  _4 k ~(T15 a b c d e f g h i j kk l m n o) = k d <&> \d' -> (T15 a b c d' e f g h i j kk l m n o)
  {-# inline _4 #-}

instance Field4 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d' e f g h i j kk l m n o p) d d' where
  _4 k ~(T16 a b c d e f g h i j kk l m n o p) = k d <&> \d' -> (T16 a b c d' e f g h i j kk l m n o p)
  {-# inline _4 #-}

instance Field4 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d' e f g h i j kk l m n o p q) d d' where
  _4 k ~(T17 a b c d e f g h i j kk l m n o p q) = k d <&> \d' -> (T17 a b c d' e f g h i j kk l m n o p q)
  {-# inline _4 #-}

instance Field4 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d' e f g h i j kk l m n o p q r) d d' where
  _4 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k d <&> \d' -> (T18 a b c d' e f g h i j kk l m n o p q r)
  {-# inline _4 #-}

instance Field4 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d' e f g h i j kk l m n o p q r s) d d' where
  _4 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k d <&> \d' -> (T19 a b c d' e f g h i j kk l m n o p q r s)
  {-# inline _4 #-}

-- ---------------------------------------------------------------- --
-- Field5 optics

instance Field5 (T5 a b c d e) (T5 a b c d e') e e' where
  _5 k ~(T5 a b c d e) = k e <&> \e' -> (T5 a b c d e')
  {-# inline _5 #-}

instance Field5 (T6 a b c d e f) (T6 a b c d e' f) e e' where
  _5 k ~(T6 a b c d e f) = k e <&> \e' -> (T6 a b c d e' f)
  {-# inline _5 #-}

instance Field5 (T7 a b c d e f g) (T7 a b c d e' f g) e e' where
  _5 k ~(T7 a b c d e f g) = k e <&> \e' -> (T7 a b c d e' f g)
  {-# inline _5 #-}

instance Field5 (T8 a b c d e f g h) (T8 a b c d e' f g h) e e' where
  _5 k ~(T8 a b c d e f g h) = k e <&> \e' -> (T8 a b c d e' f g h)
  {-# inline _5 #-}

instance Field5 (T9 a b c d e f g h i) (T9 a b c d e' f g h i) e e' where
  _5 k ~(T9 a b c d e f g h i) = k e <&> \e' -> (T9 a b c d e' f g h i)
  {-# inline _5 #-}

instance Field5 (T10 a b c d e f g h i j) (T10 a b c d e' f g h i j) e e' where
  _5 k ~(T10 a b c d e f g h i j) = k e <&> \e' -> (T10 a b c d e' f g h i j)
  {-# inline _5 #-}

instance Field5 (T11 a b c d e f g h i j kk) (T11 a b c d e' f g h i j kk) e e' where
  _5 k ~(T11 a b c d e f g h i j kk) = k e <&> \e' -> (T11 a b c d e' f g h i j kk)
  {-# inline _5 #-}

instance Field5 (T12 a b c d e f g h i j kk l) (T12 a b c d e' f g h i j kk l) e e' where
  _5 k ~(T12 a b c d e f g h i j kk l) = k e <&> \e' -> (T12 a b c d e' f g h i j kk l)
  {-# inline _5 #-}

instance Field5 (T13 a b c d e f g h i j kk l m) (T13 a b c d e' f g h i j kk l m) e e' where
  _5 k ~(T13 a b c d e f g h i j kk l m) = k e <&> \e' -> (T13 a b c d e' f g h i j kk l m)
  {-# inline _5 #-}

instance Field5 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e' f g h i j kk l m n) e e' where
  _5 k ~(T14 a b c d e f g h i j kk l m n) = k e <&> \e' -> (T14 a b c d e' f g h i j kk l m n)
  {-# inline _5 #-}

instance Field5 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e' f g h i j kk l m n o) e e' where
  _5 k ~(T15 a b c d e f g h i j kk l m n o) = k e <&> \e' -> (T15 a b c d e' f g h i j kk l m n o)
  {-# inline _5 #-}

instance Field5 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e' f g h i j kk l m n o p) e e' where
  _5 k ~(T16 a b c d e f g h i j kk l m n o p) = k e <&> \e' -> (T16 a b c d e' f g h i j kk l m n o p)
  {-# inline _5 #-}

instance Field5 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e' f g h i j kk l m n o p q) e e' where
  _5 k ~(T17 a b c d e f g h i j kk l m n o p q) = k e <&> \e' -> (T17 a b c d e' f g h i j kk l m n o p q)
  {-# inline _5 #-}

instance Field5 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e' f g h i j kk l m n o p q r) e e' where
  _5 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k e <&> \e' -> (T18 a b c d e' f g h i j kk l m n o p q r)
  {-# inline _5 #-}

instance Field5 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e' f g h i j kk l m n o p q r s) e e' where
  _5 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k e <&> \e' -> (T19 a b c d e' f g h i j kk l m n o p q r s)
  {-# inline _5 #-}

-- ---------------------------------------------------------------- --
-- Field6 optics

instance Field6 (T6 a b c d e f) (T6 a b c d e f') f f' where
  _6 k ~(T6 a b c d e f) = k f <&> \f' -> (T6 a b c d e f')
  {-# inline _6 #-}

instance Field6 (T7 a b c d e f g) (T7 a b c d e f' g) f f' where
  _6 k ~(T7 a b c d e f g) = k f <&> \f' -> (T7 a b c d e f' g)
  {-# inline _6 #-}

instance Field6 (T8 a b c d e f g h) (T8 a b c d e f' g h) f f' where
  _6 k ~(T8 a b c d e f g h) = k f <&> \f' -> (T8 a b c d e f' g h)
  {-# inline _6 #-}

instance Field6 (T9 a b c d e f g h i) (T9 a b c d e f' g h i) f f' where
  _6 k ~(T9 a b c d e f g h i) = k f <&> \f' -> (T9 a b c d e f' g h i)
  {-# inline _6 #-}

instance Field6 (T10 a b c d e f g h i j) (T10 a b c d e f' g h i j) f f' where
  _6 k ~(T10 a b c d e f g h i j) = k f <&> \f' -> (T10 a b c d e f' g h i j)
  {-# inline _6 #-}

instance Field6 (T11 a b c d e f g h i j kk) (T11 a b c d e f' g h i j kk) f f' where
  _6 k ~(T11 a b c d e f g h i j kk) = k f <&> \f' -> (T11 a b c d e f' g h i j kk)
  {-# inline _6 #-}

instance Field6 (T12 a b c d e f g h i j kk l) (T12 a b c d e f' g h i j kk l) f f' where
  _6 k ~(T12 a b c d e f g h i j kk l) = k f <&> \f' -> (T12 a b c d e f' g h i j kk l)
  {-# inline _6 #-}

instance Field6 (T13 a b c d e f g h i j kk l m) (T13 a b c d e f' g h i j kk l m) f f' where
  _6 k ~(T13 a b c d e f g h i j kk l m) = k f <&> \f' -> (T13 a b c d e f' g h i j kk l m)
  {-# inline _6 #-}

instance Field6 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f' g h i j kk l m n) f f' where
  _6 k ~(T14 a b c d e f g h i j kk l m n) = k f <&> \f' -> (T14 a b c d e f' g h i j kk l m n)
  {-# inline _6 #-}

instance Field6 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f' g h i j kk l m n o) f f' where
  _6 k ~(T15 a b c d e f g h i j kk l m n o) = k f <&> \f' -> (T15 a b c d e f' g h i j kk l m n o)
  {-# inline _6 #-}

instance Field6 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f' g h i j kk l m n o p) f f' where
  _6 k ~(T16 a b c d e f g h i j kk l m n o p) = k f <&> \f' -> (T16 a b c d e f' g h i j kk l m n o p)
  {-# inline _6 #-}

instance Field6 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f' g h i j kk l m n o p q) f f' where
  _6 k ~(T17 a b c d e f g h i j kk l m n o p q) = k f <&> \f' -> (T17 a b c d e f' g h i j kk l m n o p q)
  {-# inline _6 #-}

instance Field6 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f' g h i j kk l m n o p q r) f f' where
  _6 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k f <&> \f' -> (T18 a b c d e f' g h i j kk l m n o p q r)
  {-# inline _6 #-}

instance Field6 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f' g h i j kk l m n o p q r s) f f' where
  _6 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k f <&> \f' -> (T19 a b c d e f' g h i j kk l m n o p q r s)
  {-# inline _6 #-}

-- ---------------------------------------------------------------- --
-- Field7 optics

instance Field7 (T7 a b c d e f g) (T7 a b c d e f g') g g' where
  _7 k ~(T7 a b c d e f g) = k g <&> \g' -> (T7 a b c d e f g')
  {-# inline _7 #-}

instance Field7 (T8 a b c d e f g h) (T8 a b c d e f g' h) g g' where
  _7 k ~(T8 a b c d e f g h) = k g <&> \g' -> (T8 a b c d e f g' h)
  {-# inline _7 #-}

instance Field7 (T9 a b c d e f g h i) (T9 a b c d e f g' h i) g g' where
  _7 k ~(T9 a b c d e f g h i) = k g <&> \g' -> (T9 a b c d e f g' h i)
  {-# inline _7 #-}

instance Field7 (T10 a b c d e f g h i j) (T10 a b c d e f g' h i j) g g' where
  _7 k ~(T10 a b c d e f g h i j) = k g <&> \g' -> (T10 a b c d e f g' h i j)
  {-# inline _7 #-}

instance Field7 (T11 a b c d e f g h i j kk) (T11 a b c d e f g' h i j kk) g g' where
  _7 k ~(T11 a b c d e f g h i j kk) = k g <&> \g' -> (T11 a b c d e f g' h i j kk)
  {-# inline _7 #-}

instance Field7 (T12 a b c d e f g h i j kk l) (T12 a b c d e f g' h i j kk l) g g' where
  _7 k ~(T12 a b c d e f g h i j kk l) = k g <&> \g' -> (T12 a b c d e f g' h i j kk l)
  {-# inline _7 #-}

instance Field7 (T13 a b c d e f g h i j kk l m) (T13 a b c d e f g' h i j kk l m) g g' where
  _7 k ~(T13 a b c d e f g h i j kk l m) = k g <&> \g' -> (T13 a b c d e f g' h i j kk l m)
  {-# inline _7 #-}

instance Field7 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f g' h i j kk l m n) g g' where
  _7 k ~(T14 a b c d e f g h i j kk l m n) = k g <&> \g' -> (T14 a b c d e f g' h i j kk l m n)
  {-# inline _7 #-}

instance Field7 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g' h i j kk l m n o) g g' where
  _7 k ~(T15 a b c d e f g h i j kk l m n o) = k g <&> \g' -> (T15 a b c d e f g' h i j kk l m n o)
  {-# inline _7 #-}

instance Field7 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g' h i j kk l m n o p) g g' where
  _7 k ~(T16 a b c d e f g h i j kk l m n o p) = k g <&> \g' -> (T16 a b c d e f g' h i j kk l m n o p)
  {-# inline _7 #-}

instance Field7 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g' h i j kk l m n o p q) g g' where
  _7 k ~(T17 a b c d e f g h i j kk l m n o p q) = k g <&> \g' -> (T17 a b c d e f g' h i j kk l m n o p q)
  {-# inline _7 #-}

instance Field7 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g' h i j kk l m n o p q r) g g' where
  _7 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k g <&> \g' -> (T18 a b c d e f g' h i j kk l m n o p q r)
  {-# inline _7 #-}

instance Field7 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g' h i j kk l m n o p q r s) g g' where
  _7 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k g <&> \g' -> (T19 a b c d e f g' h i j kk l m n o p q r s)
  {-# inline _7 #-}

-- ---------------------------------------------------------------- --
-- Field8 optics

instance Field8 (T8 a b c d e f g h) (T8 a b c d e f g h') h h' where
  _8 k ~(T8 a b c d e f g h) = k h <&> \h' -> (T8 a b c d e f g h')
  {-# inline _8 #-}

instance Field8 (T9 a b c d e f g h i) (T9 a b c d e f g h' i) h h' where
  _8 k ~(T9 a b c d e f g h i) = k h <&> \h' -> (T9 a b c d e f g h' i)
  {-# inline _8 #-}

instance Field8 (T10 a b c d e f g h i j) (T10 a b c d e f g h' i j) h h' where
  _8 k ~(T10 a b c d e f g h i j) = k h <&> \h' -> (T10 a b c d e f g h' i j)
  {-# inline _8 #-}

instance Field8 (T11 a b c d e f g h i j kk) (T11 a b c d e f g h' i j kk) h h' where
  _8 k ~(T11 a b c d e f g h i j kk) = k h <&> \h' -> (T11 a b c d e f g h' i j kk)
  {-# inline _8 #-}

instance Field8 (T12 a b c d e f g h i j kk l) (T12 a b c d e f g h' i j kk l) h h' where
  _8 k ~(T12 a b c d e f g h i j kk l) = k h <&> \h' -> (T12 a b c d e f g h' i j kk l)
  {-# inline _8 #-}

instance Field8 (T13 a b c d e f g h i j kk l m) (T13 a b c d e f g h' i j kk l m) h h' where
  _8 k ~(T13 a b c d e f g h i j kk l m) = k h <&> \h' -> (T13 a b c d e f g h' i j kk l m)
  {-# inline _8 #-}

instance Field8 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f g h' i j kk l m n) h h' where
  _8 k ~(T14 a b c d e f g h i j kk l m n) = k h <&> \h' -> (T14 a b c d e f g h' i j kk l m n)
  {-# inline _8 #-}

instance Field8 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g h' i j kk l m n o) h h' where
  _8 k ~(T15 a b c d e f g h i j kk l m n o) = k h <&> \h' -> (T15 a b c d e f g h' i j kk l m n o)
  {-# inline _8 #-}

instance Field8 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h' i j kk l m n o p) h h' where
  _8 k ~(T16 a b c d e f g h i j kk l m n o p) = k h <&> \h' -> (T16 a b c d e f g h' i j kk l m n o p)
  {-# inline _8 #-}

instance Field8 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h' i j kk l m n o p q) h h' where
  _8 k ~(T17 a b c d e f g h i j kk l m n o p q) = k h <&> \h' -> (T17 a b c d e f g h' i j kk l m n o p q)
  {-# inline _8 #-}

instance Field8 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h' i j kk l m n o p q r) h h' where
  _8 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k h <&> \h' -> (T18 a b c d e f g h' i j kk l m n o p q r)
  {-# inline _8 #-}

instance Field8 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h' i j kk l m n o p q r s) h h' where
  _8 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k h <&> \h' -> (T19 a b c d e f g h' i j kk l m n o p q r s)
  {-# inline _8 #-}

-- ---------------------------------------------------------------- --
-- Field9 optics

instance Field9 (T9 a b c d e f g h i) (T9 a b c d e f g h i') i i' where
  _9 k ~(T9 a b c d e f g h i) = k i <&> \i' -> (T9 a b c d e f g h i')
  {-# inline _9 #-}

instance Field9 (T10 a b c d e f g h i j) (T10 a b c d e f g h i' j) i i' where
  _9 k ~(T10 a b c d e f g h i j) = k i <&> \i' -> (T10 a b c d e f g h i' j)
  {-# inline _9 #-}

instance Field9 (T11 a b c d e f g h i j kk) (T11 a b c d e f g h i' j kk) i i' where
  _9 k ~(T11 a b c d e f g h i j kk) = k i <&> \i' -> (T11 a b c d e f g h i' j kk)
  {-# inline _9 #-}

instance Field9 (T12 a b c d e f g h i j kk l) (T12 a b c d e f g h i' j kk l) i i' where
  _9 k ~(T12 a b c d e f g h i j kk l) = k i <&> \i' -> (T12 a b c d e f g h i' j kk l)
  {-# inline _9 #-}

instance Field9 (T13 a b c d e f g h i j kk l m) (T13 a b c d e f g h i' j kk l m) i i' where
  _9 k ~(T13 a b c d e f g h i j kk l m) = k i <&> \i' -> (T13 a b c d e f g h i' j kk l m)
  {-# inline _9 #-}

instance Field9 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f g h i' j kk l m n) i i' where
  _9 k ~(T14 a b c d e f g h i j kk l m n) = k i <&> \i' -> (T14 a b c d e f g h i' j kk l m n)
  {-# inline _9 #-}

instance Field9 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g h i' j kk l m n o) i i' where
  _9 k ~(T15 a b c d e f g h i j kk l m n o) = k i <&> \i' -> (T15 a b c d e f g h i' j kk l m n o)
  {-# inline _9 #-}

instance Field9 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h i' j kk l m n o p) i i' where
  _9 k ~(T16 a b c d e f g h i j kk l m n o p) = k i <&> \i' -> (T16 a b c d e f g h i' j kk l m n o p)
  {-# inline _9 #-}

instance Field9 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i' j kk l m n o p q) i i' where
  _9 k ~(T17 a b c d e f g h i j kk l m n o p q) = k i <&> \i' -> (T17 a b c d e f g h i' j kk l m n o p q)
  {-# inline _9 #-}

instance Field9 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i' j kk l m n o p q r) i i' where
  _9 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k i <&> \i' -> (T18 a b c d e f g h i' j kk l m n o p q r)
  {-# inline _9 #-}

instance Field9 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i' j kk l m n o p q r s) i i' where
  _9 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k i <&> \i' -> (T19 a b c d e f g h i' j kk l m n o p q r s)
  {-# inline _9 #-}

-- ---------------------------------------------------------------- --
-- Field10 optics

instance Field10 (T10 a b c d e f g h i j) (T10 a b c d e f g h i j') j j' where
  _10 k ~(T10 a b c d e f g h i j) = k j <&> \j' -> (T10 a b c d e f g h i j')
  {-# inline _10 #-}

instance Field10 (T11 a b c d e f g h i j kk) (T11 a b c d e f g h i j' kk) j j' where
  _10 k ~(T11 a b c d e f g h i j kk) = k j <&> \j' -> (T11 a b c d e f g h i j' kk)
  {-# inline _10 #-}

instance Field10 (T12 a b c d e f g h i j kk l) (T12 a b c d e f g h i j' kk l) j j' where
  _10 k ~(T12 a b c d e f g h i j kk l) = k j <&> \j' -> (T12 a b c d e f g h i j' kk l)
  {-# inline _10 #-}

instance Field10 (T13 a b c d e f g h i j kk l m) (T13 a b c d e f g h i j' kk l m) j j' where
  _10 k ~(T13 a b c d e f g h i j kk l m) = k j <&> \j' -> (T13 a b c d e f g h i j' kk l m)
  {-# inline _10 #-}

instance Field10 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f g h i j' kk l m n) j j' where
  _10 k ~(T14 a b c d e f g h i j kk l m n) = k j <&> \j' -> (T14 a b c d e f g h i j' kk l m n)
  {-# inline _10 #-}

instance Field10 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g h i j' kk l m n o) j j' where
  _10 k ~(T15 a b c d e f g h i j kk l m n o) = k j <&> \j' -> (T15 a b c d e f g h i j' kk l m n o)
  {-# inline _10 #-}

instance Field10 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h i j' kk l m n o p) j j' where
  _10 k ~(T16 a b c d e f g h i j kk l m n o p) = k j <&> \j' -> (T16 a b c d e f g h i j' kk l m n o p)
  {-# inline _10 #-}

instance Field10 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i j' kk l m n o p q) j j' where
  _10 k ~(T17 a b c d e f g h i j kk l m n o p q) = k j <&> \j' -> (T17 a b c d e f g h i j' kk l m n o p q)
  {-# inline _10 #-}

instance Field10 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j' kk l m n o p q r) j j' where
  _10 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k j <&> \j' -> (T18 a b c d e f g h i j' kk l m n o p q r)
  {-# inline _10 #-}

instance Field10 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j' kk l m n o p q r s) j j' where
  _10 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k j <&> \j' -> (T19 a b c d e f g h i j' kk l m n o p q r s)
  {-# inline _10 #-}

-- ---------------------------------------------------------------- --
-- Field11 optics

instance Field11 (T11 a b c d e f g h i j kk) (T11 a b c d e f g h i j kk') kk kk' where
  _11 k ~(T11 a b c d e f g h i j kk) = k kk <&> \kk' -> (T11 a b c d e f g h i j kk')
  {-# inline _11 #-}

instance Field11 (T12 a b c d e f g h i j kk l) (T12 a b c d e f g h i j kk' l) kk kk' where
  _11 k ~(T12 a b c d e f g h i j kk l) = k kk <&> \kk' -> (T12 a b c d e f g h i j kk' l)
  {-# inline _11 #-}

instance Field11 (T13 a b c d e f g h i j kk l m) (T13 a b c d e f g h i j kk' l m) kk kk' where
  _11 k ~(T13 a b c d e f g h i j kk l m) = k kk <&> \kk' -> (T13 a b c d e f g h i j kk' l m)
  {-# inline _11 #-}

instance Field11 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f g h i j kk' l m n) kk kk' where
  _11 k ~(T14 a b c d e f g h i j kk l m n) = k kk <&> \kk' -> (T14 a b c d e f g h i j kk' l m n)
  {-# inline _11 #-}

instance Field11 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g h i j kk' l m n o) kk kk' where
  _11 k ~(T15 a b c d e f g h i j kk l m n o) = k kk <&> \kk' -> (T15 a b c d e f g h i j kk' l m n o)
  {-# inline _11 #-}

instance Field11 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h i j kk' l m n o p) kk kk' where
  _11 k ~(T16 a b c d e f g h i j kk l m n o p) = k kk <&> \kk' -> (T16 a b c d e f g h i j kk' l m n o p)
  {-# inline _11 #-}

instance Field11 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i j kk' l m n o p q) kk kk' where
  _11 k ~(T17 a b c d e f g h i j kk l m n o p q) = k kk <&> \kk' -> (T17 a b c d e f g h i j kk' l m n o p q)
  {-# inline _11 #-}

instance Field11 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j kk' l m n o p q r) kk kk' where
  _11 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k kk <&> \kk' -> (T18 a b c d e f g h i j kk' l m n o p q r)
  {-# inline _11 #-}

instance Field11 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk' l m n o p q r s) kk kk' where
  _11 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k kk <&> \kk' -> (T19 a b c d e f g h i j kk' l m n o p q r s)
  {-# inline _11 #-}

-- ---------------------------------------------------------------- --
-- Field12 optics

instance Field12 (T12 a b c d e f g h i j kk l) (T12 a b c d e f g h i j kk l') l l' where
  _12 k ~(T12 a b c d e f g h i j kk l) = k l <&> \l' -> (T12 a b c d e f g h i j kk l')
  {-# inline _12 #-}

instance Field12 (T13 a b c d e f g h i j kk l m) (T13 a b c d e f g h i j kk l' m) l l' where
  _12 k ~(T13 a b c d e f g h i j kk l m) = k l <&> \l' -> (T13 a b c d e f g h i j kk l' m)
  {-# inline _12 #-}

instance Field12 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f g h i j kk l' m n) l l' where
  _12 k ~(T14 a b c d e f g h i j kk l m n) = k l <&> \l' -> (T14 a b c d e f g h i j kk l' m n)
  {-# inline _12 #-}

instance Field12 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g h i j kk l' m n o) l l' where
  _12 k ~(T15 a b c d e f g h i j kk l m n o) = k l <&> \l' -> (T15 a b c d e f g h i j kk l' m n o)
  {-# inline _12 #-}

instance Field12 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h i j kk l' m n o p) l l' where
  _12 k ~(T16 a b c d e f g h i j kk l m n o p) = k l <&> \l' -> (T16 a b c d e f g h i j kk l' m n o p)
  {-# inline _12 #-}

instance Field12 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i j kk l' m n o p q) l l' where
  _12 k ~(T17 a b c d e f g h i j kk l m n o p q) = k l <&> \l' -> (T17 a b c d e f g h i j kk l' m n o p q)
  {-# inline _12 #-}

instance Field12 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j kk l' m n o p q r) l l' where
  _12 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k l <&> \l' -> (T18 a b c d e f g h i j kk l' m n o p q r)
  {-# inline _12 #-}

instance Field12 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk l' m n o p q r s) l l' where
  _12 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k l <&> \l' -> (T19 a b c d e f g h i j kk l' m n o p q r s)
  {-# inline _12 #-}

-- ---------------------------------------------------------------- --
-- Field13 optics

instance Field13 (T13 a b c d e f g h i j kk l m) (T13 a b c d e f g h i j kk l m') m m' where
  _13 k ~(T13 a b c d e f g h i j kk l m) = k m <&> \m' -> (T13 a b c d e f g h i j kk l m')
  {-# inline _13 #-}

instance Field13 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f g h i j kk l m' n) m m' where
  _13 k ~(T14 a b c d e f g h i j kk l m n) = k m <&> \m' -> (T14 a b c d e f g h i j kk l m' n)
  {-# inline _13 #-}

instance Field13 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g h i j kk l m' n o) m m' where
  _13 k ~(T15 a b c d e f g h i j kk l m n o) = k m <&> \m' -> (T15 a b c d e f g h i j kk l m' n o)
  {-# inline _13 #-}

instance Field13 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h i j kk l m' n o p) m m' where
  _13 k ~(T16 a b c d e f g h i j kk l m n o p) = k m <&> \m' -> (T16 a b c d e f g h i j kk l m' n o p)
  {-# inline _13 #-}

instance Field13 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i j kk l m' n o p q) m m' where
  _13 k ~(T17 a b c d e f g h i j kk l m n o p q) = k m <&> \m' -> (T17 a b c d e f g h i j kk l m' n o p q)
  {-# inline _13 #-}

instance Field13 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j kk l m' n o p q r) m m' where
  _13 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k m <&> \m' -> (T18 a b c d e f g h i j kk l m' n o p q r)
  {-# inline _13 #-}

instance Field13 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk l m' n o p q r s) m m' where
  _13 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k m <&> \m' -> (T19 a b c d e f g h i j kk l m' n o p q r s)
  {-# inline _13 #-}

-- ---------------------------------------------------------------- --
-- Field14 optics

instance Field14 (T14 a b c d e f g h i j kk l m n) (T14 a b c d e f g h i j kk l m n') n n' where
  _14 k ~(T14 a b c d e f g h i j kk l m n) = k n <&> \n' -> (T14 a b c d e f g h i j kk l m n')
  {-# inline _14 #-}

instance Field14 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g h i j kk l m n' o) n n' where
  _14 k ~(T15 a b c d e f g h i j kk l m n o) = k n <&> \n' -> (T15 a b c d e f g h i j kk l m n' o)
  {-# inline _14 #-}

instance Field14 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h i j kk l m n' o p) n n' where
  _14 k ~(T16 a b c d e f g h i j kk l m n o p) = k n <&> \n' -> (T16 a b c d e f g h i j kk l m n' o p)
  {-# inline _14 #-}

instance Field14 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i j kk l m n' o p q) n n' where
  _14 k ~(T17 a b c d e f g h i j kk l m n o p q) = k n <&> \n' -> (T17 a b c d e f g h i j kk l m n' o p q)
  {-# inline _14 #-}

instance Field14 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j kk l m n' o p q r) n n' where
  _14 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k n <&> \n' -> (T18 a b c d e f g h i j kk l m n' o p q r)
  {-# inline _14 #-}

instance Field14 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk l m n' o p q r s) n n' where
  _14 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k n <&> \n' -> (T19 a b c d e f g h i j kk l m n' o p q r s)
  {-# inline _14 #-}

-- ---------------------------------------------------------------- --
-- Field15 optics

instance Field15 (T15 a b c d e f g h i j kk l m n o) (T15 a b c d e f g h i j kk l m n o') o o' where
  _15 k ~(T15 a b c d e f g h i j kk l m n o) = k o <&> \o' -> (T15 a b c d e f g h i j kk l m n o')
  {-# inline _15 #-}

instance Field15 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h i j kk l m n o' p) o o' where
  _15 k ~(T16 a b c d e f g h i j kk l m n o p) = k o <&> \o' -> (T16 a b c d e f g h i j kk l m n o' p)
  {-# inline _15 #-}

instance Field15 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i j kk l m n o' p q) o o' where
  _15 k ~(T17 a b c d e f g h i j kk l m n o p q) = k o <&> \o' -> (T17 a b c d e f g h i j kk l m n o' p q)
  {-# inline _15 #-}

instance Field15 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j kk l m n o' p q r) o o' where
  _15 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k o <&> \o' -> (T18 a b c d e f g h i j kk l m n o' p q r)
  {-# inline _15 #-}

instance Field15 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk l m n o' p q r s) o o' where
  _15 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k o <&> \o' -> (T19 a b c d e f g h i j kk l m n o' p q r s)
  {-# inline _15 #-}

-- ---------------------------------------------------------------- --
-- Field16 optics

instance Field16 (T16 a b c d e f g h i j kk l m n o p) (T16 a b c d e f g h i j kk l m n o p') p p' where
  _16 k ~(T16 a b c d e f g h i j kk l m n o p) = k p <&> \p' -> (T16 a b c d e f g h i j kk l m n o p')
  {-# inline _16 #-}

instance Field16 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i j kk l m n o p' q) p p' where
  _16 k ~(T17 a b c d e f g h i j kk l m n o p q) = k p <&> \p' -> (T17 a b c d e f g h i j kk l m n o p' q)
  {-# inline _16 #-}

instance Field16 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j kk l m n o p' q r) p p' where
  _16 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k p <&> \p' -> (T18 a b c d e f g h i j kk l m n o p' q r)
  {-# inline _16 #-}

instance Field16 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk l m n o p' q r s) p p' where
  _16 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k p <&> \p' -> (T19 a b c d e f g h i j kk l m n o p' q r s)
  {-# inline _16 #-}

-- ---------------------------------------------------------------- --
-- Field17 optics

instance Field17 (T17 a b c d e f g h i j kk l m n o p q) (T17 a b c d e f g h i j kk l m n o p q') q q' where
  _17 k ~(T17 a b c d e f g h i j kk l m n o p q) = k q <&> \q' -> (T17 a b c d e f g h i j kk l m n o p q')
  {-# inline _17 #-}

instance Field17 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j kk l m n o p q' r) q q' where
  _17 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k q <&> \q' -> (T18 a b c d e f g h i j kk l m n o p q' r)
  {-# inline _17 #-}

instance Field17 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk l m n o p q' r s) q q' where
  _17 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k q <&> \q' -> (T19 a b c d e f g h i j kk l m n o p q' r s)
  {-# inline _17 #-}

-- ---------------------------------------------------------------- --
-- Field18 optics

instance Field18 (T18 a b c d e f g h i j kk l m n o p q r) (T18 a b c d e f g h i j kk l m n o p q r') r r' where
  _18 k ~(T18 a b c d e f g h i j kk l m n o p q r) = k r <&> \r' -> (T18 a b c d e f g h i j kk l m n o p q r')
  {-# inline _18 #-}

instance Field18 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk l m n o p q r' s) r r' where
  _18 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k r <&> \r' -> (T19 a b c d e f g h i j kk l m n o p q r' s)
  {-# inline _18 #-}

-- ---------------------------------------------------------------- --
-- Field19 optics

instance Field19 (T19 a b c d e f g h i j kk l m n o p q r s) (T19 a b c d e f g h i j kk l m n o p q r s') s s' where
  _19 k ~(T19 a b c d e f g h i j kk l m n o p q r s) = k s <&> \s' -> (T19 a b c d e f g h i j kk l m n o p q r s')
  {-# inline _19 #-}
