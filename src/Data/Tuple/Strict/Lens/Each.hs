{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
-- |
-- Module       : Data.Tuple.Strict.Lens.Each
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- 'Control.Lens.Each.Each' instances for 'T1' through 'T9'
--
module Data.Tuple.Strict.Lens.Each where


import Control.Lens

import Data.Tuple.Strict


instance (a~a') => Each (T1 a) (T1 a') a a' where
  each f ~(T1 a) = T1 <$> f a

instance (a~a', b~b') => Each (T2 a a') (T2 b b') a b where
  each f ~(T2 a b) = T2 <$> f a <*> f b
  {-# inline each #-}

instance (a~a2, a~a3, b~b2, b~b3) => Each (T3 a a2 a3) (T3 b b2 b3) a b where
  each f ~(T3 a b c) = T3 <$> f a <*> f b <*> f c
  {-# inline each #-}

instance (a~a2, a~a3, a~a4, b~b2, b~b3, b~b4) => Each (T4 a a2 a3 a4) (T4 b b2 b3 b4) a b where
  each f ~(T4 a b c d) = T4 <$> f a <*> f b <*> f c <*> f d
  {-# inline each #-}

instance (a~a2, a~a3, a~a4, a~a5, b~b2, b~b3, b~b4, b~b5) => Each (T5 a a2 a3 a4 a5) (T5 b b2 b3 b4 b5) a b where
  each f ~(T5 a b c d e) = T5 <$> f a <*> f b <*> f c <*> f d <*> f e
  {-# inline each #-}

instance (a~a2, a~a3, a~a4, a~a5, a~a6, b~b2, b~b3, b~b4, b~b5, b~b6) => Each (T6 a a2 a3 a4 a5 a6) (T6 b b2 b3 b4 b5 b6) a b where
  each f ~(T6 a b c d e g) = T6 <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g
  {-# inline each #-}

instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7) => Each (T7 a a2 a3 a4 a5 a6 a7) (T7 b b2 b3 b4 b5 b6 b7) a b where
  each f ~(T7 a b c d e g h) = T7 <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h
  {-# inline each #-}

instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8) => Each (T8 a a2 a3 a4 a5 a6 a7 a8) (T8 b b2 b3 b4 b5 b6 b7 b8) a b where
  each f ~(T8 a b c d e g h i) = T8 <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i
  {-# inline each #-}

instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, a~a9, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8, b~b9) => Each (T9 a a2 a3 a4 a5 a6 a7 a8 a9) (T9 b b2 b3 b4 b5 b6 b7 b8 b9) a b where
  each f ~(T9 a b c d e g h i j) = T9 <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i <*> f j
  {-# inline each #-}
