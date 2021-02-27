{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Tuple.Strict.Lens.Empty
-- Copyright    : (c) 2020-2021 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : FlexibleContexts, MPTC
--
-- 'Control.Lens.Empty.AsEmpty' instances for 'T2' and 'T3'
--
module Data.Tuple.Strict.Lens.Empty
( AsEmpty(..)
) where


import Control.Lens

import Data.Tuple.Strict


instance (AsEmpty a, AsEmpty b) => AsEmpty (T2 a b) where
  _Empty = prism' (\() -> T2 (_Empty # ()) (_Empty # ())) $ \(T2 s s') -> case _Empty Left s of
    Left () -> case _Empty Left s' of
      Left () -> Just ()
      _ -> Nothing
    _ -> Nothing
  {-# inline _Empty #-}

instance (AsEmpty a, AsEmpty b, AsEmpty c) => AsEmpty (T3 a b c) where
  _Empty = prism' (\() -> T3 (_Empty # ()) (_Empty # ()) (_Empty # ())) $ \(T3 s s' s'') -> case _Empty Left s of
    Left () -> case _Empty Left s' of
      Left () -> case _Empty Left s'' of
        Left () -> Just ()
        Right _ -> Nothing
      Right _ -> Nothing
    Right _ -> Nothing
  {-# inline _Empty #-}
