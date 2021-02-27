{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Tuple.Strict.Lens
-- Copyright    : (c) 2020-2021 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module exports all instances defined for strict tuples.
--
-- Instances include:
--
--  - 'Control.Lens.Each'
--  - 'Control.Lens.Empty'
--  - 'Control.Lens.Field'
--
module Data.Tuple.Strict.Lens
( module Data.Tuple.Strict.Lens.Each
, module Data.Tuple.Strict.Lens.Empty
, module Data.Tuple.Strict.Lens.Field
) where


import Data.Tuple.Strict.Lens.Each
import Data.Tuple.Strict.Lens.Empty
import Data.Tuple.Strict.Lens.Field
