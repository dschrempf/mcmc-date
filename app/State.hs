{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  State
-- Description :  State space
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Jul 13 13:57:09 2021.
module State where

import Control.Lens
import Data.Aeson
import GHC.Generics
import Mcmc.Tree

-- | State space containing all parameters.
--
-- We are interested in inferring an ultrametric time tree with branch lengths
-- measured in units of time (e.g., in million years). Let i be a branch of the
-- time tree. Further, let T_i be the length of branch i, and R_i be the
-- absolute evolutionary rate on this branch. Then, the length of branch i
-- measured in average number of substitutions is d_i=T_i*R_i.
--
-- Internally, a relative time t_i and relative rate r_i are stored and used
-- such that the branch length measured in average number of substitution is
-- d_i=T_i*R_i=(t_i*h)*(r_i*mu), where h is the root height of the time tree,
-- and mu is the mean rate.
--
-- In brief, the relative time and rate are defined as t_i=T_i/h, and
-- r_i=R_i/mu.
--
-- This has various advantages:
--
-- 1. The ultrametric tree object storing the relative times is a normalized
--    tree with root height 1.0.
--
-- 2. The relative rates have a mean of 1.0.
--
-- 3. The absolute times and rates can be scaled easily by proposing new values
--    for h or mu.
--
-- NOTE: The relative times and rates are stored using two separate tree
-- objects: (1) An ultrametric time tree, and (2) an unconstrained rate tree.
-- The separation of the two trees allows usage of two different types:
--
-- (1) The time tree is of type 'HeightTree' because the ultrametricity
--     constraint allows the change of node heights only.
--
-- (2) The rate tree is of type 'Tree' because the branch lengths are not
--     limited by any other constraints than being positive.
--
-- Accordingly, the types of the proposals ensure that they are used on the
-- correct tree objects.
--
-- NOTE: Absolute times can only be inferred if node calibrations are available.
-- Otherwise, the time tree height will be left unchanged at 1.0, and relative
-- times will be inferred.
--
-- NOTE: The topologies of the time and rate trees are equal. This is, however,
-- not ensured by the types. Equality of the topology could be ensured by using
-- one tree storing both, the times and the rates.
data IG a = I
  { -- | Hyper-parameter. Birth rate of relative time tree.
    _timeBirthRate :: a,
    -- | Hyper-parameter. Death rate of relative time tree.
    _timeDeathRate :: a,
    -- | Height of absolute time tree in unit time. Normalization factor of
    -- relative time. Here, we use units of million years; see the
    -- calibrations.
    _timeHeight :: a,
    -- | Normalized time tree of height 1.0. Branch labels denote relative
    -- node heights.
    _timeTree :: HeightTree a,
    -- | Mean of the absolute rates. Normalization factor of relative rates.
    _rateMean :: a,
    -- | Hyper-parameter. The variance of the relative rates.
    _rateVariance :: a,
    -- | Relative rate tree. Branch labels denote relative rates with mean 1.0.
    _rateTree :: LengthTree a
  }
  deriving (Eq, Generic)

type I = IG Double

-- Create accessors (lenses) to the parameters in the state space.
makeLenses ''IG

deriving instance Functor IG

deriving instance Foldable IG

deriving instance Traversable IG

-- Allow storage of the trace as JSON.
instance ToJSON a => ToJSON (IG a)

instance FromJSON a => FromJSON (IG a)

-- | Check if a state is valid.
isValidState :: I -> Bool
isValidState (I l m h t mu va r) =
  and
    [ l > 0,
      m > 0,
      h > 0,
      isValidHeightTree t,
      mu > 0,
      va > 0,
      isValidLengthTree r
    ]

-- where
--   f = toNewick . lengthToPhyloTree . setLengths . getLengthTree . heightTreeToLengthTree
--   setLengths = first (errF . toLength)
--   -- Call 'error' when branch lengths are negative. One could use 'assert'
--   -- here. But then, the checks are not too time intense, and provide some
--   -- security also in production.
--   errF = either (error . ("monitorLengthTree: " <>)) id
