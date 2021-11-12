-- |
-- Module      :  Mcmc.Tree.Proposal.Internal
-- Description :  Common functions used by all tree proposals
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed Nov  4 11:53:16 2020.
module Mcmc.Tree.Proposal.Internal
  ( assertWith,
    HeightBoundaryData (..),
    getHeightBoundaries,
    nInnerNodes,
    truncatedNormalSample,
    scaleUltrametricTreeF,
  )
where

import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Maybe
import ELynx.Tree
import Mcmc.Proposal
import Mcmc.Statistics.Types
import Mcmc.Tree.Types
import Numeric.Log hiding (sum)
import Statistics.Distribution hiding (Mean)
import Statistics.Distribution.TruncatedNormal
import System.Random.MWC

data HeightBoundaryData = HeightBoundaryData
  { hbdTreePosition :: TreePos Double Name,
    hbdNodeHeight :: Double,
    hbdChildrenHeights :: [Double],
    hbdMaximumChildrenHeight :: Double,
    hbdParentHeight :: Double
  }

-- | Assert predicate.
assertWith :: (a -> Bool) -> a -> a
assertWith f x = assert (f x) x
{-# INLINE assertWith #-}

-- Calculate boundaries for sliding a node at given path.
--
-- The other values are returned for computation efficiency.
--
-- Call 'error' with given function name if:
--
-- - The path is invalid.
--
-- - The path leads to a leaf.
getHeightBoundaries ::
  String ->
  HeightTree Double ->
  Path ->
  HeightBoundaryData
getHeightBoundaries n t p
  | null children =
    error $ "getHeightBoundaries: " <> n <> ": Path leads to a leaf: " <> show p <> "."
  | otherwise = HeightBoundaryData position hNode hsChildren hChild hParent
  where
    position =
      fromMaybe
        (error $ "getHeightBoundaries: " <> n <> ": Path is invalid: " <> show p <> ".")
        (goPath p $ fromTree $ getHeightTree t)
    focus = current position
    hNode = branch focus
    children = forest focus
    hsChildren = map branch children
    -- Above, we call 'error' with a meaningful message if @null children@.
    hChild = maximum hsChildren
    -- Set the upper bound to @+Infinity@ if no parent node exists.
    hParent = maybe (1 / 0) (branch . current) (goParent position)

-- | Calculate the number of inner nodes.
nInnerNodes :: Tree e a -> Int
nInnerNodes (Node _ _ []) = 0
nInnerNodes tr = 1 + sum (map nInnerNodes $ forest tr)

-- A very specific function scaling an ultrametric tree.
--
-- NOTE: Also scale leaf heights. This may be unintuitive, when leaf heights are
-- non-zero.
scaleUltrametricTreeF ::
  -- | New root node height.
  Double ->
  -- | Scaling factor for other nodes. The scaling factor for inner node heights
  -- is also given, since it is calculated anyways by the calling functions.
  Double ->
  Tree Double Name ->
  Tree Double Name
scaleUltrametricTreeF h xi (Node _ lb ts) =
  Node h lb $ map (first (* xi)) ts

-- A very specific function that samples a delta value from the truncated normal
-- distribution with given bounds [a,b] and also computes the required factor of
-- the Metropolis-Hastings-Green (MHG) proposal ratio.
--
-- NOTE: No Jacobian is computed, because it is not known how the proposal will
-- be used.
truncatedNormalSample ::
  Mean Double ->
  StandardDeviation Double ->
  TuningParameter ->
  LowerBoundary Double ->
  UpperBoundary Double ->
  GenIO ->
  -- (NewValue, MHGRatioWithoutJacobian)
  IO (Double, Log Double)
truncatedNormalSample m s t a b g = do
  let s' = t * s
      d = truncatedNormalDistr m s' a b
  u <- genContinuous d g
  let msgOutOfBounds = "Value " <> show u <> " out of bounds [" <> show a <> "," <> show b <> "]."
      -- msgMeanLessThanZero = "Mean " <> show m <> " is negative."
      -- msgValueLessThanZero = "Value " <> show u <> " is negative."
      msgParams = "Mean, sd, and tuneparam: " <> show m <> " " <> show s <> " " <> show t <> "."
      errWith msg = error $ "truncatedNormalSample: " <> msg <> "\n" <> msgParams
  -- NOTE: With the following error check, we catch numerical errors. This check
  -- is also required in production, and 'assert' is not used on purpose.
  when (a > u || b < u) $ errWith msgOutOfBounds
  -- -- NOTE: Pulleys allow negative values.
  --
  -- when (m < 0) $ errWith msgMeanLessThanZero
  -- when (u < 0) $ errWith msgValueLessThanZero
  --
  -- Compute Metropolis-Hastings-Green factor.
  let d' = truncatedNormalDistr u s' a b
      qXY = Exp $ logDensity d u
      qYX = Exp $ logDensity d' m
  -- Do not compute Jacobian.
  return (u, qYX / qXY)
