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
  ( truncatedNormalSample,
  )
where

import Control.Monad
import Mcmc.Proposal
import Mcmc.Statistics.Types
import Numeric.Log hiding (sum)
import Statistics.Distribution hiding (Mean)
import Statistics.Distribution.TruncatedNormal
import System.Random.MWC

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
  -- | (NewValue, MHGRatioWithoutJacobian)
  IO (Double, Log Double)
truncatedNormalSample m s t a b g = do
  let s' = t * s
      d = truncatedNormalDistr m s' a b
  u <- genContinuous d g
  let -- msgMeanLessThanZero = "Mean " <> show m <> " is negative."
      msgOutOfBounds = "Value " <> show u <> " out of bounds [" <> show a <> "," <> show b <> "]."
      -- msgValueLessThanZero = "Value " <> show u <> " is negative."
      msgParams = "Mean, sd, and tuneparam: " <> show m <> " " <> show s <> " " <> show t <> "."
      errWith msg = error $ "truncatedNormalSample: " <> msg <> "\n" <> msgParams
  -- -- NOTE: Pulleys allow negative values.
  -- when (m < 0) $ errWith msgMeanLessThanZero
  when (a > u || b < u) $ errWith msgOutOfBounds
  -- when (u < 0) $ errWith msgValueLessThanZero
  -- Compute Metropolis-Hastings-Green factor.
  let d' = truncatedNormalDistr u s' a b
      qXY = Exp $ logDensity d u
      qYX = Exp $ logDensity d' m
  -- Do not compute Jacobian.
  return (u, qYX / qXY)
