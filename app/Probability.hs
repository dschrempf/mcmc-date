-- |
-- Module      :  Probability
-- Description :  Prior and likelihood functions
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Jul 13 11:02:03 2021.
module Probability
  ( priorFunction,
    gradLogPriorFunction,
  )
where

import qualified Data.Vector as VB
import Numeric.AD
import Numeric.Log

{- ORMOLU_DISABLE -}
import Mcmc
import Mcmc.Tree
import State
{- ORMOLU_ENABLE -}

-- | Prior function.
priorFunction ::
  (RealFloat a, Show a) =>
  VB.Vector (Calibration Double) ->
  VB.Vector Constraint ->
  PriorFunctionG (IG a) a
priorFunction cb cs (IG l m h t mu va r) =
  product' $
    calibrateAndConstrain cb 1e-4 h cs 1e-4 t :
    -- -- Usually, the combined treatment is faster.
    -- calibrate 1e-4 cb h t :
    -- constrain 1e-4 cs t :
    [ -- Birth and death rates of the relative time tree.
      exponential 1 l,
      exponential 1 m,
      -- No explicit prior on the height of the time tree. However, the height
      -- is calibrated (see above). If no calibrations are given, the height is
      -- set to 1.0.
      --
      -- Relative time tree.
      birthDeath ConditionOnTimeOfMrca l m 1.0 t',
      -- Mean rate.
      --
      -- IDEA: Use gamma distribution with mean calculated using the number of
      -- branches and the total length of the substitution-like tree.
      exponential 1 mu,
      -- Variance of the relative rates.
      exponential 1 va,
      -- Relative rate tree.
      uncorrelatedGamma WithoutStem 1 va r
    ]
  where
    t' = heightTreeToLengthTree t
{-# SPECIALIZE priorFunction ::
  VB.Vector (Calibration Double) ->
  VB.Vector Constraint ->
  PriorFunction I
  #-}

gradLogPriorFunction :: (RealFloat a, Show a) => VB.Vector (Calibration Double) -> VB.Vector Constraint -> IG a -> IG a
gradLogPriorFunction cs ks = grad (ln . priorFunction cs ks)
