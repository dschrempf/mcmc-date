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
    likelihoodFunction,
    gradLogPosteriorFunc,
  )
where

import Control.Lens
import Data.Foldable
import qualified Data.Matrix as MB
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as L
import Numeric.AD
import Numeric.Log hiding (sum)
import Numeric.MathFunctions.Constants

{- ORMOLU_DISABLE -}
import ELynx.Tree
import Mcmc
import Mcmc.Tree
import State
import Tools
{- ORMOLU_ENABLE -}

-- | Prior function.
priorFunction ::
  (RealFloat a, Show a) =>
  VB.Vector (Calibration Double) ->
  VB.Vector Constraint ->
  PriorFunctionG (IG a) a
priorFunction cb' cs (IG l m h t mu va r) =
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
    cb = VB.map realToFracC cb'
    t' = heightTreeToLengthTree t
{-# SPECIALIZE priorFunction ::
  VB.Vector (Calibration Double) ->
  VB.Vector Constraint ->
  PriorFunction I
  #-}

-- NOTE: The generalized likelihood function with boxed vectors is much slower.

-- Log of density of multivariate normal distribution with given parameters.
-- https://en.wikipedia.org/wiki/Multivariate_normal_distribution.
logDensityMultivariateNormal ::
  -- Mean vector.
  VS.Vector Double ->
  -- Inverted covariance matrix.
  L.Matrix Double ->
  -- Log of determinant of covariance matrix.
  Double ->
  -- Value vector.
  VS.Vector Double ->
  Log Double
logDensityMultivariateNormal mu sigmaInv logSigmaDet xs =
  Exp $ c + (-0.5) * (logSigmaDet + ((dxs L.<# sigmaInv) L.<.> dxs))
  where
    dxs = xs - mu
    k = fromIntegral $ VS.length mu
    c = negate $ m_ln_sqrt_2_pi * k

-- | Approximation of the phylogenetic likelihood using a multivariate normal
-- distribution.
likelihoodFunction ::
  -- | Mean vector.
  VS.Vector Double ->
  -- | Inverted covariance matrix.
  L.Matrix Double ->
  -- | Log of determinant of covariance matrix.
  Double ->
  LikelihoodFunction I
likelihoodFunction mu sigmaInv logSigmaDet x =
  logDensityMultivariateNormal mu sigmaInv logSigmaDet distances
  where
    times = getBranches (getLengthTree $ heightTreeToLengthTree $ x ^. timeTree)
    rates = getBranches (getLengthTree $ x ^. rateTree)
    tH = x ^. timeHeight
    rMu = x ^. rateMean
    distances = VS.map (* (tH * rMu)) $ sumFirstTwo $ VS.zipWith (*) times rates

-- Vector-matrix-vector product.
--
-- Assume the dimensions match.
reduceVMV :: RealFloat a => VB.Vector a -> MB.Matrix a -> VB.Vector a -> a
-- reduceVMV vl m vr = foldl' (accF vl vr) 0 (zip [0 ..] (MB.toRows m))
reduceVMV vl m vr =
  foldl'
    (+)
    0
    [ (vl VB.! i) * (m `MB.unsafeIndex` (i, j)) * (vr VB.! j)
      | i <- [0 .. (nl -1)],
        j <- [0 .. (nr -1)]
    ]
  where
    nl = VB.length vl
    nr = VB.length vr

-- Generalized multivariate normal.
logDensityMultivariateNormalG ::
  RealFloat a =>
  -- Mean vector.
  VB.Vector a ->
  -- Inverted covariance matrix.
  MB.Matrix a ->
  -- Log of determinant of covariance matrix.
  a ->
  -- Value vector.
  VB.Vector a ->
  Log a
logDensityMultivariateNormalG mu sigmaInv logSigmaDet xs =
  Exp $ c + (-0.5) * (logSigmaDet + reduceVMV dxs sigmaInv dxs)
  where
    dxs = VB.zipWith (-) xs mu
    k = fromIntegral $ VB.length mu
    -- Usage of 'realToFrac' is safe here, because values are constant.
    c = negate $ realToFrac (m_ln_sqrt_2_pi * k)

getBranchesG :: Tree a b -> VB.Vector a
getBranchesG (Node _ _ [l, r]) =
  {-# SCC getBranchesG #-}
  VB.fromList $ head ls : head rs : tail ls ++ tail rs
  where
    ls = branches l
    rs = branches r
getBranchesG _ = error "getBranches: Root node is not bifurcating."

sumFirstTwoG :: RealFloat a => VB.Vector a -> VB.Vector a
sumFirstTwoG v = (v VB.! 0 + v VB.! 1) `VB.cons` VB.drop 2 v

-- Generalizaed likelihood function for automatic differentiation.
likelihoodFunctionG ::
  (RealFloat a, Show a) =>
  -- Mean vector.
  VB.Vector Double ->
  -- Inverted covariance matrix.
  MB.Matrix Double ->
  -- Log of determinant of covariance matrix.
  Double ->
  LikelihoodFunctionG (IG a) a
likelihoodFunctionG mu' sigmaInv' logSigmaDet' x =
  logDensityMultivariateNormalG mu sigmaInv logSigmaDet distances
  where
    -- Usage of 'realToFrac' is safe here, because values are constant.
    mu = VB.map realToFrac mu'
    sigmaInv = MB.map realToFrac sigmaInv'
    logSigmaDet = realToFrac logSigmaDet'
    -- Actual computation.
    times = getBranchesG (getLengthTree $ heightTreeToLengthTree $ x ^. timeTree)
    rates = getBranchesG (getLengthTree $ x ^. rateTree)
    tH = x ^. timeHeight
    rMu = x ^. rateMean
    distances = VB.map (* (tH * rMu)) $ sumFirstTwoG $ VB.zipWith (*) times rates
{-# SPECIALIZE likelihoodFunctionG ::
  VB.Vector Double ->
  MB.Matrix Double ->
  Double ->
  LikelihoodFunction I
  #-}

posteriorFunction ::
  (RealFloat a, Show a) =>
  VB.Vector (Calibration Double) ->
  VB.Vector Constraint ->
  -- Mean  vector.
  VB.Vector Double ->
  -- Inverted covariance matrix.
  MB.Matrix Double ->
  -- Log of determinant of covariance matrix.
  Double ->
  PosteriorFunctionG (IG a) a
posteriorFunction cs ks mu sigmaInv logSigmaDet xs =
  priorFunction cs ks xs * likelihoodFunctionG mu sigmaInv logSigmaDet xs

-- | Gradient of the log posterior function.
--
-- Useful for Hamiltonian Monte Carlo proposals.
gradLogPosteriorFunc ::
  (RealFloat a, Show a) =>
  VB.Vector (Calibration Double) ->
  VB.Vector Constraint ->
  -- | Mean  vector.
  VB.Vector Double ->
  -- | Inverted covariance matrix.
  MB.Matrix Double ->
  -- | Log of determinant of covariance matrix.
  Double ->
  IG a ->
  IG a
gradLogPosteriorFunc cs ks mu sigmaInv logSigmaDet =
  -- grad (ln . priorFunction cs ks)
  -- grad (ln . likelihoodFunction mu sigmaInv logSigmaDet)
  grad (ln . posteriorFunction cs ks mu sigmaInv logSigmaDet)

-- numDiffLogPosteriorFunc ::
--   (RealFloat a, Show a) =>
--   VB.Vector (Calibration Double) ->
--   VB.Vector Constraint ->
--   -- | Mean  vector.
--   VB.Vector Double ->
--   -- | Inverted covariance matrix.
--   MB.Matrix Double ->
--   -- | Log of determinant of covariance matrix.
--   Double ->
--   IG a ->
--   IG a ->
--   a ->
--   a
-- numDiffLogPosteriorFunc cs ks mu sigmaInv logSigmaDet xs ys h =
--   (f ys - f xs) / h
--   where
--     -- f = ln . priorFunction cs ks
--     -- f = ln . likelihoodFunction mu sigmaInv logSigmaDet
--     f = ln . posteriorFunction cs ks mu sigmaInv logSigmaDet
