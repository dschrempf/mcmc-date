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
import Data.Typeable
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import Numeric.AD
import qualified Numeric.LinearAlgebra as L
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
  (RealFloat a, Show a, Typeable a) =>
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  PriorFunctionG (IG a) a
priorFunction cb' cs' bs' (I l m h t mu va r) =
  product' $
    calibrateConstrainBraceSoft 1e-4 h cb 1e-4 cs 1e-4 bs t :
    -- -- Usually, the combined treatment is faster.
    -- calibrateSoft 1e-4 h cb t :
    -- constrainSoft 1e-4 cs t :
    -- braceSoft 1e-4 bs t :
    [ -- Birth and death rates of the relative time tree.
      exponential 1.0 l,
      exponential 1.0 m,
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
      exponential 1.0 mu,
      -- Variance of the relative rates.
      exponential 1.0 va,
      -- Relative rate tree.
      uncorrelatedGamma WithoutStem 1.0 va r
    ]
  where
    cb = VB.map realToFracCalibration cb'
    cs = VB.map realToFracConstraint cs'
    bs = VB.map realToFracBrace bs'
    t' = heightTreeToLengthTree t
{-# SPECIALIZE priorFunction ::
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  PriorFunction I
  #-}

-- Log of density of multivariate normal distribution with given parameters.
-- https://en.wikipedia.org/wiki/Multivariate_normal_distribution#Density_function.
logDensityMultivariateNormal ::
  -- Mean vector.
  VS.Vector Double ->
  -- Inverted covariance matrix.
  (Either (L.Herm Double) L.GMatrix) ->
  -- Log of determinant of covariance matrix.
  Double ->
  -- Value vector.
  VS.Vector Double ->
  Log Double
logDensityMultivariateNormal mu eSigmaInv logDetSigma xs =
  case eSigmaInv of
    Left sigmaInvH ->
      let sigmaInv = L.unSym sigmaInvH
       in Exp $ c + (-0.5) * (logDetSigma + ((dxs L.<# sigmaInv) L.<.> dxs))
    Right sigmaInvS ->
      -- Exp $ c + (-0.5) * (logDetSigma + ((dxs L.<# sigmaInv) L.<.> dxs))
      Exp $ c + (-0.5) * (logDetSigma + (dxs L.<.> (sigmaInvS L.!#> dxs)))
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
  (Either (L.Herm Double) L.GMatrix) ->
  -- | Log of determinant of covariance matrix.
  Double ->
  LikelihoodFunction I
likelihoodFunction mu sigmaInv logDetSigma x =
  logDensityMultivariateNormal mu sigmaInv logDetSigma distances
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
      | i <- [0 .. (nl - 1)],
        j <- [0 .. (nr - 1)]
    ]
  where
    nl = VB.length vl
    nr = VB.length vr

-- Generalized multivariate normal.
--
-- Assume the inverted covariance matrix is symmetric.
--
-- XXX: Check that the inverted covariance matrix is symmetric (similar to
-- above, where a hermitian matrix is used).
--
-- NOTE: The generalized likelihood function with boxed vectors is much slower
-- than the specialized ones using storable vectors.
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
logDensityMultivariateNormalG mu sigmaInv logDetSigma xs =
  Exp $ c + (-0.5) * (logDetSigma + reduceVMV dxs sigmaInv dxs)
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
likelihoodFunctionG mu' sigmaInv' logDetSigma' x =
  logDensityMultivariateNormalG mu sigmaInv logDetSigma distances
  where
    -- Usage of 'realToFrac' is safe here, because values are constant.
    mu = VB.map realToFrac mu'
    sigmaInv = MB.map realToFrac sigmaInv'
    logDetSigma = realToFrac logDetSigma'
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
  (RealFloat a, Show a, Typeable a) =>
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  -- Mean  vector.
  VB.Vector Double ->
  -- Inverted covariance matrix.
  MB.Matrix Double ->
  -- Log of determinant of covariance matrix.
  Double ->
  PosteriorFunctionG (IG a) a
posteriorFunction cs ks bs mu sigmaInv logDetSigma xs =
  priorFunction cs ks bs xs * likelihoodFunctionG mu sigmaInv logDetSigma xs

-- | Gradient of the log posterior function.
--
-- Useful for Hamiltonian Monte Carlo proposals.
--
-- NOTE: Automatic differentiation.
--
-- - Automatic differentiation only works on overloaded operators. The type
--   signatures of all used operators need to be general enough. Specifically:
--
--   + I cannot use any function pinning the type to a =Double=
--
--   + I cannot use linear algebra functions provided by =hmatrix=, because the
--     data type is not =Storable=.
--
-- - I think in our case, manual calculation of the gradient may be a better
--   option, but would also be more than cumbersome. In the book Bayesian Data
--   Analysis by Gelman, they suggest computing the gradient manually. We could
--   start using univariate normal distributions.
gradLogPosteriorFunc ::
  (RealFloat a, Show a, Typeable a) =>
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  -- | Mean  vector.
  VB.Vector Double ->
  -- | Inverted covariance matrix.
  MB.Matrix Double ->
  -- | Log of determinant of covariance matrix.
  Double ->
  IG a ->
  IG a
gradLogPosteriorFunc cs ks bs mu sigmaInv logDetSigma =
  -- grad (ln . priorFunction cs ks)
  -- grad (ln . likelihoodFunction mu sigmaInv logDetSigma)
  grad (ln . posteriorFunction cs ks bs mu sigmaInv logDetSigma)

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
-- numDiffLogPosteriorFunc cs ks mu sigmaInv logDetSigma xs ys h =
--   (f ys - f xs) / h
--   where
--     -- f = ln . priorFunction cs ks
--     -- f = ln . likelihoodFunction mu sigmaInv logDetSigma
--     f = ln . posteriorFunction cs ks mu sigmaInv logDetSigma
