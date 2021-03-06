-- |
-- Module      :  Mcmc.Tree.Prior.Branch.Internal
-- Description :  Generalized Dirichlet distribution
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Jul 19 22:51:22 2021.
module Mcmc.Tree.Prior.Branch.Internal
  ( DirichletDistributionSymmetric,
    dirichletDistributionSymmetric,
    dirichletDensitySymmetric,
  )
where

import Data.Typeable
import qualified Data.Vector as VB
import Mcmc.Internal.SpecFunctions
import Numeric.Log hiding (sum)

-- | Symmetric Dirichlet distribution.
data DirichletDistributionSymmetric a = DirichletDistributionSymmetric
  { ddSymGetParameter :: a,
    _symGetDimension :: Int,
    _symGetNormConst :: Log a
  }
  deriving (Eq, Show)

invBetaSym :: (RealFloat a, Typeable a) => Int -> a -> Log a
invBetaSym k a = Exp $ logDenominator - logNominator
  where
    logNominator = fromIntegral k * logGammaG a
    logDenominator = logGammaG (fromIntegral k * a)

-- | Create a symmetric Dirichlet distribution.
dirichletDistributionSymmetric ::
  (RealFloat a, Typeable a) =>
  Int ->
  a ->
  Either String (DirichletDistributionSymmetric a)
dirichletDistributionSymmetric k a
  | k < 2 =
      Left "dirichletDistributionSymmetric: The dimension is smaller than two."
  | a <= 0 =
      Left "dirichletDistributionSymmetric: The parameter is negative or zero."
  | otherwise = Right $ DirichletDistributionSymmetric a k (invBetaSym k a)
{-# SPECIALIZE dirichletDistributionSymmetric ::
  Int ->
  Double ->
  Either String (DirichletDistributionSymmetric Double)
  #-}

eps :: RealFloat a => a
eps = 1e-14

-- Check if vector is normalized with tolerance 'eps'.
isNormalized :: RealFloat a => VB.Vector a -> Bool
isNormalized v
  | abs (VB.sum v - 1) > eps = False
  | otherwise = True

-- | Dirichlet density.
dirichletDensitySymmetric :: RealFloat a => DirichletDistributionSymmetric a -> VB.Vector a -> Log a
dirichletDensitySymmetric (DirichletDistributionSymmetric a k c) xs
  | k /= VB.length xs = error "dicihletDensitySymmetric: Dimension mismatch."
  | VB.any (<= 0) xs = error "dirichletDensitySymmetric: Negative value."
  | not (isNormalized xs) = error "dirichletDensitySymmetric: Out of domain."
  | otherwise = c * Exp logXsPow
  where
    accF acc x = acc + log (x ** (a - 1))
    logXsPow = VB.foldl' accF 0 xs
{-# SPECIALIZE dirichletDensitySymmetric ::
  DirichletDistributionSymmetric Double ->
  VB.Vector Double ->
  Log Double
  #-}
