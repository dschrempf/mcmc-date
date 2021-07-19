-- |
-- Module      :  Mcmc.Internal.Dirichlet
-- Description :  Generalized Dirichlet distribution
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Jul 19 22:51:22 2021.
module Mcmc.Internal.Dirichlet
  ( DirichletDistributionSymmetric,
    dirichletDistributionSymmetric,
    dirichletDensitySymmetric,
  )
where

import Mcmc.Internal.Gamma
import Numeric.Log hiding (sum)

data DirichletDistributionSymmetric a = DirichletDistributionSymmetric
  { ddSymGetParameter :: a,
    _symGetDimension :: Int,
    _symGetNormConst :: Log a
  }
  deriving (Eq, Show)

invBetaSym :: RealFloat a => Int -> a -> Log a
invBetaSym k a = Exp $ logDenominator - logNominator
  where
    logNominator = fromIntegral k * logGammaG a
    logDenominator = logGammaG (fromIntegral k * a)

dirichletDistributionSymmetric ::
  RealFloat a =>
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
isNormalized :: RealFloat a => [a] -> Bool
isNormalized v
  | abs (sum v - 1.0) > eps = False
  | otherwise = True

dirichletDensitySymmetric :: RealFloat a => DirichletDistributionSymmetric a -> [a] -> Log a
dirichletDensitySymmetric (DirichletDistributionSymmetric a k c) xs
  | k /= length xs = 0.0
  | any (<= 0) xs = 0.0
  | not (isNormalized xs) = 0.0
  | otherwise = c * Exp logXsPow
  where
    logXsPow = sum $ map (\x -> log $ x ** (a - 1.0)) xs
{-# SPECIALIZE dirichletDensitySymmetric ::
  DirichletDistributionSymmetric Double ->
  [Double] ->
  Log Double
  #-}
