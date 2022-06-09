-- |
-- Module      :  Mcmc.Tree.Prior.Node.Internal
-- Description :  Functions used by more modules
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Nov 18 09:03:24 2021.
module Mcmc.Tree.Prior.Node.Internal
  ( -- Probability masses
    ProbabilityMass,
    getProbabilityMass,
    probabilityMass,
    realToFracProbabilityMass,

    -- * Relationships
    isAncestor,
    isDescendant,
    Relationship (..),
    areDirectDescendants,
  )
where

import Data.List

-- | Probability mass.
--
-- The probability mass describes how soft a node height calibration boundary or
-- a relative node order constraint is. In other words, the probability mass
-- describes the steepness of the decline of the prior function when the
-- calibration or constraint is dishonored. A larger probability mass
-- corresponds to a softer boundary, a lower probability mass corresponds to a
-- harder boundary.
--
-- We specify a probability mass with respect to normalized trees with a height
-- of 1.0. A probability mass has to be strictly positive and less than 1.0,
-- which is the total probability mass in the unit interval. If unsure, use
-- probability masses of 0.025, which corresponds to 2.5 percent probability at
-- each boundary or constraint. A probability mass close to 1.0 will correspond
-- to a prior too soft to have any effect.
--
-- See also Yang, Rannala (2005) Bayesian Estimation of Species Divergence Times
-- Under a Molecular Clock Using Multiple Fossil Calibrations with Soft Bounds,
-- Molecular Biology and Evolution.
newtype ProbabilityMass a = ProbabilityMass a
  deriving (Eq)

instance Show a => Show (ProbabilityMass a) where
  showsPrec p (ProbabilityMass x) = showsPrec p x

-- | Get the probability mass.
getProbabilityMass :: ProbabilityMass a -> a
getProbabilityMass (ProbabilityMass x) = x

-- | Create a probability mass.
probabilityMass :: (Ord a, Num a) => a -> Either String (ProbabilityMass a)
probabilityMass x
  | x <= 0 = Left "probabilityMass: Zero or negative."
  | x >= 1 = Left "probabilityMass: 1.0 or larger."
  | otherwise = Right $ ProbabilityMass x

-- | Real to fractional conversion helper.
realToFracProbabilityMass :: (Real a, Fractional b) => ProbabilityMass a -> ProbabilityMass b
realToFracProbabilityMass (ProbabilityMass x) = ProbabilityMass $ realToFrac x

-- | Is the left node an ancestor of the right node?
isAncestor :: Eq a => [a] -> [a] -> Bool
isAncestor = isPrefixOf

-- | Is the left node a descendant of the right node?
isDescendant :: Eq a => [a] -> [a] -> Bool
isDescendant = flip isPrefixOf

-- | Relationship of two nodes.
data Relationship
  = Equal
  | -- | Same as @RightIsDescendentOfLeft@.
    LeftIsAncestorOfRight
  | -- | Same as @RightIsAncestorOfLeft@.
    LeftIsDescendantOfRight
  | Unrelated
  deriving (Eq, Show, Read)

-- | Fast function avoiding two consecutive uses of `isPrefixOf`.
--
-- Are the two nodes direct descent of each other? Basically check both:
-- 'isAncestor' and 'isDescendant'.
areDirectDescendants :: Eq a => [a] -> [a] -> Relationship
areDirectDescendants [] [] = Equal
areDirectDescendants (x : xs) (y : ys)
  | x == y = areDirectDescendants xs ys
  | otherwise = Unrelated
areDirectDescendants (_ : _) [] = LeftIsDescendantOfRight
areDirectDescendants [] (_ : _) = LeftIsAncestorOfRight
