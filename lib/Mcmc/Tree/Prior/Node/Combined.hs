-- |
-- Module      :  Mcmc.Tree.Prior.Node.Combined
-- Description :  Combined calibrations and constraints
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Jun 25 11:23:24 2021.
module Mcmc.Tree.Prior.Node.Combined
  ( calibrateAndConstrain,
  )
where

import qualified Data.Vector as VB
import ELynx.Tree
import Mcmc.Prior
import Mcmc.Statistics.Types
import Mcmc.Tree.Prior.Node.Calibration
import Mcmc.Tree.Prior.Node.Constraint
import Mcmc.Tree.Types

-- Get the heights of all nodes and store them in a vector.
getAllHeights :: HeightTree a -> VB.Vector a
getAllHeights = VB.fromList . branches . getHeightTree

calibrateV ::
  (RealFloat a) =>
  StandardDeviation a ->
  Calibration a ->
  PriorFunctionG (VB.Vector a) a
calibrateV s c hs = calibrateSoftF s l h
  where
    l = calibrationInterval c
    i = calibrationNodeIndex c
    h = hs VB.! i

constrainV ::
  (RealFloat a) =>
  StandardDeviation a ->
  Constraint ->
  PriorFunctionG (VB.Vector a) a
constrainV s k hs = constrainSoftF s (hY, hO)
  where
    iY = constraintYoungNodeIndex k
    hY = hs VB.! iY
    iO = constraintOldNodeIndex k
    hO = hs VB.! iO

-- | Calibrate and constrain nodes.
--
-- See 'calibrate', and 'constrain'.
--
-- First, extract all node heights from the trees.
--
-- Second, check the calibrations and constraints.
--
-- Use if there are many calibrations or constraints.
calibrateAndConstrain ::
  (RealFloat a) =>
  VB.Vector (Calibration a) ->
  -- | Standard deviation of calibrations.
  StandardDeviation a ->
  -- | Height multiplier of tree for calibrations.
  a ->
  VB.Vector Constraint ->
  -- | Standard deviation of constraints.
  StandardDeviation a ->
  PriorFunctionG (HeightTree a) a
calibrateAndConstrain cs sdC h ks sdK t
  | sdC <= 0 = error "calibrateAndConstrain: Standard deviation of calibrations is zero or negative."
  | sdK <= 0 = error "calibrateAndConstrain: Standard deviation of constraints is zero or negative."
  | h <= 0 = error "calibrateAndConstrain: Height multiplier is zero or negative."
  | otherwise = VB.product csPr * VB.product ksPr
  where
    hs = getAllHeights t
    transform (Calibration n x i l) =
      let l' = if h == 1.0 then l else transformInterval (recip h) l
       in Calibration n x i l'
    csPr = VB.map ((\c -> calibrateV sdC c hs) . transform) cs
    ksPr = VB.map (\k -> constrainV sdK k hs) ks
{-# SPECIALIZE calibrateAndConstrain ::
  VB.Vector (Calibration Double) ->
  Double ->
  Double ->
  VB.Vector Constraint ->
  Double ->
  PriorFunction (HeightTree Double)
  #-}
