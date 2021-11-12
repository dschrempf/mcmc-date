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
  ( calibrateConstrainBraceSoft,
  )
where

import qualified Data.Vector as VB
import ELynx.Tree
import Mcmc.Prior
import Mcmc.Statistics.Types
import Mcmc.Tree.Prior.Node.Brace
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

braceV ::
  (RealFloat a) =>
  StandardDeviation a ->
  Brace ->
  PriorFunctionG (VB.Vector a) a
braceV s b hs = braceSoftF s nHs
  where
    nIs = map nodeIndex $ braceNodes b
    nHs = map (hs VB.!) nIs

-- | Calibrate, constrain, and brace nodes.
--
-- See 'calibrateSoft', 'constrainSoft', and 'braceSoft'.
--
-- First, extract all node heights from the trees.
--
-- Then, check the calibrations, constraints, and braces.
--
-- Use if there are many calibrations, constraints, or braces.
calibrateConstrainBraceSoft ::
  (RealFloat a) =>
  -- | Standard deviation of calibrations.
  StandardDeviation a ->
  -- | Height multiplier of tree for calibrations.
  a ->
  VB.Vector (Calibration a) ->
  -- | Standard deviation of constraints.
  StandardDeviation a ->
  VB.Vector Constraint ->
  -- | Standard deviation of braces.
  StandardDeviation a ->
  VB.Vector Brace ->
  PriorFunctionG (HeightTree a) a
calibrateConstrainBraceSoft sdC h cs sdK ks sdB bs t
  | sdC <= 0 = error "calibrateConstrainBraceSoft: Standard deviation of calibrations is zero or negative."
  | h <= 0 = error "calibrateConstrainBraceSoft: Height multiplier is zero or negative."
  | sdK <= 0 = error "calibrateConstrainBraceSoft: Standard deviation of constraints is zero or negative."
  | sdB <= 0 = error "calibrateConstrainBraceSoft: Standard deviation of braces is zero or negative."
  | otherwise = VB.product csPr * VB.product ksPr * VB.product bsPr
  where
    hs = getAllHeights t
    transform (Calibration n x i l) =
      let l' = if h == 1.0 then l else transformInterval (recip h) l
       in Calibration n x i l'
    csPr = VB.map ((\c -> calibrateV sdC c hs) . transform) cs
    ksPr = VB.map (\k -> constrainV sdK k hs) ks
    bsPr = VB.map (\b -> braceV sdB b hs) bs
{-# SPECIALIZE calibrateConstrainBraceSoft ::
  Double ->
  Double ->
  VB.Vector (Calibration Double) ->
  Double ->
  VB.Vector Constraint ->
  Double ->
  VB.Vector Brace ->
  PriorFunction (HeightTree Double)
  #-}
