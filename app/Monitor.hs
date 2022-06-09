-- |
-- Module      :  Monitor
-- Description :  Monitor functions
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue May 24 15:05:13 2022.
module Monitor
  ( monitorPriorCsKsBs,
    monitorPriorBirthDeath,
    monitorPriorRelaxedMolecularClock,
  )
where

import Control.Lens
import qualified Data.Vector as VB
import Mcmc
import Mcmc.Tree
import Probability
import State

-- | Monitor prior function for calibrations, constraints, and braces.
monitorPriorCsKsBs ::
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  MonitorParameter I
monitorPriorCsKsBs cb cs bs = f >$< monitorDouble "PriorCsKsBs"
  where
    f = ln . priorFunctionCalibrationsConstraintsBraces cb cs bs

monitorPriorBirthDeath :: MonitorParameter I
monitorPriorBirthDeath = f >$< monitorDouble "PriorBirthDeath"
  where
    f x =
      let t = heightTreeToLengthTree (x ^. timeTree)
       in ln $ priorFunctionBirthDeath t x

monitorPriorRelaxedMolecularClock ::
  -- | Initial, constant, approximate absolute time tree height.
  Double ->
  RelaxedMolecularClockModel ->
  MonitorParameter I
monitorPriorRelaxedMolecularClock ht md = f >$< monitorDouble n
  where
    n = case md of
      UncorrelatedGamma -> "PriorUncorrelatedGammaRelaxedMolecularClock"
      AutocorrelatedLogNormal -> "PriorAutocorrelatedLogNormalRelaxedMolecularClock"
    f x =
      let t = heightTreeToLengthTree (x ^. timeTree)
       in ln $ priorFunctionRelaxedMolecularClock ht md t x
