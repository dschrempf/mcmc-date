-- |
-- Module      :  Hamiltonian
-- Description :  Tools for Hamiltonian proposal
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Sep 14 17:36:51 2021.
module Hamiltonian
  ( htargetWith,
    hmcWith,
  )
where

import Control.Lens
import Data.Foldable
import qualified Data.Matrix as MB
import Data.Traversable
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import ELynx.Tree
import Mcmc hiding (toVector)
import Mcmc.Tree
import qualified Numeric.LinearAlgebra as L
import Probability
import State

-- Return a state defining if variables are to be changed by the Hamiltonian
-- proposal ('True') or not ('False').
getMask :: Bool -> I -> [Bool]
getMask calibrationsAvailable x =
  toList $
    fmap (const True) x
      -- Do not change the height of the relative time tree.
      & timeTree . heightTreeL . branchL .~ False
      -- Do not change the height of the relative time tree leaves.
      & timeTree . heightTreeL %~ setLeaves
      -- Do not change the root branch of the relative rate tree.
      & rateTree . lengthTreeL . branchL .~ False
      -- Only change the time height if calibrations are available.
      & timeHeight .~ calibrationsAvailable
  where
    setLeaves (Node _ lb []) = Node False lb []
    setLeaves (Node br lb ts) = Node br lb (map setLeaves ts)

toVector :: [Bool] -> I -> L.Vector Double
toVector mask xs = L.fromList $ snd $ foldl' f (mask, []) xs
  where
    f (m : ms, ys) y = (ms, if m then y : ys else ys)
    f _ _ = error "toVector: Mask is too short."

fromVectorWith :: [Bool] -> I -> L.Vector Double -> I
fromVectorWith mask x xs = snd $ mapAccumL f (mask, L.size xs - 1) x
  where
    f (True : ms, i) _ = ((ms, i - 1), xs VS.! i)
    f (False : ms, i) z = ((ms, i), z)
    f (_, _) _ = error "fromVectorWith: Mask is too short or traversable structure is too long."

tspecWith :: [Bool] -> I -> HTuningSpec
tspecWith mask x =
  either error id $
    hTuningSpec masses 10 0.001 (HTuningConf HTuneLeapfrog HTuneAllMasses)
  where
    toVector' = toVector mask
    masses = L.scale 10.0 $ L.trustSym $ L.ident $ L.size $ toVector' x

hspecWith :: [Bool] -> I -> HSpec IG
hspecWith mask x = HSpec x toVector' fromVectorWith'
  where
    toVector' = toVector mask
    fromVectorWith' = fromVectorWith mask

-- | Target for Hamiltonian proposal.
htargetWith ::
  Double ->
  RelaxedMolecularClockModel ->
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  -- | Mean vector.
  VB.Vector Double ->
  -- | Inverted full covariance matrix.
  MB.Matrix Double ->
  -- | Log of determinant of full covariance matrix.
  Double ->
  HTarget IG
htargetWith ht md cb cs bs mu s d =
  HTarget
    -- I can not use a where or let statement to define the prior and likelihood
    -- functions. See https://stackoverflow.com/q/63119425/3536806, and
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/17130.
    (Just $ priorFunction ht md cb cs bs)
    (likelihoodFunctionG mu s d)
    (Just jacobianRootBranch)

-- | The Hamiltonian proposal.
hmcWith ::
  Bool ->
  I ->
  HTarget IG ->
  Proposal I
hmcWith calibrationsAvailable x htarget = hamiltonian tspec hspec htarget n w
  where
    mask = getMask calibrationsAvailable x
    tspec = tspecWith mask x
    hspec = hspecWith mask x
    n = PName "All parameters"
    w = pWeight 1
