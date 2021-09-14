-- |
-- Module      :  Hamiltonian
-- Description :  Tools for Hamiltonian proposal
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Sep 14 17:36:51 2021.
module Hamiltonian
  ( hmc,
  )
where

import Control.Lens
import Data.Foldable
import Data.Traversable
import qualified Data.Vector.Storable as VS
import ELynx.Tree
import Mcmc hiding (toVector)
import Mcmc.Tree
import qualified Numeric.LinearAlgebra as L
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
    f (True : ms, i) _ = ((ms, i -1), xs VS.! i)
    f (False : ms, i) z = ((ms, i), z)
    f (_, _) _ = error "fromVectorWith: Mask is too short or traversable structure is too long."

hmcSettingsWith :: [Bool] -> I -> (I -> I) -> HSettings I
hmcSettingsWith mask x gradient =
  HSettings
    toVector'
    (fromVectorWith mask)
    gradient
    (Just isValidState)
    masses
    10
    0.05
    (HTune HTuneLeapfrog HTuneDiagonalMassesOnly)
  where
    toVector' = toVector mask
    masses = L.scale 100 $ L.trustSym $ L.ident $ L.size $ toVector' x

-- | The Hamiltonian proposal.
hmc :: Bool -> I -> (I -> I) -> Proposal I
hmc calibrationsAvailable x gradient = hamiltonian x s (PName "All parameters") (pWeight 1)
  where
    mask = getMask calibrationsAvailable x
    s = hmcSettingsWith mask x gradient
