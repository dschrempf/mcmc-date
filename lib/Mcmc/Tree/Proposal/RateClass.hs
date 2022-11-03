-- |
-- Module      :  Mcmc.Tree.Proposal.RateClass
-- Description :  Proposals on rate class trees and related parameters
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Oct 31 11:57:09 2022.
module Mcmc.Tree.Proposal.RateClass
  ( switchClassesAndRates,
    scaleDispersionAndRates,
  )
where

import Control.Lens
import Data.Foldable
import ELynx.Tree
import Mcmc.Proposal
import Mcmc.Proposal.Generic
import Mcmc.Statistics.Types
import Mcmc.Tree.Import ()
import Mcmc.Tree.Lens
import Mcmc.Tree.Types
import Numeric.Log hiding (sum)
import Statistics.Distribution.Gamma

switchClassAndRateAtPFunction :: Path -> PFunction (Double, RateClassTree, LengthTree Double)
switchClassAndRateAtPFunction p (dp, RateClassTree rcTr, LengthTree rTr) _ =
  pure (Propose x' 1.0 1.0, Nothing)
  where
    (c, rcTr') = rcTr & (subTreeAtL p . branchL) %%~ (\x -> (x, not x))
    d = dp + 1.0
    -- Switch from slow to fast class; relative rate has to become slower.
    f False x = x / d / d
    -- Switch from fast to slow class; relative rate has to become larger.
    f True x = x * d * d
    rTr' = rTr & (subTreeAtL p . branchL) %~ f c
    x' = (dp, RateClassTree rcTr', LengthTree rTr')

switchClassAndRateAt :: Path -> PName -> PWeight -> Proposal (Double, RateClassTree, LengthTree Double)
switchClassAndRateAt p n w =
  createProposal d (const $ switchClassAndRateAtPFunction p) PFast (PDimension 2) n w NoTune
  where
    d = PDescription $ "Switch rate class and rate"

switchClassesAndRates ::
  Tree e a ->
  HandleNode ->
  PName ->
  -- | Minimum weight.
  PWeight ->
  -- | Maximum weight.
  PWeight ->
  [Proposal (Double, RateClassTree, LengthTree Double)]
switchClassesAndRates tr hn n wMin wMax =
  [ switchClassAndRateAt p (name lb) w
    | (p, lb) <- itoList $ identify tr,
      let focus = tr ^. subTreeAtL p,
      let currentDepth = depth focus,
      let w = pWeight $ min (fromPWeight wMin + currentDepth - 2) (fromPWeight wMax),
      hn p
  ]
  where
    name lb = n <> PName (" node " ++ show lb)

scaleDispersionAndRatesJacobian ::
  Int ->
  (Double, RateClassTree, LengthTree Double) ->
  Double ->
  Jacobian
scaleDispersionAndRatesJacobian nTotal (dp, RateClassTree rcTr, _) u =
  -- u:
  --
  -- -2: reverse transform.
  --
  -- +1: scaling the dispersion.
  --
  -- xi:
  --
  -- +nSlow: if the dispersion gets larger, the slow mean rate gets lower, and
  -- the relative rates of the slow branches need to be scaled up.
  --
  -- -(nTotal - nSlow): the fast branches need to be slowed down; see above.
  Exp $ log u * (-1) + log xi * fromIntegral (sum [2 * nSlow, -nTotal])
  where
    -- Count slow branches. Ignore root branch, so it is a bit complicated.
    getNSlow tr = foldl' (\n x -> if x then n else n + 1) 0 $ BranchTree tr
    nSlow :: Int
    nSlow = sum $ map getNSlow $ forest rcTr
    dNew = 1 + dp * u
    dOld = 1 + dp
    xi = dNew / dOld

scaleDispersionAndRatesFunction ::
  (Double, RateClassTree, LengthTree Double) ->
  Double ->
  (Double, RateClassTree, LengthTree Double)
scaleDispersionAndRatesFunction (dp, RateClassTree rcTr, LengthTree (Node br lb ts)) u =
  (dp', RateClassTree rcTr, LengthTree rTr')
  where
    dp' = dp * u
    dNew = 1 + dp * u
    dOld = 1 + dp
    xi = dNew / dOld
    -- Slow branches need to be sped up.
    f False r = r * xi
    -- Fast branches need to slow down.
    f True r = r / xi
    getRTr' (Node b _ rcTrs) (Node r x rTrs) = Node (f b r) x $ zipWith getRTr' rcTrs rTrs
    rTr' = Node br lb $ zipWith getRTr' (forest rcTr) ts

scaleDispersionAndRatesPFunction ::
  -- Number of branches.
  Int ->
  Shape Double ->
  TuningParameter ->
  PFunction (Double, RateClassTree, LengthTree Double)
scaleDispersionAndRatesPFunction n k t =
  genericContinuous
    (gammaDistr (k / t) (t / k))
    scaleDispersionAndRatesFunction
    (Just recip)
    (Just $ scaleDispersionAndRatesJacobian n)

-- | Scale dispersion and rates such that the total rate at each branch stays
-- constant.
--
-- The stem is ignored.
scaleDispersionAndRates ::
  Tree e a ->
  Shape Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (Double, RateClassTree, LengthTree Double)
scaleDispersionAndRates tr s =
  createProposal
    dsc
    (scaleDispersionAndRatesPFunction nBranches s)
    PFast
    dim
  where
    dsc = PDescription "Scale dispersion and rates"
    nBranches = length tr - 1
    dim = PDimension $ 1 + nBranches
