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
  )
where

import Control.Lens
import ELynx.Tree
import Mcmc.Proposal
import Mcmc.Tree

switchClassAndRateAtPFunction :: Path -> PFunction (Double, RateClassTree, LengthTree Double)
switchClassAndRateAtPFunction p (dp, RateClassTree rcTr, LengthTree rTr) _ =
  -- TODO: Check Jacobian.
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

-- TODO.
scaleDispersionAndRates :: a
scaleDispersionAndRates = undefined
