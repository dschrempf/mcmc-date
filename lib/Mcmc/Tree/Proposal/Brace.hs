-- |
-- Module      :  Mcmc.Tree.Proposal.Brace
-- Description :  Proposals on braced nodes
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Nov 11 14:28:07 2021.
--
-- Proposals on braced nodes. Two nodes are braced when their height is equal
-- (hard brace) or nearly equal (soft brace).
--
-- See "Mcmc.Tree.Prior.Node.Brace".
--
-- NOTE: For hard braces, all Jacobian determinants of multiplicative proposals
-- acting on the heights of both braced nodes need to be amended. This is not
-- really feasible. At the moment, I use soft braces exclusively.
module Mcmc.Tree.Proposal.Brace
  ( slideBracedNodesUltrametric,
  )
where

import Control.Exception
import Control.Lens
import Data.Maybe
import ELynx.Tree
import Mcmc.Proposal
import Mcmc.Statistics.Types
import Mcmc.Tree.Lens
import Mcmc.Tree.Prior.Node.Brace
import Mcmc.Tree.Proposal.Internal
import Mcmc.Tree.Types

slideBracedNodesUltrametricSimple ::
  Brace ->
  StandardDeviation Double ->
  TuningParameter ->
  ProposalSimple (HeightTree Double)
slideBracedNodesUltrametricSimple (Brace _ x _ y _) s t tr g
  | any null childrens = error "slideBracedNodesUltrametricSimple: Cannot slide leaf."
  | otherwise = do
    (deltaH, q) <- truncatedNormalSample 0 s t (hMaxChildren - hBar) (hMinParents - hBar) g
    let f h = let h' = h + deltaH in assert (h' > 0) h'
        -- NOTE: The first path is walked again which could be improved.
        tr' =
          tr
            -- XXX: How can I improve this? I want to convert the list of paths
            -- to a traversal.
            & heightTreeL . subTreeAtL x . branchL %~ f
            & heightTreeL . subTreeAtL y . branchL %~ f
    return (tr', q, 1.0)
  where
    pths = [x, y]
    trPs = map (\p -> goPathUnsafe p $ fromTree $ getHeightTree tr) pths
    focuses = map current trPs
    childrens = map forest focuses
    hsNodes = map branch focuses
    hBar = sum hsNodes / fromIntegral (length hsNodes)
    hMaxChildren = maximum $ map branch $ concat childrens
    -- 'parents' is taken.
    progenitors =
      let f pth pos = if null pth then Nothing else Just $ current $ goParentUnsafe pos
       in catMaybes $ zipWith f pths trPs
    hMinParents = if null progenitors then 1 / 0 else minimum $ map branch progenitors

-- | Slide braced nodes.
--
-- See 'Mcmc.Tree.Proposal.Ultrametric.slideNodeAtUltrametric'.
--
-- Call 'error' if:
--
-- - A path is invalid.
--
-- - A path leads to a leaf.
slideBracedNodesUltrametric ::
  -- | The topology of the tree is used to check the paths.
  Tree e a ->
  Brace ->
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (HeightTree Double)
slideBracedNodesUltrametric tr b@(Brace _ x _ y _) s
  | not $ isValidPath tr x =
    error $ "slideBracedNodesUltrametric: Path of first node is invalid: " <> show x <> "."
  | not $ isValidPath tr y =
    error $ "slideBracedNodesUltrametric: Path of second node is invalid: " <> show y <> "."
  | isLeafPath tr x =
    error $ "slideBracedNodesUltrametric: Path of first node leads to a leaf: " <> show x <> "."
  | isLeafPath tr y =
    error $ "slideBracedNodesUltrametric: Path of second node leads to a leaf: " <> show y <> "."
  -- NOTE: For hard braces, the dimension is 1.
  | otherwise = createProposal description (slideBracedNodesUltrametricSimple b s) (PDimension 2)
  where
    description = PDescription $ "Slide braced nodes ultrametric; sd: " ++ show s
