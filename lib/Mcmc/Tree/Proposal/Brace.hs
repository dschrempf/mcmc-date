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
    slideBracedNodesContrarily,
  )
where

import Control.Lens
import Data.Maybe
import ELynx.Tree
import Mcmc.Proposal
import Mcmc.Statistics.Types
import Mcmc.Tree.Lens
import Mcmc.Tree.Prior.Node.Brace
import Mcmc.Tree.Proposal.Internal
import Mcmc.Tree.Types

getParents :: [Path] -> [TreePos e a] -> [TreePos e a]
getParents xs = catMaybes . zipWith f xs
  where
    f pth pos = if null pth then Nothing else Just $ goParentUnsafe pos

slideBracedNodesUltrametricSimple ::
  Brace ->
  StandardDeviation Double ->
  TuningParameter ->
  ProposalSimple (HeightTree Double)
slideBracedNodesUltrametricSimple b s t tr g = do
  (deltaH, q) <- truncatedNormalSample 0 s t lowerBound upperBound g
  let modifyHeight = assertWith (> 0) . (+ deltaH)
      -- Set the node height at path.
      modifyHeightAcc pth tre = tre & heightTreeL . subTreeAtL pth . branchL %~ modifyHeight
      -- NOTE: The first path is walked again which could be improved.
      tr' = foldr modifyHeightAcc tr paths
  return (tr', q, 1)
  where
    -- Calculate the boundaries per node, and then choose the smallest interval
    -- satisfying all boundaries.
    paths = map nodePath $ getBraceNodes b
    hbds = map (getHeightBoundaries "slideBracedNodesUltrametricSimple" tr) paths
    -- Interval around 0.
    getInterval d = let h = hbdNodeHeight d in (hbdMaximumChildrenHeight d - h, hbdParentHeight d - h)
    intervals = map getInterval hbds
    -- Should be strictly negative.
    lowerBound = assertWith (< 0) $ maximum $ map fst intervals
    -- Should be strictly positive.
    upperBound = assertWith (> 0) $ minimum $ map snd intervals

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
slideBracedNodesUltrametric tr b s
  | not $ all (isValidPath tr) paths =
    error $ "slideBracedNodesUltrametric: Path of a node is invalid: Brace: " <> show n <> ", Paths: " <> show paths <> "."
  | any (isLeafPath tr) paths =
    error $ "slideBracedNodesUltrametric: Path of a node leads to a leaf: Brace: " <> show n <> ", Paths: " <> show paths <> "."
  -- NOTE: For hard braces, the dimension is 1.
  | otherwise =
    createProposal description (slideBracedNodesUltrametricSimple b s) (PDimension $ length ns)
  where
    n = getBraceName b
    ns = getBraceNodes b
    paths = map nodePath ns
    description = PDescription $ "Slide braced nodes ultrametric; sd: " ++ show s

slideBracedNodesContrarilySimple ::
  Brace ->
  StandardDeviation Double ->
  TuningParameter ->
  ProposalSimple (HeightTree Double, LengthTree Double)
slideBracedNodesContrarilySimple b s t (tTr, rTr) g
  | any null rTrChildren =
    error "slideBracedNodesContrarilySimple: Sub tree of unconstrained tree is a leaf."
  | otherwise = do
    (deltaH, q) <- truncatedNormalSample 0 s t lowerBound upperBound g
    -- Time tree. See also 'slideBracedNodesUltrametricSimple'.
    let modifyHeight = assertWith (> 0) . (+ deltaH)
        modifyHeightAcc pth tre = tre & heightTreeL . subTreeAtL pth . branchL %~ modifyHeight
        tr' = foldr modifyHeightAcc tTr paths
    -- TODO.
    return undefined
    -- -- Rate tree.
    -- let -- Scaling factor of rate tree stem.
    --     xiStemR =
    --       if null pth
    --         then 1
    --         else
    --           let x = (hParent - hNode) / (hParent - hNode')
    --            in assertWith (> 0) x
    --     -- Scaling factors of rate tree daughter branches excluding the stem.
    --     getXiR h = let x = (hNode - h) / (hNode' - h) in assertWith (> 0) x
    --     xisR = map getXiR hsChildren
    --     scaleDaughterBranches (Node br lb trs) =
    --       Node br lb $ zipWith (modifyStem . (*)) xisR trs
    --     -- If the root node is handled, do not scale the stem because no upper
    --     -- bound is set.
    --     f =
    --       if null pth
    --         then scaleDaughterBranches
    --         else modifyStem (* xiStemR) . scaleDaughterBranches
    --     rTr' = toTree $ modifyTree f rTrPos
    -- -- New state.
    -- let x' = (HeightTree tTr', LengthTree rTr')
    --     jacobian = Exp $ sum (map log xisR) + log xiStemR
    -- let
    -- return (x', q, jacobian)
  where
    -- Time tree. See also 'slideBracedNodesUltrametricSimple'.
    paths = map nodePath $ getBraceNodes b
    hbds = map (getHeightBoundaries "slideBracedNodesUltrametricSimple" tTr) paths
    getInterval d = let h = hbdNodeHeight d in (hbdMaximumChildrenHeight d - h, hbdParentHeight d - h)
    intervals = map getInterval hbds
    lowerBound = assertWith (< 0) $ maximum $ map fst intervals
    upperBound = assertWith (> 0) $ minimum $ map snd intervals
    -- Rate tree.
    rTrPs = map (\p -> goPathUnsafe p $ fromTree $ getLengthTree rTr) paths
    rTrFocuses = map current rTrPs
    rTrChildren = map forest rTrFocuses

-- | Slide braced nodes contrarily.
--
-- See 'Mcmc.Tree.Proposal.Contrary.slideNodesAtContrarily'.
--
-- Call 'error' if:
--
-- - A path is invalid.
--
-- - A path leads to a leaf.
--
-- - A node height or branch length is zero.
slideBracedNodesContrarily ::
  Tree e a ->
  Brace ->
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (HeightTree Double, LengthTree Double)
slideBracedNodesContrarily tr b s
  | not $ all (isValidPath tr) paths =
    error $
      "slideBracedNodesContrarily: Path of a node is invalid: Brace: "
        <> show n
        <> ", Paths: "
        <> show paths
        <> "."
  | any (isLeafPath tr) paths =
    error $
      "slideBracedNodesContrarily: Path of a node leads to a leaf: Brace: "
        <> show n
        <> ", Paths: "
        <> show paths
        <> "."
  | otherwise =
    createProposal
      description
      (slideBracedNodesContrarilySimple b s)
      -- NOTE: For hard braces, the dimension is `1 + nStems + nDaughters`.
      (PDimension $ length ns + nStems + nDaughters)
  where
    n = getBraceName b
    ns = getBraceNodes b
    description = PDescription $ "Slide braced nodes contrarily; sd: " ++ show s
    paths = map nodePath ns
    nStems = sum [if null p then 0 else 1 | p <- paths]
    nDaughters = sum [length $ forest $ current $ goPathUnsafe p $ fromTree tr | p <- paths]
