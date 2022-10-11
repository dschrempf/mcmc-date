-- |
-- Module      :  Mcmc.Tree.Proposal.Contrary
-- Description :  Contrary proposal between two trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Mar  3 13:19:54 2021.
module Mcmc.Tree.Proposal.Contrary
  ( slideNodesAtContrarily,
    slideNodesContrarily,
    slideRootContrarily,
    scaleSubTreesAtContrarily,
    scaleSubTreesContrarily,
    scaleRatesAndTreeContrarily,
  )
where

import Control.Lens
import Control.Monad
import Data.Bifunctor
import ELynx.Tree
import Mcmc.Proposal
import Mcmc.Statistics.Types
import Mcmc.Tree.Lens
import Mcmc.Tree.Proposal.Internal
import Mcmc.Tree.Proposal.Unconstrained
import Mcmc.Tree.Types
import Numeric.Log hiding (sum)

-- See also 'slideNodeAtUltrametricPFunction'.
slideNodesAtContrarilyPFunction ::
  Path ->
  StandardDeviation Double ->
  TuningParameter ->
  PFunction (HeightTree Double, LengthTree Double)
slideNodesAtContrarilyPFunction pth sd t (tTr, LengthTree rTr) g
  | null rTrChildren =
      error "slideNodesAtContrarilyPFunction: Sub tree of unconstrained tree is a leaf."
  | otherwise = do
      (hNode', q) <- truncatedNormalSample hNode sd t hMaxChild hParent g
      -- Time tree.
      let tTr' = toTree $ tTrPos & currentTreeL . branchL .~ assertWith (> 0) hNode'
      -- Rate tree.
      let -- Scaling factors of rate tree stems (inversely proportional to scaling
          -- factors of height tree).
          xiStemR =
            if null pth
              then 1
              else assertWith (> 0) $ (hParent - hNode) / (hParent - hNode')
          -- Scaling factors of rate tree daughter branches excluding the stem.
          getXiR hChild = assertWith (> 0) (hNode - hChild) / (hNode' - hChild)
          xisR = map getXiR hsChildren
          scaleDaughterBranches (Node br lb trs) =
            Node br lb $ zipWith (modifyStem . (*)) xisR trs
          -- If the root node is handled, do not scale the stem because no upper
          -- bound is set.
          f =
            if null pth
              then scaleDaughterBranches
              else modifyStem (* xiStemR) . scaleDaughterBranches
          rTr' = toTree $ modifyTree f rTrPos
      -- New state.
      let x' = (HeightTree tTr', LengthTree rTr')
          jacobian = Exp $ sum (map log xisR) + log xiStemR
      return (Propose x' q jacobian, Nothing)
  where
    -- Time tree.
    (HeightBoundaryData tTrPos hNode hsChildren hMaxChild hParent) =
      getHeightBoundaries "slideNodesAtContrarilyPFunction" tTr pth
    -- Rate tree.
    rTrPos = goPathUnsafe pth $ fromTree rTr
    rTrFocus = current rTrPos
    rTrChildren = forest rTrFocus

-- | Slide nodes contrarily at given path.
--
-- This proposal acts on a pair of trees. The first tree is an ultrametric tree
-- (see "Mcmc.Tree.Proposal.Ultrametric"), usually the time tree with branches
-- denoting absolute or relative time. The second tree is an unconstrained tree
-- (see "Mcmc.Tree.Proposal.Unconstrained"), usually the rate tree with branches
-- denoting absolute or relative rates.
--
-- The specified nodes of both trees are slid contrarily. For example, if the
-- parent branch of the ultrametric tree node is shortened, the parent branch of
-- the unconstrained tree node is elongated. Ultrametricity is maintained.
--
-- A normal distribution truncated at the height of the parent node of the
-- ultrametric tree and the oldest daughter node of the ultrametric tree is used
-- to determine the new node height of the ultrametric tree. The rates are
-- changed accordingly.
--
-- See 'Mcmc.Tree.Proposal.Ultrametric.slideNodeAtUltrametric'.
--
-- NOTE: When applying to the root node (1) the tree heights change contrarily,
-- (2) no upper bound is used because no parent node exists, and (3) the stem of
-- the unconstrained tree is not changed because it does not map to a valid
-- branch of the ultrametric tree.
--
-- NOTE: Application of this proposal to trees with different topologies will
-- lead to unexpected behavior and possibly to run time errors.
--
-- Call 'error' if:
--
-- - The path is invalid.
--
-- - The path leads to a leaf.
--
-- - A node height or branch length is zero.
slideNodesAtContrarily ::
  -- | The topology of the tree is used to check the path.
  Tree e a ->
  Path ->
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (HeightTree Double, LengthTree Double)
slideNodesAtContrarily tr pth sd
  | not $ isValidPath tr pth = error $ "slideNodesAtContrarily: Path is invalid: " <> show pth <> "."
  | isLeafPath tr pth = error $ "slideNodesAtContrarily: Path leads to a leaf: " <> show pth <> "."
  | otherwise =
      createProposal
        description
        (slideNodesAtContrarilyPFunction pth sd)
        PFast
        -- 1 for ultrametric node.
        -- 0 or 1 for unconstrained stem.
        -- n for unconstrained daughters.
        (PDimension $ 1 + nStem + nDaughters)
  where
    description = PDescription $ "Slide nodes contrarily; sd: " <> show sd
    nStem = if null pth then 0 else 1
    nDaughters = length $ forest $ current $ goPathUnsafe pth $ fromTree tr

-- | Slide the nodes of two trees contrarily.
--
-- See 'slideNodesAtContrarily'.
--
-- The weights are assigned as described in
-- 'Mcmc.Tree.Proposal.Ultrametric.scaleSubTreesUltrametric'.
--
-- Do not scale the leaves.
slideNodesContrarily ::
  Tree e a ->
  HandleNode ->
  StandardDeviation Double ->
  PName ->
  -- | Minimum weight.
  PWeight ->
  -- | Maximum weight.
  PWeight ->
  Tune ->
  [Proposal (HeightTree Double, LengthTree Double)]
slideNodesContrarily tr hn s n wMin wMax t =
  [ slideNodesAtContrarily tr pth s (name lb) w t
    | (pth, lb) <- itoList $ identify tr,
      let focus = tr ^. subTreeAtL pth,
      let currentDepth = depth focus,
      -- Subtract 2 because leaves have depth one and are not scaled.
      let w = pWeight $ min (fromPWeight wMin + currentDepth - 2) (fromPWeight wMax),
      -- Do not scale the leaves.
      not $ null $ forest focus,
      -- Filter other nodes.
      hn pth
  ]
  where
    name lb = n <> PName (" node " ++ show lb)

slideRootContrarilyJacobian ::
  -- Number of inner nodes.
  Int ->
  -- Scaling factor u for heights.
  Double ->
  -- Scaling factors xi_j for rates. Usually those are just two factors, but
  -- let's keep it general.
  [Double] ->
  Jacobian
slideRootContrarilyJacobian n u xis =
  Exp $
    sum $
      -- Minus n: Scaling the time tree node heights contrarily.
      fromIntegral (-n) * log u
        :
        -- Scaling the rate branches.
        map log xis

slideRootContrarilyPFunction ::
  Int ->
  StandardDeviation Double ->
  TuningParameter ->
  PFunction (Double, HeightTree Double, LengthTree Double)
slideRootContrarilyPFunction n s t (ht, HeightTree tTr, LengthTree rTr) g = do
  let tTrHeight = branch tTr
  when
    (abs (tTrHeight - 1.0) > 1e-14)
    ( error $
        "slideRootContrarilyPFunction: Height of relative time tree is different from 1: "
          <> show tTrHeight
          <> "."
    )
  -- Do not use 'genericContinuous' because the forward operator and the
  -- Jacobian function need access to the time tree node heights.
  (ht', q) <- truncatedNormalSample ht s t htOldestChild (1 / 0) g
  -- Scaling factor of absolute time tree height. This is the reverse scaling
  -- factor of the time tree node heights.
  let u = let x = ht' / ht in assertWith (> 0) x
  -- Scaling factors for rates.
  let getXi h = let x = (1 - h) / (u - h) in assertWith (> 0) x
      xis = map getXi htsChildren
  -- Compute new state.
  let tTr' = tTr & forestL %~ map (first (/ u))
      rTr' = rTr & forestL %~ zipWith (modifyStem . (*)) xis
      j = slideRootContrarilyJacobian n u xis
      x' = (ht', HeightTree tTr', LengthTree rTr')
  return (Propose x' q j, Nothing)
  where
    htsChildren = map branch $ forest tTr
    -- Absolute height of oldest child.
    htOldestChild = ht * maximum htsChildren

-- | Specific proposal sliding the absolute time height while leaving the
-- absolute heights of internal time tree nodes untouched.
--
-- The proposal works on a parameter triple \((H, t, r)\), where \(H\) is the
-- absolute height of the time tree, \(t\) is a relative time tree, and \(r\) is
-- an absolute or relative rate tree.
--
-- Use a truncated normal distribution with given standard deviation to propose
-- a new height \(H'\) such that \(H'\) is larger than the highest daughter node
-- of the root. Let \(H'=H*u\). Scale all node heights of \(t\) contrarily. That
-- is, let \(I\) be the index set traversing the nodes of the time tree
-- excluding the leaves and the root. For any \(i \in I\), the node height
-- \(t_i\) will become \(t_i'=t_i/u\). Further, propose new rates for the rate
-- tree branches \(r_j\) leading to the root node. In particular,
-- \(r_j'=r_j\frac{1-t_j}{u-t_j}\), where \(t_j\) is the height of the node
-- corresponding to branch \(j\). In this way, the expected number of
-- substitutions on all branches stays constant.
--
-- Call 'error' if:
--
-- - The height of the relative time tree is different from 1.
slideRootContrarily ::
  -- | The topology of the tree is used to precompute the number of inner nodes.
  Tree e a ->
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (Double, HeightTree Double, LengthTree Double)
slideRootContrarily tr s =
  createProposal
    description
    (slideRootContrarilyPFunction n s)
    PFast
    -- 1: Slide absolute time height.
    -- n: Scale inner nodes of time tree.
    -- k: Scale the rate tree branches leading to the root.
    (PDimension $ 1 + n + k)
  where
    description = PDescription $ "Slide root contrarily; sd: " ++ show s
    n = nInnerNodes tr
    k = length $ forest tr

-- See also 'scaleSubTreeAtUltrametricPFunction'.
scaleSubTreeAtContrarilyPFunction ::
  -- Number of inner nodes.
  Int ->
  -- Number of branches.
  Int ->
  Path ->
  StandardDeviation Double ->
  TuningParameter ->
  PFunction (HeightTree Double, LengthTree Double)
scaleSubTreeAtContrarilyPFunction nNodes nBranches pth sd t (HeightTree tTr, LengthTree rTr) g
  | null tTrChildren =
      error "scaleSubTreeAtContrarilyPFunction: Sub tree of ultrametric tree is a leaf."
  | null rTrChildren =
      error "scaleSubTreeAtContrarilyPFunction: Sub tree of unconstrained tree is a leaf."
  | otherwise = do
      (hTTrNode', q) <- truncatedNormalSample hTTrNode sd t 0 hTTrParent g
      -- Time tree.
      let -- Scaling factor of time tree nodes heights (xi, not x_i).
          xiT = let x = hTTrNode' / hTTrNode in assertWith (> 0) x
          tTr' = toTree $ tTrPos & currentTreeL %~ scaleUltrametricTreeF hTTrNode' xiT
      -- Rate tree.
      let -- Scaling factor of rate tree branches excluding the stem.
          xiR = recip xiT
          -- Scaling factor of rate tree stem.
          xiStemR =
            -- If root node is handled, the stem scaling factor is 1.0.
            if null pth
              then 1.0
              else
                let x = (hTTrParent - hTTrNode) / (hTTrParent - hTTrNode')
                 in assertWith (> 0) x
          f =
            -- If the root node is handled, do not scale the stem because no upper
            -- bound is set.
            if null pth
              then scaleUnconstrainedTreeWithoutStemF xiR
              else modifyStem (* xiStemR) . scaleUnconstrainedTreeWithoutStemF xiR
          rTr' = toTree $ rTrPos & currentTreeL %~ f
      -- New state.
      let x' = (HeightTree tTr', LengthTree rTr')
          -- jacobianTimeTree = Exp $ fromIntegral (nNodes - 1) * log xi
          -- jacobianRateTree = Exp $ fromIntegral (nBranches -1) * log xi' + log xiStem
          jacobian = Exp $ fromIntegral (nNodes - nBranches) * log xiT + log xiStemR
      return (Propose x' q jacobian, Nothing)
  where
    -- Time tree.
    tTrPos = goPathUnsafe pth $ fromTree tTr
    tTrFocus = current tTrPos
    tTrParent = current $ goParentUnsafe tTrPos
    tTrChildren = forest tTrFocus
    hTTrNode = branch tTrFocus
    -- If the root node is handled, set the upper bound to +Infinity because no
    -- parent node exists.
    hTTrParent = if null pth then 1 / 0 else branch tTrParent
    -- Rate tree.
    rTrPos = goPathUnsafe pth $ fromTree rTr
    rTrFocus = current rTrPos
    rTrChildren = forest rTrFocus

-- | Scale the sub trees at given path.
--
-- This proposal acts on a pair of trees. The first tree is an ultrametric tree
-- (see "Mcmc.Tree.Proposal.Ultrametric"), usually the time tree with branches
-- denoting absolute or relative time. The second tree is an unconstrained tree
-- (see "Mcmc.Tree.Proposal.Unconstrained"), usually the rate tree with branches
-- denoting absolute or relative rates.
--
-- The sub trees of both trees are scaled contrarily. For example, if the sub
-- tree of the ultrametric tree is magnified, the sub tree of the unconstrained
-- tree is shrunk. In order to maintain ultrametricity of the ultrametric tree,
-- the stem of the sub tree is shortened. Correspondingly, the stem of the
-- unconstrained tree is elongated.
--
-- A normal distribution truncated at the height of the parent node of the
-- ultrametric tree and the leaves is used to determine the new height of the
-- sub tree of the ultrametric tree.
--
-- For reference, please see
-- 'Mcmc.Tree.Proposal.Ultrametric.scaleSubTreeAtUltrametric', and
-- 'scaleSubTreeAt'.
--
-- NOTE: When applying to the root node (1) the tree heights change contrarily,
-- (2) no upper bound is used because no parent node exists, and (3) the stem of
-- the unconstrained tree is not changed because it does not map to a valid
-- branch of the ultrametric tree.
--
-- NOTE: Application of this proposal to trees with different topologies will
-- lead to unexpected behavior and possibly to run time errors.
--
-- Call 'error' if:
--
-- - The path is invalid.
--
-- - The path leads to a leaf.
--
-- - A node height or branch length is zero.
scaleSubTreesAtContrarily ::
  -- | The topology of the tree is used to precompute the number of inner nodes.
  Tree e a ->
  Path ->
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (HeightTree Double, LengthTree Double)
scaleSubTreesAtContrarily tr pth sd
  | not $ isValidPath tr pth = error $ "scaleSubTreesAtContrarily: Path is invalid: " <> show pth <> "."
  | isLeafPath tr pth = error $ "scaleSubTreesAtContrarily: Path leads to a leaf: " <> show pth <> "."
  | otherwise =
      createProposal
        description
        (scaleSubTreeAtContrarilyPFunction nNodes nBranches pth sd)
        PFast
        (PDimension $ nNodes + nBranches)
  where
    description = PDescription $ "Scale sub trees contrarily; sd: " <> show sd
    subtree = current $ goPathUnsafe pth $ fromTree tr
    nNodes = nInnerNodes subtree
    nBranches = length subtree

-- | Scale the sub trees of two trees contrarily.
--
-- See 'scaleSubTreesAtContrarily'.
--
-- The weights are assigned as described in
-- 'Mcmc.Tree.Proposal.Ultrametric.scaleSubTreesUltrametric'.
--
-- Do not scale the leaves.
scaleSubTreesContrarily ::
  Tree e a ->
  HandleNode ->
  StandardDeviation Double ->
  PName ->
  -- | Minimum weight.
  PWeight ->
  -- | Maximum weight.
  PWeight ->
  Tune ->
  [Proposal (HeightTree Double, LengthTree Double)]
scaleSubTreesContrarily tr hn s n wMin wMax t =
  [ scaleSubTreesAtContrarily tr pth s (name lb) w t
    | (pth, lb) <- itoList $ identify tr,
      let focus = tr ^. subTreeAtL pth,
      let currentDepth = depth focus,
      let w = pWeight $ min (fromPWeight wMin + currentDepth - 2) (fromPWeight wMax),
      not $ null $ forest focus,
      hn pth
  ]
  where
    name lb = n <> PName (" node " ++ show lb)

scaleRatesAndTreeContrarilyPFunction ::
  Int ->
  StandardDeviation Double ->
  TuningParameter ->
  PFunction (Double, Double, HeightTree Double)
scaleRatesAndTreeContrarilyPFunction nNodes sd tp (la, mu, HeightTree tTr) g
  | nNodes < 1 = error "scaleRatesAndTreeContrarilyPFunction: no internal nodes to scale"
  | otherwise = do
      (tTrMaxChildHeight', q) <- truncatedNormalSample tTrMaxChildHeight sd tp 0 tTrHeight g
      -- Time tree.
      let -- Scaling factor of time tree nodes heights excluding the root node (xi, not x_i).
          xi = let x = tTrMaxChildHeight' / tTrMaxChildHeight in assertWith (> 0) x
          tTr' = scaleUltrametricTreeF tTrHeight xi tTr
      -- Birth rate of the time tree.
      let la' = la / xi
      -- Mean rate of the rate tree.
      let mu' = mu / xi
      -- New state.
      let x' = (la', mu', HeightTree tTr')
          -- jacobianTimeTree = Exp $ fromIntegral (nNodes - 1) * log xi
          -- jacobianRates = Exp $ (-2) * log xi
          jacobian = Exp $ fromIntegral (nNodes - 1 - 2) * log xi
      return (Propose x' q jacobian, Nothing)
  where
    -- Time tree.
    tTrHeight = branch tTr
    tTrMaxChildHeight = maximum $ map branch $ forest tTr

-- | Scale the birth rate, the mean rate, and the time tree contrarily such that
-- prior roughly stays constant.
--
-- This proposal acts on two rates and an ultrametric tree, usually the time
-- tree with branches denoting absolute or relative time. The first rate is the
-- birth rate of the ultrametric tree. The second rate is the mean rate.
--
-- The root node height of the ultrametric tree is not changed.
--
-- When the internal node ages of the ultrametric tree are increased both rates
-- are decreased.
--
-- A normal distribution truncated at the height of the root node of the
-- ultrametric tree and the leaves is used to determine the new node heights of
-- the ultrametric tree.
--
-- Call 'error' if:
--
-- - The ultrametric tree does not contain nodes to be scaled.
--
-- - A node height or branch length is zero.
scaleRatesAndTreeContrarily ::
  Tree e a ->
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  -- | Proposal (timeBirthRate, rateMean, timeTree).
  Proposal (Double, Double, HeightTree Double)
scaleRatesAndTreeContrarily tr sd =
  createProposal
    description
    (scaleRatesAndTreeContrarilyPFunction nNodes sd)
    PFast
    (PDimension $ nNodes + 2)
  where
    description = PDescription $ "Scale rates and tree contrarily; sd: " <> show sd
    -- -1: Do not scale the root node height.
    nNodes = nInnerNodes tr - 1
