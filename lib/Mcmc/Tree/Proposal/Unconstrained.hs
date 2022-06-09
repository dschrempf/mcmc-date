-- |
-- Module      :  Mcmc.Tree.Proposal.Unconstrained
-- Description :  Proposals on trees with unconstrained branch lengths
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jul 23 09:10:07 2020.
--
-- For proposals on ultrametric trees, see "Mcmc.Tree.Proposal.Ultrametric".
module Mcmc.Tree.Proposal.Unconstrained
  ( scaleBranchAt,
    scaleBranches,
    scaleSubTreeAt,
    scaleSubTrees,
    pulley,
    scaleNormAndTreeContrarily,
    scaleVarianceAndTree,

    -- * Helper functions
    scaleUnconstrainedTreeF,
    scaleUnconstrainedTreeWithoutStemF,
  )
where

import Control.Lens
import Data.Bifunctor
import ELynx.Tree
import Mcmc.Proposal
import Mcmc.Proposal.Generic
import Mcmc.Proposal.Scale
import Mcmc.Statistics.Types
import Mcmc.Tree.Import ()
import Mcmc.Tree.Lens
import Mcmc.Tree.Proposal.Internal
import Mcmc.Tree.Types
import Numeric.Log hiding (sum)
import Statistics.Distribution.Gamma
import System.Random.MWC

scaleBranch ::
  Shape Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (Tree Double a)
scaleBranch s n w t = branchL @~ scaleUnbiased s n w t

-- | Scale branch at given node.
--
-- See 'scaleUnbiased'.
scaleBranchAt ::
  Path ->
  Shape Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (LengthTree Double)
scaleBranchAt pth s n w t = lengthTreeL . subTreeAtL pth @~ scaleBranch s n w t

-- | Scale the branches of a given tree.
--
-- See 'scaleBranchAt'.
scaleBranches ::
  Tree e a ->
  HandleNode ->
  Shape Double ->
  -- | Base name of proposals.
  PName ->
  PWeight ->
  Tune ->
  [Proposal (LengthTree Double)]
scaleBranches tr hn s n w t =
  [ lengthTreeL . subTreeAtL pth
      @~ scaleBranch s (name lb) w t
    | (pth, lb) <- itoList $ identify tr,
      -- Filter nodes.
      hn pth
  ]
  where
    name lb = n <> PName (" branch " ++ show lb)

scaleTreeJacobian ::
  -- Number of branches.
  Int ->
  a ->
  Double ->
  Jacobian
scaleTreeJacobian n _ u = Exp $ fromIntegral (n - 2) * log u

scaleTreeSimple ::
  -- Number of branches.
  Int ->
  Shape Double ->
  TuningParameter ->
  Propose (Tree Double a)
scaleTreeSimple n k t =
  genericContinuous
    (gammaDistr (k / t) (t / k))
    (flip scaleUnconstrainedTreeF)
    (Just recip)
    (Just $ scaleTreeJacobian n)

scaleTree ::
  Tree e a ->
  Shape Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (Tree Double b)
scaleTree tr k = createProposal description (scaleTreeSimple n k) PFast (PDimension n)
  where
    description = PDescription $ "Scale tree; shape: " ++ show k
    n = length tr

-- | Scale all branches including the stem at given node.
--
-- A gamma distributed kernel of given shape is used. The scale is set such that
-- the mean is 1.
--
-- NOTE: Because the determinant of the Jacobian matrix depends on the number of
-- branches scaled, this proposal is only valid if all branch lengths including
-- the stem are unconstrained and strictly positive.
scaleSubTreeAt ::
  Path ->
  -- | The topology of the tree is used to precompute the number of inner nodes.
  Tree e a ->
  Shape Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (LengthTree Double)
scaleSubTreeAt pth tr k n w t = lengthTreeL . subTreeAtL pth @~ scaleTree tr k n w t

-- | Scale the sub trees of a given tree.
--
-- See 'scaleSubTreeAt'.
--
-- The weights are assigned as described in
-- 'Mcmc.Tree.Proposal.Ultrametric.scaleSubTreesUltrametric'.
--
-- Do not scale the leaves.
scaleSubTrees ::
  Tree e a ->
  HandleNode ->
  Shape Double ->
  -- | Base name of proposals.
  PName ->
  -- | Minimum weight.
  PWeight ->
  -- | Maximum weight.
  PWeight ->
  Tune ->
  [Proposal (LengthTree Double)]
scaleSubTrees tr hn s n wMin wMax t =
  [ lengthTreeL . subTreeAtL pth
      @~ scaleTree focus s (name lb) w t
    | (pth, lb) <- itoList $ identify tr,
      let focus = tr ^. subTreeAtL pth,
      let currentDepth = depth focus,
      -- Subtract 2 because leaves have depth one and are not scaled.
      let w = pWeight $ minimum [fromPWeight wMin + currentDepth - 2, fromPWeight wMax],
      -- Do not scale the leaves, because 'scaleBranch' is faster.
      not $ null $ forest focus,
      -- Filter other nodes.
      hn pth
  ]
  where
    name lb = n <> PName (" node " ++ show lb)

-- See 'truncatedNormalSample'. U is added to the left branch. I.e., if u is
-- positive, the left branch is elongated.
pulleyTruncatedNormalSample ::
  StandardDeviation Double ->
  TuningParameter ->
  Tree Double a ->
  GenIO ->
  IO (Double, Log Double)
pulleyTruncatedNormalSample s t (Node _ _ [l, r])
  | brL <= 0 =
      error $
        "pulleyTruncatedNormalSample: Left branch is zero or negative: " ++ show brL ++ "."
  | brR <= 0 =
      error $
        "pulleyTruncatedNormalSample: Right branch is zero or negative: " ++ show brR ++ "."
  | otherwise = truncatedNormalSample 0 s t a b
  where
    brL = branch l
    brR = branch r
    a = negate brL
    b = brR
pulleyTruncatedNormalSample _ _ _ = error "pulleyTruncatedNormalSample: Node is not bifurcating."

pulleySimple :: StandardDeviation Double -> TuningParameter -> Propose (LengthTree Double)
pulleySimple s t (LengthTree tr@(Node br lb [l, r])) g = do
  (u, q) <- pulleyTruncatedNormalSample s t tr g
  let tr' =
        Node
          br
          lb
          [ l & branchL +~ u,
            r & branchL -~ u
          ]
  -- The determinant of the Jacobian matrix is (-1).
  pure (Suggest (LengthTree tr') q 1, Nothing)
pulleySimple _ _ _ _ = error "pulleySimple: Node is not bifurcating."

-- | Use a node as a pulley.
--
-- Change the daughter branch lengths contrarily. The left and right daughter
-- branches are elongated/shortened by the same amount.
--
-- The heights of the two sub trees change.
--
-- Call 'error' if:
--
-- - The node is not bifurcating.
pulley ::
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (LengthTree Double)
pulley s = createProposal description (pulleySimple s) PFast (PDimension 2)
  where
    description = PDescription $ "Pulley; sd: " ++ show s

scaleNormAndTreeContrarilyFunction ::
  (Double, LengthTree Double) ->
  Double ->
  (Double, LengthTree Double)
-- Do not touch the stem, because this leads to problems with negative stem
-- lengths (or NaNs).
scaleNormAndTreeContrarilyFunction (x, LengthTree tr) u =
  (x / u, LengthTree $ scaleUnconstrainedTreeWithoutStemF u tr)

scaleNormAndTreeContrarilySimple ::
  -- Number of branches.
  Int ->
  Shape Double ->
  TuningParameter ->
  Propose (Double, LengthTree Double)
scaleNormAndTreeContrarilySimple n k t =
  genericContinuous
    (gammaDistr (k / t) (t / k))
    scaleNormAndTreeContrarilyFunction
    (Just recip)
    (Just jacobianFunction)
  where
    -- Minus 2 because of reverse transform.
    -- Minus 1 because of scaling the norm contrarily.
    jacobianFunction _ u = Exp $ fromIntegral (n - 2 - 1) * log u

-- | Let the branch lengths of an unconstrained tree be normalized with a
-- multiplicative factor. This proposal scales the normalization factor and the
-- branches contrarily.
--
-- NOTE: Because the determinant of the Jacobian matrix depends on the number of
-- branches scaled, this proposal is only valid if all inner branch lengths are
-- unconstrained and strictly positive. The stem is ignored.
--
-- See also 'scaleVarianceAndTree'.
scaleNormAndTreeContrarily ::
  -- | The topology of the tree is used to precompute the number of inner branches.
  Tree e a ->
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (Double, LengthTree Double)
scaleNormAndTreeContrarily tr sd =
  createProposal
    description
    (scaleNormAndTreeContrarilySimple nBranches sd)
    PFast
    (PDimension $ nBranches + 1)
  where
    description = PDescription $ "Scale norm and tree contrarily; sd: " <> show sd
    -- Ignore the stem, see note above.
    nBranches = length tr - 1

scaleVarianceAndTreeFunction ::
  Int ->
  (Double, LengthTree Double) ->
  Double ->
  (Double, LengthTree Double)
-- Do not touch the stem, this leads to problems with negative stem lengths (or
-- NaNs).
scaleVarianceAndTreeFunction n (x, LengthTree tr) u =
  (x * u * u, LengthTree $ tr & forestL %~ map (first f))
  where
    -- The calculation of sample mean requires a separate traversal of the tree.
    -- This may be slow.
    --
    -- Specifically ignore the stem.
    s = sum $ concatMap branches $ forest tr
    -- Sample mean. We assume that 'n' does not count the stem.
    mu = s / fromIntegral n
    -- Force NaN when new value is negative. This avoids numerical problems with
    -- negative branch lengths.
    f b = let b' = (b - mu) * u + mu in if b' > 0 then b' else 0 / 0

scaleVarianceAndTreeSimple ::
  -- Number of branches.
  Int ->
  Shape Double ->
  TuningParameter ->
  Propose (Double, LengthTree Double)
scaleVarianceAndTreeSimple n k t =
  genericContinuous
    (gammaDistr (k / t) (t / k))
    (scaleVarianceAndTreeFunction n)
    (Just recip)
    (Just jacobianFunction)
  where
    n1 = recip (fromIntegral n)
    -- Minus 2 because of the reverse transform.
    -- Plus 2 because of the scaling of the variance.
    -- Is 0 :).
    -- However, n times a complicated factor because the mean is used.
    jacobianFunction _ u = Exp $ fromIntegral n * log (u - n1 * u + n1)

-- | Let the branch lengths of an unconstrained tree be distributed according to
-- a distribution with a given variance hyper-parameter. This proposal scales
-- the variance hyper-parameter and the branches. The branches are scaled such
-- that the sample variance changes in the same way as the variance
-- hyper-parameter.
--
-- NOTE: The behavior of this proposal is opposite to
-- 'scaleNormAndTreeContrarily'.
--
-- NOTE: Because the determinant of the Jacobian matrix depends on the number of
-- branches scaled, this proposal is only valid if all inner branch lengths are
-- unconstrained and strictly positive. The stem is ignored.
--
-- NOTE: The sample mean is used to scale the parameters. This has some
-- disadvantages:
--
-- (1) It is slow.
--
-- (2) The sample mean may be off considerably. However, the actual mean of the
--     distribution is, in general, unknown.
--
-- (3) Negative branch lengths may arise. If this is the case, the branch length
--     are set to @NaN@, and the state will be rejected by the sampler if
--     appropriate prior or likelihood functions are used.
scaleVarianceAndTree ::
  -- | The topology of the tree is used to precompute the number of inner branches.
  Tree e a ->
  StandardDeviation Double ->
  PName ->
  PWeight ->
  Tune ->
  Proposal (Double, LengthTree Double)
scaleVarianceAndTree tr sd =
  createProposal
    description
    (scaleVarianceAndTreeSimple nBranches sd)
    PFast
    (PDimension $ nBranches + 1)
  where
    description = PDescription $ "Scale variance and tree; sd: " <> show sd
    -- Ignore the stem, see note above.
    nBranches = length tr - 1

-- | Scale the branches of an unconstrained tree.
scaleUnconstrainedTreeF :: Double -> Tree Double a -> Tree Double a
scaleUnconstrainedTreeF u = first (* u)

-- | Scale the branches of an unconstrained tree. Do not scale the stem.
scaleUnconstrainedTreeWithoutStemF :: Double -> Tree Double a -> Tree Double a
scaleUnconstrainedTreeWithoutStemF u tr = tr & forestL %~ map (scaleUnconstrainedTreeF u)
