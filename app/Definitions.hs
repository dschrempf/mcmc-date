-- |
-- Module      :  Definitions
-- Description :  State space, prior function, likelihood function and more.
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Oct 22 20:05:05 2020.
--
-- NOTE: For large trees, mixing may be problematic. The chain mixes well for
-- small- and medium-sized trees, so the model per se is fine.
--
-- Measures that didn't help at all:
--
-- - Per-branch normal distributions in the likelihood function instead of a
--   multivariate normal distribution.
--
-- - Re-parametrization of the branches leading to the root: Using a single rate
--   for both branches.
--
-- - Re-parametrization of the time and rate trees: Use time and rate trees with
--   absolute branch lengths (and not branch lengths in relative time and rate).
--
-- - Re-parametrization of the birth and death rates: Use diversification rate
--   (diversification rate = birth rate - death rate) and a possibly fixed death
--   rate.
--
-- - Fix rate mean (remove hyper-prior on mean).
--
-- Measures that improved mixing:
--
-- - Non-uniform proposal weights for sub-tree proposals on the time and rate
--   trees. Proposals deeper in the tree which affect more parameters are
--   assigned higher weights.
--
-- - Contrary proposals (time and rate trees, time height and rate mean, rate
--   mean and rate tree).
--
-- Game changers:
--
-- - Contrary node slide proposal. This proposal improved mixing a lot for all
--   nodes accept the root node and the daughters.
--
-- - Slide root contrarily ('slideRootContrarily') proposal. This proposal
--   finally improved mixing for the root node and the daughters.
module Definitions
  ( initWith,
    proposals,
    monitor,
    burnIn,
    burnInProf,
    iterations,
    iterationsProf,
    nPoints,
    nPointsProf,
    repetitiveBurnIn,
    repetitiveBurnInProf,
  )
where

import Control.Lens
import Data.Bifunctor
import qualified Data.Vector.Unboxed as VU
import Numeric.Log hiding (sum)
import qualified Statistics.Sample as S

-- import Debug.Trace

-- Disable the syntax formatter Ormolu to highlight relevant module imports.
{- ORMOLU_DISABLE -}
-- ELynx tree library.
import ELynx.Tree

-- Mcmc library.
import Mcmc

-- Local Modules.
import Mcmc.Tree
import Hamiltonian
import State
import Tools
{- ORMOLU_ENABLE -}

-- | Initial state.
--
-- The given tree is used to initiate the time and rate trees. For the time
-- tree, the terminal branches are elongated such that the tree becomes
-- ultrametric ('makeUltrametric'). For the rate tree, we just use the topology
-- and set all rates to 1.0.
initWith :: Tree Length Name -> I
initWith t =
  I
    { _timeBirthRate = 1.0,
      _timeDeathRate = 1.0,
      _timeHeight = 1.0,
      _timeTree = initialTimeTree,
      _rateMean = 1.0,
      _rateVariance = 1.0,
      _rateTree = LengthTree $ setStem 0 $ first (const 1.0) t
    }
  where
    -- Treat a pathological case when branch lengths (excluding the stem) are
    -- zero. In this case, the initial state of the MCMC sampler would be
    -- invalid and the sampler would fail.
    --
    -- Ignore the root branch.
    bs = concatMap branches $ forest t
    n = length bs
    tAverage = sum bs / fromIntegral n
    -- Avoid branches of length zero. Exclude the stem.
    tPositiveBranches = t & forestL %~ map (first (\x -> if x == 0 then tAverage else x))
    -- Set the stem length to 0.
    tPositiveBranchesStemZero = setStem 0 tPositiveBranches
    initialTimeTree =
      toHeightTreeUltrametric $
        normalizeHeight $
          makeUltrametric tPositiveBranchesStemZero

-- The root splits the branch of the unrooted tree into two branches. This
-- function retrieves the root branch measured in expected number of
-- substitutions.
rootBranch :: I -> Double
rootBranch x = tH * rM * (t1 * r1 + t2 * r2)
  where
    (t1, t2) = case heightTreeToLengthTree $ x ^. timeTree of
      LengthTree (Node _ _ [l, r]) -> (branch l, branch r)
      _ -> error "rootBranch: Time tree is not bifurcating."
    (r1, r2) = case x ^. rateTree of
      LengthTree (Node _ _ [l, r]) -> (branch l, branch r)
      _ -> error "rootBranch: Rate tree is not bifurcating."
    tH = x ^. timeHeight
    rM = x ^. rateMean

-- This Jacobian is necessary to have unbiased proposals on the branches leading
-- to the root of the time tree.
jacobianRootBranch :: JacobianFunction I
jacobianRootBranch = Exp . log . recip . rootBranch

-- Weight of proposals not acting on individual branches. The larger the tree,
-- the higher the weight.
weightNBranches :: Int -> PWeight
weightNBranches n =
  let n' = fromIntegral n :: Double
   in pWeight $ floor $ logBase 1.3 n'

-- Handle children of the root.
childrenOfRoot :: HandleNode
childrenOfRoot = (== 1) . length

-- Handle other nodes.
otherNodes :: HandleNode
otherNodes = (> 1) . length

-- Proposals on the time tree.
proposalsTimeTree :: [Brace] -> Tree e a -> [Proposal I]
proposalsTimeTree bs t =
  map (liftProposalWith jacobianRootBranch timeTree) psAtRoot
    ++ map (liftProposal timeTree) psOthers
    ++ map (liftProposal timeTree) psBraces
  where
    -- Pulley on the root node.
    nP = PName "Time tree [R]"
    maybePulley = case t of
      Node _ _ [l, r]
        | null (forest l) -> []
        | null (forest r) -> []
        | otherwise -> [pulleyUltrametric t 0.01 nP (pWeight 6) Tune]
      _ -> error "maybePulley: Tree is not bifurcating."
    ps hn n =
      slideNodesUltrametric t hn 0.01 n (pWeight 5) Tune
        ++ scaleSubTreesUltrametric t hn 0.01 n (pWeight 3) (pWeight 8) Tune
    nR = PName "Time tree [R]"
    psAtRoot = maybePulley ++ ps childrenOfRoot nR
    nO = PName "Time tree [O]"
    psOthers = ps otherNodes nO
    nPB = PName "Time tree [B]"
    psBraces = [slideBracedNodesUltrametric t b 0.01 nPB (pWeight 5) Tune | b <- bs]

-- Lens for proposals on the rate mean and rate tree.
rateMeanRateTreeL :: Lens' I (Double, LengthTree Double)
rateMeanRateTreeL = tupleLens rateMean rateTree

-- Lens for proposals on the rate variance and rate tree.
rateVarianceRateTreeL :: Lens' I (Double, LengthTree Double)
rateVarianceRateTreeL = tupleLens rateVariance rateTree

-- Proposals on the rate tree.
proposalsRateTree :: Tree e a -> [Proposal I]
proposalsRateTree t =
  liftProposalWith jacobianRootBranch rateMeanRateTreeL psMeanContra :
  liftProposalWith jacobianRootBranch rateVarianceRateTreeL psVariance :
  map (liftProposalWith jacobianRootBranch rateTree) psAtRoot
    ++ map (liftProposal rateTree) psOthers
  where
    w = weightNBranches $ length t
    -- I am proud of the next three proposals :).
    nMR = PName "Rate mean, Rate tree [R]"
    psMeanContra = scaleNormAndTreeContrarily t 100 nMR w Tune
    nVR = PName "Rate variance, Rate tree [R]"
    psVariance = scaleVarianceAndTree t 100 nVR w Tune
    nR = PName "Rate tree [R]"
    ps hn n =
      scaleBranches t hn 100 n (pWeight 3) Tune
        ++ scaleSubTrees t hn 100 n (pWeight 3) (pWeight 8) Tune
    psAtRoot = ps childrenOfRoot nR
    nO = PName "Rate tree [O]"
    psOthers = ps otherNodes nO

-- Contrary proposals on the time and rate trees.
proposalsTimeRateTreeContra :: Show a => [Brace] -> Tree e a -> [Proposal I]
proposalsTimeRateTreeContra bs t =
  map (liftProposalWith jacobianRootBranch timeRateTreesL) psAtRoot
    ++ map (liftProposal timeRateTreesL) psOthers
    ++ map (liftProposal timeRateTreesL) psBraces
  where
    -- Lens for the contrary proposal on the trees.
    timeRateTreesL :: Lens' I (HeightTree Double, LengthTree Double)
    timeRateTreesL = tupleLens timeTree rateTree
    ps hn n =
      slideNodesContrarily t hn 0.1 n (pWeight 3) (pWeight 8) Tune
        ++ scaleSubTreesContrarily t hn 0.1 n (pWeight 3) (pWeight 8) Tune
    nR = PName "Trees [C] [R]"
    psAtRoot = ps childrenOfRoot nR
    nO = PName "Trees [C] [O]"
    psOthers = ps otherNodes nO
    nPB = PName "Trees [C] [B]"
    psBraces = [slideBracedNodesContrarily t b 0.1 nPB (pWeight 5) Tune | b <- bs]

-- Lens for a contrary proposal on the time height and rate mean.
timeHeightRateMeanL :: Lens' I (Double, Double)
timeHeightRateMeanL = tupleLens timeHeight rateMean

-- Lens for proposals on the rate mean and rate tree.
timeHeightRateTreeL :: Lens' I (Double, LengthTree Double)
timeHeightRateTreeL = tupleLens timeHeight rateTree

-- Lens for proposals on the triple (1) absolute time height, (2) relative time
-- tree, and (3) relative rate tree.
heightTimeRateTreesLens :: Lens' I (Double, HeightTree Double, LengthTree Double)
heightTimeRateTreesLens = tripleLens timeHeight timeTree rateTree

-- Proposals only activated when calibrations are available and absolute times
-- are estimated.
proposalsChangingTimeHeight :: Tree e a -> [Proposal I]
proposalsChangingTimeHeight t =
  [ timeHeight @~ scaleUnbiased 3000 (PName "Time height") w Tune,
    timeHeightRateMeanL @~ scaleContrarily 10 0.1 (PName "Time height, rate mean") w Tune,
    liftProposalWith jacobianRootBranch timeHeightRateTreeL psHeightContra,
    liftProposalWith jacobianRootBranch heightTimeRateTreesLens psSlideRoot
  ]
  where
    w = weightNBranches $ length t
    nH = PName "Time height, Rate tree [R]"
    psHeightContra = scaleNormAndTreeContrarily t 100 nH w Tune
    nRC = PName "Trees [R]"
    psSlideRoot = slideRootContrarily t 10 nRC w Tune

-- | The proposal cycle includes proposals for the other parameters.
proposals :: [Brace] -> Bool -> I -> (I -> I) -> Cycle I
proposals bs calibrationsAvailable x gradient =
  cycleFromList $
    [ timeBirthRate @~ scaleUnbiased 10 (PName "Time birth rate") w Tune,
      timeDeathRate @~ scaleUnbiased 10 (PName "Time death rate") w Tune,
      rateMean @~ scaleUnbiased 10 (PName "Rate mean") w Tune,
      rateVariance @~ scaleUnbiased 10 (PName "Rate variance") w Tune,
      liftProposalWith jacobianRootBranch id $ hmc calibrationsAvailable x gradient
    ]
      ++ proposalsTimeTree bs t
      ++ proposalsRateTree t
      ++ proposalsTimeRateTreeContra bs t
      -- Only add proposals on time tree height when calibrations are available.
      ++ if calibrationsAvailable then proposalsChangingTimeHeight t else []
  where
    t = getLengthTree $ _rateTree x
    w = weightNBranches $ length t

-- -- Hamiltonian proposal only.

-- -- | The proposal cycle includes proposals for the other parameters.
-- proposals :: Bool -> I -> (I -> I) -> Cycle I
-- proposals calibrationsAvailable x gradient =
--   cycleFromList
--     [liftProposalWith jacobianRootBranch id $ hmc calibrationsAvailable x gradient]

-- Monitor parameters.
monParams :: [MonitorParameter I]
monParams =
  [ _timeBirthRate >$< monitorDouble "TimeBirthRate",
    _timeDeathRate >$< monitorDouble "TimeDeathRate",
    _timeHeight >$< monitorDouble "TimeHeight",
    _rateMean >$< monitorDouble "RateMean",
    _rateVariance >$< monitorDouble "RateVariance"
  ]

-- Monitor to standard output.
monStdOut :: MonitorStdOut I
-- Screen width doesn't support more than four parameter monitors.
monStdOut = monitorStdOut (take 4 monParams) 2

-- Get the height of the node at path. Useful to have a look at calibrated nodes.
getTimeTreeNodeHeight :: Path -> I -> Double
getTimeTreeNodeHeight p x = (* h) $ t ^. subTreeAtL p . branchL
  where
    t = getHeightTree $ x ^. timeTree
    h = x ^. timeHeight

-- Monitor the height of calibrated nodes.
monCalibratedNodes :: [Calibration Double] -> [MonitorParameter I]
monCalibratedNodes cb =
  [ getTimeTreeNodeHeight p >$< monitorDouble (name n l)
    | c <- cb,
      let n = getCalibrationName c
          p = getCalibrationPath c
          l = getCalibrationInterval c
  ]
  where
    name nm iv = "Calibration " <> nm <> " " <> show iv

-- Get the difference in height of the nodes at path. Useful to have a look at
-- constrained nodes. Positive if constraint is honored.
getTimeTreeDeltaNodeHeight :: Path -> Path -> I -> Double
getTimeTreeDeltaNodeHeight y o x = getTimeTreeNodeHeight o x - getTimeTreeNodeHeight y x

-- Monitor the heights of constrained nodes.
monConstrainedNodes :: [Constraint] -> [MonitorParameter I]
monConstrainedNodes cs =
  [ getTimeTreeDeltaNodeHeight y o >$< monitorDouble (name n)
    | c <- cs,
      let n = getConstraintName c
          y = getConstraintYoungNodePath c
          o = getConstraintOldNodePath c
  ]
  where
    name s = "Constraint " ++ s

getBraceVariance :: [Path] -> I -> Double
getBraceVariance ps x = S.variance $ VU.fromList ts
  where
    ts = map (`getTimeTreeNodeHeight` x) ps

monBracedNodes :: [Brace] -> [MonitorParameter I]
monBracedNodes bs =
  [ getBraceVariance (map nodePath ns) >$< monitorDouble (name n)
    | b <- bs,
      let n = getBraceName b
          ns = getBraceNodes b
  ]
  where
    name s = "Brace " ++ s ++ " variance"

-- The file monitor is more verbose.
monFileParams :: [Calibration Double] -> [Constraint] -> [Brace] -> MonitorFile I
monFileParams cb cs bs =
  monitorFile
    "params"
    ( monParams
        -- ++ monPrior
        ++ monCalibratedNodes cb
        ++ monConstrainedNodes cs
        ++ monBracedNodes bs
    )
    2

-- Monitor the time tree with absolute branch lengths, because they are more
-- informative.
absoluteTimeTree :: I -> LengthTree Double
absoluteTimeTree s = fmap (* h) t
  where
    h = s ^. timeHeight
    t = heightTreeToLengthTree $ s ^. timeTree

-- The time tree with absolute branch lengths is written to a separate file.
monFileTimeTree :: MonitorFile I
monFileTimeTree = monitorFile "timetree" [absoluteTimeTree >$< monitorLengthTree "TimeTree"] 2

-- The rate tree with relative rates is written to a separate file.
monFileRateTree :: MonitorFile I
monFileRateTree = monitorFile "ratetree" [_rateTree >$< monitorLengthTree "RateTree"] 2

-- | Monitor to standard output and files. Do not use any batch monitors for now.
monitor :: [Calibration Double] -> [Constraint] -> [Brace] -> Monitor I
monitor cb cs bs =
  Monitor monStdOut [monFileParams cb cs bs, monFileTimeTree, monFileRateTree] []

-- | Number of burn in iterations and auto tuning period.
burnIn :: BurnInSettings
burnIn = BurnInWithCustomAutoTuning $ 10 : 10 : [10, 20 .. 400]

-- | Number of burn in iterations when profiling is enabled.
burnInProf :: BurnInSettings
burnInProf = BurnInWithCustomAutoTuning $ 10 : 10 : 20 : [20]

-- | Number of iterations after burn in.
iterations :: Iterations
iterations = Iterations 15000

-- | Number of iterations after burn in when profiling is enabled.
iterationsProf :: Iterations
iterationsProf = Iterations 100

-- | Number of points of the stepping stone sampler.
nPoints :: NPoints
nPoints = NPoints 128

-- | Number of points of the stepping stone sampler.
nPointsProf :: NPoints
nPointsProf = NPoints 12

-- | Repetitive burn in at each point on the path.
repetitiveBurnIn :: BurnInSettings
repetitiveBurnIn = BurnInWithCustomAutoTuning $ [20, 40, 60, 80] <> replicate 9 100

-- | Repetitive burn in at each point on the path when profiling is enabled.
repetitiveBurnInProf :: BurnInSettings
repetitiveBurnInProf = BurnInWithCustomAutoTuning [10, 20, 40]
