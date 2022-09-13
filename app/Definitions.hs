-- |
-- Module      :  Definitions
-- Description :  State space, prior function, likelihood function and more.
-- Copyright   :  2021 Dominik Schrempf
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
    burnInInformed,
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
import qualified Statistics.Sample as S

{- ORMOLU_DISABLE -}
-- ELynx tree library.
import ELynx.Tree

-- Mcmc library.
import Mcmc

-- Local library.
import Mcmc.Tree

-- Local modules.
import Hamiltonian
import Monitor
import Probability
import State
import Tools
import qualified Data.Vector as VB
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

-- NOTE: Proposals with [R] include the root, and so need the specific jacobian
-- function 'jacobianRootBranch'. Other proposals on the trees start with [O]
-- for other nodes, or [B] for braces.

-- Proposals on the time tree.
proposalsTimeTree :: [Brace Double] -> Tree e a -> [Proposal I]
proposalsTimeTree bs t =
  map (liftProposalWith jacobianRootBranch timeTree) psAtRoot
    ++ map (liftProposal timeTree) psOthers
    ++ map (liftProposal timeTree) psBraces
  where
    -- Pulley on the root node.
    nR = PName "[R] Time tree"
    maybePulley = case t of
      Node _ _ [l, r]
        | null (forest l) -> []
        | null (forest r) -> []
        | otherwise -> [pulleyUltrametric t 0.01 nR (pWeight 6) Tune]
      _ -> error "maybePulley: Tree is not bifurcating."
    ps hn n =
      slideNodesUltrametric t hn 0.01 n (pWeight 5) Tune
        ++ scaleSubTreesUltrametric t hn 0.01 n (pWeight 3) (pWeight 8) Tune
    psAtRoot = maybePulley ++ ps childrenOfRoot nR
    nO = PName "[O] Time tree"
    psOthers = ps otherNodes nO
    nPB = PName "[B] Time tree"
    psBraces = [slideBracedNodesUltrametric t b 0.01 nPB (pWeight 5) Tune | b <- bs]

-- Lens for proposals on the rate mean and rate tree.
rateMeanRateTreeL :: Lens' I (Double, LengthTree Double)
rateMeanRateTreeL = tupleLens rateMean rateTree

-- Lens for proposals on the rate variance and rate tree.
rateVarianceRateTreeL :: Lens' I (Double, LengthTree Double)
rateVarianceRateTreeL = tupleLens rateVariance rateTree

rateMeanVarianceTreeL :: Lens' I (Double, Double, LengthTree Double)
rateMeanVarianceTreeL = tripleLens rateMean rateVariance rateTree

-- Proposals on the rate tree.
proposalsRateTree :: Tree e a -> [Proposal I]
proposalsRateTree t =
  liftProposalWith jacobianRootBranch rateMeanRateTreeL pMeanContra
    : liftProposalWith jacobianRootBranch rateVarianceRateTreeL pVarianceUncorrelated
    : liftProposalWith jacobianRootBranch rateMeanVarianceTreeL pVarianceAutocorrelated
    : map (liftProposalWith jacobianRootBranch rateTree) psAtRoot
    ++ map (liftProposal rateTree) psOthers
  where
    w = weightNBranches $ length t
    -- I am proud of the next three proposals :).
    nMR = PName "[R] Rate mean, Rate tree"
    pMeanContra = scaleNormAndTreeContrarily t 100 nMR w Tune
    nVR = PName "[R] Rate variance, Rate tree"
    pVarianceUncorrelated = scaleVarianceAndTree t 100 nVR w Tune
    pVarianceAutocorrelated = scaleVarianceAndTreeAutocorrelated t 100 nVR w Tune
    ps hn n =
      scaleBranches t hn 100 n (pWeight 3) Tune
        ++ scaleSubTrees t hn 100 n (pWeight 3) (pWeight 8) Tune
    nR = PName "[R] Rate tree"
    psAtRoot = ps childrenOfRoot nR
    nO = PName "[O] Rate tree"
    psOthers = ps otherNodes nO

-- Contrary proposals on the time and rate trees.
proposalsTimeRateTreeContra :: Show a => [Brace Double] -> Tree e a -> [Proposal I]
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
    nR = PName "[C] [R] Trees"
    psAtRoot = ps childrenOfRoot nR
    nO = PName "[C] [O] Trees"
    psOthers = ps otherNodes nO
    nPB = PName "[C] [B] Trees"
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
    nH = PName "[R] Time height, Rate tree"
    psHeightContra = scaleNormAndTreeContrarily t 100 nH w Tune
    nRC = PName "[R] Trees"
    psSlideRoot = slideRootContrarily t 10 nRC w Tune

-- | The proposal cycle includes proposals for the other parameters.
proposals :: [Brace Double] -> Bool -> I -> Maybe (HTarget IG) -> Cycle I
proposals bs calibrationsAvailable x mHTarget =
  cycleFromList $
    [ timeBirthRate @~ scaleUnbiased 10 (PName "Time birth rate") w Tune,
      timeDeathRate @~ scaleUnbiased 10 (PName "Time death rate") w Tune,
      rateMean @~ scaleUnbiased 10 (PName "Rate mean") w Tune,
      rateVariance @~ scaleUnbiased 10 (PName "Rate variance") w Tune
    ]
      ++ maybeHamiltonianProposal
      ++ proposalsTimeTree bs t
      ++ proposalsRateTree t
      ++ proposalsTimeRateTreeContra bs t
      -- Only add proposals on time tree height when calibrations are available.
      ++ if calibrationsAvailable then proposalsChangingTimeHeight t else []
  where
    t = getLengthTree $ _rateTree x
    w = weightNBranches $ length t
    maybeHamiltonianProposal = case mHTarget of
      Nothing -> []
      Just htarget -> [liftProposalWith jacobianRootBranch id $ nutsWith calibrationsAvailable x htarget]

-- -- | Use the Hamiltonian proposal only.
-- proposals :: [Brace Double] -> Bool -> I -> Maybe (HTarget IG) -> Cycle I
-- proposals _ _ _ Nothing = error "proposals: No target provided."
-- proposals _ calibrationsAvailable x (Just htarget) =
--   cycleFromList
--     [liftProposalWith jacobianRootBranch id $ nutsWith calibrationsAvailable x htarget]

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
monConstrainedNodes :: [Constraint Double] -> [MonitorParameter I]
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

monBracedNodes :: [Brace Double] -> [MonitorParameter I]
monBracedNodes bs =
  [ getBraceVariance (map nodePath ns) >$< monitorDouble (name n)
    | b <- bs,
      let n = getBraceName b
          ns = getBraceNodes b
  ]
  where
    name s = "Brace " ++ s ++ " variance"

-- The file monitor is more verbose.
monFileParams :: [Calibration Double] -> [Constraint Double] -> [Brace Double] -> MonitorFile I
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

-- Monitor the individual parts of the prior function.
monFilePrior ::
  -- Initial, constant, approximate absolute time tree height.
  Double ->
  RelaxedMolecularClockModel ->
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  MonitorFile I
monFilePrior ht md cb cs bs =
  monitorFile
    "prior"
    [ monitorPriorCsKsBs cb cs bs,
      monitorPriorBirthDeath,
      monitorPriorRelaxedMolecularClock ht md
    ]
    2

-- | Monitor to standard output and files. Do not use any batch monitors for now.
monitor ::
  -- | Initial, constant, approximate absolute time tree height.
  Double ->
  RelaxedMolecularClockModel ->
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  Monitor I
monitor ht md cb cs bs =
  Monitor
    monStdOut
    [ monFileParams (VB.toList cb) (VB.toList cs) (VB.toList bs),
      monFileTimeTree,
      monFileRateTree,
      monFilePrior ht md cb cs bs
    ]
    []

-- | Number of burn in iterations and auto tuning period.
burnIn :: BurnInSettings
burnIn = BurnInWithCustomAutoTuning fast slow
  where
    fast = 10 : 10 : [10, 20 .. 90]
    slow = [100, 120 .. 400]

burnInInformed :: BurnInSettings
burnInInformed = BurnInWithCustomAutoTuning fast slow
  where
    fast = []
    slow = [100, 100, 100, 100, 100, 100, 100, 100, 200, 300, 400, 400]

-- | Number of burn in iterations when profiling is enabled.
burnInProf :: BurnInSettings
burnInProf = BurnInWithCustomAutoTuning fast slow
  where
    fast = [10, 10]
    slow = [20, 20]

-- | Number of iterations after burn in.
iterations :: Iterations
iterations = Iterations 8000

-- | Number of iterations after burn in when profiling is enabled.
iterationsProf :: Iterations
iterationsProf = Iterations 50

-- | Number of points of the stepping stone sampler.
nPoints :: NPoints
nPoints = NPoints 128

-- | Number of points of the stepping stone sampler.
nPointsProf :: NPoints
nPointsProf = NPoints 12

-- | Repetitive burn in at each point on the path.
repetitiveBurnIn :: BurnInSettings
repetitiveBurnIn = BurnInWithCustomAutoTuning fast slow
  where
    fast = [20, 40, 60, 80]
    slow = replicate 9 100

-- | Repetitive burn in at each point on the path when profiling is enabled.
repetitiveBurnInProf :: BurnInSettings
repetitiveBurnInProf = BurnInWithCustomAutoTuning fast slow
  where
    fast = [10, 20]
    slow = [30, 40]
