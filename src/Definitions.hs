{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Definitions
-- Description :  State space, prior function, likelihood function and more.
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Oct 22 20:05:05 2020.
module Definitions
  ( I (..),
    initWith,
    priorFunction,
    likelihoodFunction,
    proposals,
    monitor,
    burnIn,
    iterations,
    nPoints,
    initialBurnIn,
    repetitiveBurnIn,
  )
where

-- NOTE: For large trees, mixing is an issue. I tried:
--
-- - Using per-branch normal distributions in the likelihood function instead of
-- - a multivariate normal distribution.
--
-- - Re-parametrization of the branches leading to the root: Using a single rate
--   for both branches.
--
-- - Re-parametrization of the time and rate trees: Use time and rate trees with
--   absolute branch lengths (and not branch lengths in relative time and rate).
--
-- - Using the diversification rate (diversification rate = birth rate - death
--   rate) and a fixed death rate.
--
-- - Fix rate mean (remove hyper-prior on mean).
--
-- Measures that improved mixing:
--
-- - Non-uniform proposal weights for sub-tree proposals on the time and rate
--   trees.
--
-- - Contrary proposals (time and rate trees, time height and rate mean, rate
--   mean and rate tree).
--
-- I also observe that the chain mixes for small trees, so the model per se is
-- fine.

import Control.Lens
import Data.Aeson
import Data.Bifunctor
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import GHC.Generics
import qualified Numeric.LinearAlgebra as L
import Numeric.Log hiding (sum)
import Numeric.MathFunctions.Constants

-- import Debug.Trace

-- Disable the syntax formatter Ormolu to highlight relevant module imports.
{- ORMOLU_DISABLE -}
-- The ELynx library includes functions to work on trees.
import ELynx.Tree

-- The Mcmc library includes the Metropolis-Hastings-Green algorithm.
import Mcmc
import Mcmc.Tree

-- Local modules.
import Tools
{- ORMOLU_ENABLE -}

-- | State space containing all parameters.
--
-- We are interested in inferring an ultrametric tree with branch lengths
-- measured in units of time (e.g., in million years). Let T be the length of a
-- branch of the time tree, and R be the absolute evolutionary rate on this
-- branch. Then, the length of this very same branch measured in average number
-- of substitutions is d=T*R.
--
-- Internally, a relative time t and relative rate r are used such that the
-- branch length measured in average number of substitution is
-- d=T*R=(t*h)*(r*mu), where h is the root height of the time tree, and mu is
-- the mean rate.
--
-- In brief, the relative time and rate are defined as t=T/h, and r=R/mu, where
-- h is the root height and mu is the mean rate.
--
-- This has two advantages:
--
-- 1. The ultrametric tree object storing the relative times is a normalized
--    tree with root height 1.0.
--
-- 2. The relative rates have a mean of 1.0.
--
-- Remark: The topologies of the time and rate tree are equal. This is, however,
-- not ensured by the types. One could use one tree storing both, the times and
-- the rates.
data I = I
  { -- | Birth rate of relative time tree.
    _timeBirthRate :: Double,
    -- | Death rate of relative time tree.
    _timeDeathRate :: Double,
    -- | Height of the absolute time tree in unit time. Here, we use units of
    -- million years; see the calibrations.
    _timeHeight :: Double,
    -- | Normalized time tree of height 1.0. Branch labels denote relative
    -- times; node labels store relative node heights and names.
    _timeTree :: HeightTree Name,
    -- | The mean of the absolute rates.
    _rateMean :: Double,
    -- | The variance of the relative rates.
    _rateVariance :: Double,
    -- | Relative rate tree. Branch labels denote relative rates with mean 1.0;
    -- node labels store names.
    _rateTree :: Tree Length Name
  }
  deriving (Generic)

-- Create accessors (lenses) to the parameters in the state space.
makeLenses ''I

-- Allow storage of the trace as JSON.
instance ToJSON I

instance FromJSON I

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
      _timeTree = t',
      _rateMean = 1.0,
      _rateVariance = 1.0,
      _rateTree = setStem 0 $ first (const 1.0) t
    }
  where
    t' = either error id $ toHeightTreeUltrametric $ normalizeHeight $ makeUltrametric t

-- | Prior function.
priorFunction :: VB.Vector Calibration -> VB.Vector Constraint -> PriorFunction I
priorFunction cb cs (I l m h t mu va r) =
  product' $
    calibrateUnsafe 1e-3 cb h t :
    constrainUnsafe 1e-3 cs t :
    [ -- Birth and death rates of the relative time tree.
      exponential 1 l,
      exponential 1 m,
      -- No explicit prior on the height of the time tree. However, the height
      -- is calibrated (see above). If no calibrations are given, the height is
      -- set to 1.0.
      --
      -- Relative time tree.
      birthDeath ConditionOnTimeOfMrca l m 1.0 t',
      -- Mean rate.
      --
      -- IDEA: Use gamma distribution with mean calculated using the number of
      -- branches and the total length of the substitution-like tree.
      exponential 1 mu,
      -- Variance of the relative rates.
      exponential 1 va,
      -- Relative rate tree.
      uncorrelatedGamma withoutStem 1 va r
      -- autocorrelatedGamma withoutStem 1 va t' r
    ]
  where
    t' = fromHeightTree t

-- Log of density of multivariate normal distribution with given parameters.
-- https://en.wikipedia.org/wiki/Multivariate_normal_distribution.
logDensityMultivariateNormal ::
  -- Mean vector.
  VS.Vector Double ->
  -- Inverted covariance matrix.
  L.Matrix Double ->
  -- Log of determinant of covariance matrix.
  Double ->
  -- Value vector.
  VS.Vector Double ->
  Log Double
logDensityMultivariateNormal mu sigmaInv logSigmaDet xs =
  Exp $ c + (-0.5) * (logSigmaDet + ((dxs L.<# sigmaInv) L.<.> dxs))
  where
    dxs = xs - mu
    k = fromIntegral $ VS.length mu
    c = negate $ m_ln_sqrt_2_pi * k

-- | Approximation of the phylogenetic likelihood using a multivariate normal
-- distribution.
likelihoodFunction ::
  -- | Mean vector.
  VS.Vector Double ->
  -- | Inverted covariance matrix.
  L.Matrix Double ->
  -- | Log of determinant of covariance matrix.
  Double ->
  LikelihoodFunction I
likelihoodFunction mu sigmaInv logSigmaDet x =
  logDensityMultivariateNormal mu sigmaInv logSigmaDet distances
  where
    times = getBranches (fromHeightTree $ x ^. timeTree)
    rates = getBranches (x ^. rateTree)
    tH = x ^. timeHeight
    rMu = x ^. rateMean
    distances = VS.map (* (tH * rMu)) $ sumFirstTwo $ VS.zipWith (*) times rates

-- The root splits the branch of the unrooted tree into two branches. This
-- function retrieves the root branch measured in expected number of
-- substitutions.
rootBranch :: I -> Double
rootBranch x = t1 * r1 + t2 * r2
  where
    (t1, t2) = case fromHeightTree $ x ^. timeTree of
      Node _ _ [l, r] -> (fromLength $ branch l, fromLength $ branch r)
      _ -> error "rootBranch: Time tree is not bifurcating."
    (r1, r2) = case x ^. rateTree of
      Node _ _ [l, r] -> (fromLength $ branch l, fromLength $ branch r)
      _ -> error "rootBranch: Rate tree is not bifurcating."

-- This Jacobian is necessary to have unbiased proposals on the branches leading
-- to the root of the time tree.
jacobianRootBranch :: JacobianFunction I
jacobianRootBranch = Exp . log . recip . rootBranch

-- Proposals for the time tree.
proposalsTimeTree :: Show a => Tree e a -> [Proposal I]
proposalsTimeTree t =
  map (liftProposalWith jacobianRootBranch timeTree) psAtRoot
    ++ map (liftProposal timeTree) psOthers
  where
    nR = PName "Time tree [R]"
    nO = PName "Time tree [O]"
    -- Pulley on the root node (no Jacobian required because rate is shared).
    maybePulley = case t of
      Node _ _ [l, r]
        | null (forest l) -> []
        | null (forest r) -> []
        | otherwise -> [pulleyUltrametric t 0.01 nR (pWeight 6) Tune]
      _ -> error "maybePulley: Tree is not bifurcating."
    ps hd n =
      slideNodesUltrametric t hd 0.01 n (pWeight 3) Tune
        ++ scaleSubTreesUltrametric t hd 0.01 n (pWeight 3) (pWeight 8) Tune
    psAtRoot = maybePulley ++ ps (== 1) nR
    psOthers = ps (> 1) nO

-- Lens for a contrary proposal on the rate mean and rate tree.
rateMeanRateTreeL :: Lens' I (Double, Tree Length Name)
rateMeanRateTreeL = tupleLens rateMean rateTree

-- Lens for a contrary proposal on the rate variance and rate tree.
rateVarianceRateTreeL :: Lens' I (Double, Tree Length Name)
rateVarianceRateTreeL = tupleLens rateVariance rateTree

-- Proposals for the rate tree.
proposalsRateTree :: Show a => Tree e a -> [Proposal I]
proposalsRateTree t =
  liftProposalWith jacobianRootBranch rateMeanRateTreeL psMeanContra :
  liftProposalWith jacobianRootBranch rateVarianceRateTreeL psVarianceContra :
  map (liftProposalWith jacobianRootBranch rateTree) psAtRoot
    ++ map (liftProposal rateTree) psOthers
  where
    nR = PName "Rate tree [R]"
    nO = PName "Rate tree [O]"
    ps hd n =
      scaleBranches t hd 100 n (pWeight 3) Tune
        ++ scaleSubTrees t hd 100 n (pWeight 3) (pWeight 8) Tune
    -- I am proud of the next two proposals :).
    psMeanContra = scaleNormAndTreeContrarily t 100 nR (pWeight 3) Tune
    psVarianceContra = scaleVarianceAndTreeContrarily t 100 nR (pWeight 3) Tune
    psAtRoot = ps (== 1) nR
    psOthers = ps (> 1) nO

-- Contrary proposals on the time and rate trees.
proposalsTimeRateTreeContra :: Show a => Tree e a -> [Proposal I]
proposalsTimeRateTreeContra t =
  map (liftProposalWith jacobianRootBranch timeRateTreesL) psAtRoot
    ++ map (liftProposal timeRateTreesL) psOthers
  where
    -- Lens for a contrary proposal on the trees.
    timeRateTreesL :: Lens' I (HeightTree Name, Tree Length Name)
    timeRateTreesL = tupleLens timeTree rateTree
    nR = PName "Trees contra [R]"
    nO = PName "Trees contra [O]"
    ps hd n = scaleSubTreesContrarily t hd 0.01 n (pWeight 3) (pWeight 8) Tune
    psAtRoot = ps (== 1) nR
    psOthers = ps (> 1) nO

-- Lens for a contrary proposal on the time height and rate mean.
timeHeightRateMeanL :: Lens' I (Double, Double)
timeHeightRateMeanL = tupleLens timeHeight rateMean

-- | The proposal cycle includes proposals for the other parameters.
proposals :: Show a => Bool -> Tree e a -> Cycle I
proposals calibrationsAvailable t =
  cycleFromList $
    [ timeBirthRate @~ scaleUnbiased 10 (PName "Time birth rate") (pWeight 20) Tune,
      timeDeathRate @~ scaleUnbiased 10 (PName "Time death rate") (pWeight 20) Tune,
      rateMean @~ scaleUnbiased 10 (PName "Rate mean") (pWeight 20) Tune,
      rateVariance @~ scaleUnbiased 10 (PName "Rate variance") (pWeight 20) Tune
    ]
      ++ proposalsTimeTree t
      ++ proposalsRateTree t
      ++ proposalsTimeRateTreeContra t
      -- Only add proposals on time tree height when calibrations are available.
      ++ heightProposals
  where
    heightProposals =
      if calibrationsAvailable
        then
          [ timeHeight @~ scaleUnbiased 3000 (PName "Time height") (pWeight 20) Tune,
            timeHeightRateMeanL
              @~ scaleContrarily 10 0.1 (PName "Time height, rate mean") (pWeight 20) Tune
          ]
        else []

-- Monitor parameters.
monParams :: [MonitorParameter I]
monParams =
  [ _timeBirthRate >$< monitorDouble "TimeBirthRate",
    _timeDeathRate >$< monitorDouble "TimeDeathRate",
    _timeHeight >$< monitorDouble "TimeHeight",
    _rateMean >$< monitorDouble "RateMean",
    _rateVariance >$< monitorDouble "RateVariance",
    fromLength . sum . branches . _rateTree >$< monitorDouble "TotalRate"
  ]

-- Monitor to standard output.
monStdOut :: MonitorStdOut I
-- Screen width doesn't support more than four parameter monitors.
monStdOut = monitorStdOut (take 4 monParams) 2

-- Get the height of the node at path. Useful to have a look at calibrated nodes.
getTimeTreeNodeHeight :: Path -> I -> Double
getTimeTreeNodeHeight p x = (* h) $ fromHeight $ t ^. subTreeAtUnsafeL p . labelL . hasHeightL
  where
    t = x ^. timeTree
    h = x ^. timeHeight

-- Monitor the height of calibrated nodes.
monCalibratedNodes :: [Calibration] -> [MonitorParameter I]
monCalibratedNodes cb =
  [ getTimeTreeNodeHeight p >$< monitorDouble (name n i)
    | Calibration n p i <- cb
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
    | Constraint n y o <- cs
  ]
  where
    name s = "Constraint " ++ s

-- -- Monitor the separate parts of the prior function.
-- monPrior :: [MonitorParameter I]
-- monPrior =
--   [ (ln . exponential 1 . _timeBirthRate) >$< monitorDouble "TimeBirthRatePrior",
--     (ln . exponential 1 . _timeDeathRate) >$< monitorDouble "TimeDeathRatePrior",
--     (\x -> ln $ birthDeath ConditionOnTimeOfMrca (x ^. timeBirthRate) (x ^. timeDeathRate) 1.0 (fromHeightTree (x ^. timeTree))) >$< monitorDouble "TimeTreePrior",
--     (ln . exponential 1 . _rateMean) >$< monitorDouble "RateMeanPrior",
--     -- -- Variance of the relative rates.
--     (ln . exponential 1 . _rateVariance) >$< monitorDouble "RateVariancePrior",
--     (\x -> ln $ uncorrelatedGamma withoutStem 1 (x ^. rateVariance) (x ^. rateTree)) >$< monitorDouble "RateTreePrior"
--   ]

-- The file monitor is more verbose.
monFileParams :: [Calibration] -> [Constraint] -> MonitorFile I
monFileParams cb cs =
  monitorFile
    "params"
    ( monParams
        -- ++ monPrior
        ++ monCalibratedNodes cb
        ++ monConstrainedNodes cs
    )
    2

-- Monitor the time tree with absolute branch lengths, because they are more
-- informative.
absoluteTimeTree :: I -> Tree Length Name
absoluteTimeTree s = first (* h) t
  where
    h = either (error . (<>) "absoluteTimeTree: ") id $ toLength $ s ^. timeHeight
    t = fromHeightTree $ s ^. timeTree

-- The time tree with absolute branch lengths is written to a separate file.
monFileTimeTree :: MonitorFile I
monFileTimeTree = monitorFile "timetree" [absoluteTimeTree >$< monitorTree "TimeTree"] 2

-- The rate tree with relative rates is written to a separate file.
monFileRateTree :: MonitorFile I
monFileRateTree = monitorFile "ratetree" [_rateTree >$< monitorTree "RateTree"] 2

-- | Monitor to standard output and files. Do not use any batch monitors for now.
monitor :: [Calibration] -> [Constraint] -> Monitor I
monitor cb cs = Monitor monStdOut [monFileParams cb cs, monFileTimeTree, monFileRateTree] []

-- | Number of burn in iterations and auto tuning period.
burnIn :: BurnInSpecification
burnIn = BurnInWithCustomAutoTuning [10, 20 .. 300]

-- | Number of iterations after burn in.
iterations :: Iterations
iterations = Iterations 15000

-- | Number of points of the stepping stone sampler.
nPoints :: NPoints
nPoints = NPoints 128

-- | Initial burn in iterations and auto tuning period.
initialBurnIn :: BurnInSpecification
initialBurnIn = BurnInWithCustomAutoTuning [10, 20 .. 300]

-- | Repetitive burn in at each point on the path.
repetitiveBurnIn :: BurnInSpecification
repetitiveBurnIn = BurnInWithCustomAutoTuning $ [20, 40, 60, 80] <> replicate 9 100
