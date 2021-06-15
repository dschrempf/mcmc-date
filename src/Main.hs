{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Approximate phylogenetic likelihood with multivariate normal distribution
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Jul  3 21:27:38 2020.
module Main
  ( main,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as L
import Options
import System.Random.MWC hiding (uniform)

-- Disable the syntax formatter Ormolu to highlight relevant module imports.
{- ORMOLU_DISABLE -}
-- The ELynx library includes functions to work on trees.
import qualified ELynx.Topology as T
import ELynx.Tree

-- The Mcmc library includes the Metropolis-Hastings-Green algorithm.
import Mcmc hiding (Continue)
import Mcmc.Tree

-- Local modules (see comment above).
import Definitions
import Tools
{- ORMOLU_ENABLE -}

getMeanTreeFn :: String -> FilePath
getMeanTreeFn s = s <> ".meantree"

-- The rooted tree with posterior mean branch lengths will be stored in a file
-- with this name.
getMeanTree :: String -> IO (Tree Length Name)
getMeanTree = oneTree RevBayes . getMeanTreeFn

getDataFn :: String -> FilePath
getDataFn s = s <> ".data"

-- Get the posterior branch length means, the inverted covariance matrix, and
-- the determinant of the covariance matrix.
getData :: String -> IO (VS.Vector Double, L.Matrix Double, Double)
getData s = do
  (Just (mu, sigmaInvRows, logSigmaDet)) <- decodeFileStrict' $ getDataFn s
  let sigmaInv = L.fromRows sigmaInvRows
  return (mu, sigmaInv, logSigmaDet)

-- Get the posterior matrix of branch lengths. Merge the two branch lengths
-- leading to the root.
getPosteriorMatrixMergeBranchesToRoot :: [Tree Length a] -> L.Matrix Double
getPosteriorMatrixMergeBranchesToRoot = L.fromRows . map (sumFirstTwo . getBranches)

-- Get the posterior matrix of branch lengths.
getPosteriorMatrix :: [Tree Length a] -> L.Matrix Double
getPosteriorMatrix = L.fromRows . map (VS.fromList . map fromLength . branches)

-- Read in all trees, calculate posterior means and covariances of the branch
-- lengths, and find the midpoint root of the mean tree.
prepare :: PrepSpec -> IO ()
prepare (PrepSpec an rt ts) = do
  putStrLn "Read trees."
  treesAll <- someTrees RevBayes ts
  let nTrees = length treesAll
  putStrLn $ show nTrees ++ " trees read."

  let nBurnInTrees = nTrees `div` 10
  putStrLn $ "Skip a burn in of " ++ show nBurnInTrees ++ " trees."
  let trs = drop nBurnInTrees treesAll

  putStrLn "Check if trees have unique leaves."
  if any duplicateLeaves treesAll
    then error "prepare: Trees have duplicate leaves."
    else putStrLn "OK."

  putStrLn "Read rooted tree."
  treeRooted <- oneTree RevBayes rt

  putStrLn "Root the trees at the same point as the given rooted tree."
  let og = fst $ fromBipartition $ either error id $ bipartition treeRooted
      !treesRooted =
        force $
          map
            ( \x ->
                either error id $
                  outgroup og $
                    -- If midpoint rooting does not work, root the original
                    -- tree.
                    fromRight x
                    -- First root at midpoint. This treats a pathological case when
                    -- one branch length is zero, in which case the MCMC fails.
                    $
                      midpoint x
            )
            trs

  putStrLn "Check if topologies of the trees in the tree list are equal."
  putStrLn "Topology AND sub tree orders need to match."
  let differentTrees = nub $ map T.fromLabeledTree treesRooted
  if length differentTrees == 1
    then putStrLn "OK."
    else do
      putStrLn "Trees have different topologies:"
      BL.putStrLn $ BL.unlines $ map toNewickTopology differentTrees
      error "prepare: A single topology is required."

  putStrLn "Check the topology of the rooted tree."
  putStrLn "The topology has to match the one of the trees in the tree list."
  putStrLn "The sub tree orders may differ."
  let topoRooted = T.fromLabeledTree treeRooted
      topoHead = T.fromLabeledTree $ head treesRooted
  if T.equal' topoRooted topoHead
    then putStrLn "OK."
    else do
      putStrLn "Trees have different topologies."
      BL.putStrLn $ toNewickTopology topoRooted
      BL.putStrLn $ toNewickTopology topoHead
      error "prepare: A single topology is required."

  putStrLn "Get the posterior means and the posterior covariance matrix."
  let pmR = getPosteriorMatrixMergeBranchesToRoot treesRooted
      (mu, sigma) = second L.unSym $ L.meanCov pmR
  putStrLn "The mean branch lengths are:"
  print mu
  putStrLn $
    "Minimum value of absolute values of covariance matrix: "
      ++ show (L.minElement $ L.cmap abs sigma)
  putStrLn $
    "Maximum value of absolute values of covariance matrix: "
      ++ show (L.maxElement $ L.cmap abs sigma)
  let variances = L.takeDiag sigma
  putStrLn "The variances are: "
  print variances
  putStrLn $ "Minimum variance: " ++ show (L.minElement variances)
  putStrLn $ "Maximum variance: " ++ show (L.maxElement variances)
  putStrLn "Prepare the covariance matrix for the likelihood calculation."
  let (sigmaInv, (logSigmaDet, sign)) = L.invlndet sigma
  when (sign /= 1.0) $ error "prepare: Determinant of covariance matrix is negative?"
  putStrLn $ "The logarithm of the determinant of the covariance matrix is: " ++ show logSigmaDet
  putStrLn $ "Save the posterior means and covariances to " <> getDataFn an <> "."
  encodeFile (getDataFn an) (mu, L.toRows sigmaInv, logSigmaDet)

  putStrLn "Prepare the rooted tree with mean branch lengths (used as initial state)."
  -- Use one of the trees of the tree list in case the given rooted tree has a
  -- different sub tree order. This case really happened to me...
  let treeR = head treesRooted
      pm = getPosteriorMatrix treesRooted
      -- Mean Vector including both branches to the root.
      (means, _) = L.meanCov pm
  let toLength' = either (error . (<>) "prepare: ") id . toLength
      meanTreeRooted =
        fromMaybe (error "prepare: Could not label tree with mean branch lengths") $
          setBranches (map toLength' $ VS.toList means) treeR
  putStrLn "The rooted tree with mean branch lengths is:"
  BL.putStrLn $ toNewick $ measurableToPhyloTree meanTreeRooted
  putStrLn $ "Save the rooted tree with mean branch lengths to " <> getMeanTreeFn an <> "."
  BL.writeFile (getMeanTreeFn an) (toNewick $ measurableToPhyloTree meanTreeRooted)

getCalibrations :: Tree e Name -> Maybe FilePath -> IO (VB.Vector Calibration)
getCalibrations _ Nothing = return VB.empty
getCalibrations t (Just f) = loadCalibrations t f

getConstraints :: Tree e Name -> Maybe FilePath -> IO (VB.Vector Constraint)
getConstraints _ Nothing = return VB.empty
getConstraints t (Just f) = loadConstraints t f

-- Run the Metropolis-Hastings-Green algorithm.
runMetropolisHastingsGreen :: Spec -> IO ()
runMetropolisHastingsGreen (Spec an cals cons) = do
  -- Read the mean tree and the posterior means and covariances.
  meanTree <- getMeanTree an
  (mu, sigmaInv, logSigmaDet) <- getData an

  -- Use the mean tree, and the posterior means and covariances to initialize
  -- various objects.
  --
  -- Calibrations.
  cb <- getCalibrations meanTree cals
  -- Constraints.
  cs <- getConstraints meanTree cons
  let -- Prior function.
      pr' = priorFunction cb cs
      -- Likelihood function.
      lh' = likelihoodFunction mu sigmaInv logSigmaDet
      -- Proposal cycle.
      cc' = proposals (isJust cals) meanTree
      -- Monitor.
      mon' = monitor (VB.toList cb) (VB.toList cs)
      -- Starting state.
      start' = initWith meanTree

  -- Create a seed value for the random number generator. Actually, the
  -- 'create' function is deterministic, but useful during development. For
  -- real analyses, use 'createSystemRandom'.
  g <- create

  -- Construct a Metropolis-Hastings-Green Markov chain.
  let mcmcS =
        Settings
          (AnalysisName an)
          burnIn
          iterations
          Overwrite
          Parallel
          Save
          LogStdOutAndFile
          Debug
  -- Either use the MC3 algorithm.
  let mc3S = MC3Settings (NChains 4) (SwapPeriod 2) (NSwaps 3)
  a <- mc3 mc3S pr' lh' cc' mon' TraceAuto start' g

  -- -- Or the standard MHG algorithm.
  -- a <- mhg pr' lh' cc' mon' TraceAuto start' g

  -- Run the Markov chain.
  void $ mcmc mcmcS a

continueMetropolisHastingsGreen :: Spec -> IO ()
continueMetropolisHastingsGreen (Spec an cals cons) = do
  -- Read the mean tree and the posterior means and covariances.
  meanTree <- getMeanTree an
  (mu, sigmaInv, logSigmaDet) <- getData an

  -- Use the mean tree, and the posterior means and covariances to initialize
  -- various objects.
  --
  -- Calibrations.
  cb <- getCalibrations meanTree cals
  -- Constraints.
  cs <- getConstraints meanTree cons
  let -- Prior function.
      pr' = priorFunction cb cs
      -- Likelihood function.
      lh' = likelihoodFunction mu sigmaInv logSigmaDet
      -- Proposal cycle.
      cc' = proposals (isJust cals) meanTree
      -- Monitor.
      mon' = monitor (VB.toList cb) (VB.toList cs)

  -- Load the MCMC settings and the algorithm.
  let an' = AnalysisName an
  s <- settingsLoad an'
  a <- mc3Load pr' lh' cc' mon' an'
  void $ mcmcContinue iterations s a

runMarginalLikelihood :: Spec -> IO ()
runMarginalLikelihood (Spec an cals cons) = do
  -- Read the mean tree and the posterior means and covariances.
  meanTree <- getMeanTree an
  (mu, sigmaInv, logSigmaDet) <- getData an

  -- Use the mean tree, and the posterior means and covariances to initialize
  -- various objects.
  --
  -- Calibrations.
  cb <- getCalibrations meanTree cals
  -- Constraints.
  cs <- getConstraints meanTree cons
  let -- Prior function.
      pr' = priorFunction cb cs
      -- Likelihood function.
      lh' = likelihoodFunction mu sigmaInv logSigmaDet
      -- Proposal cycle.
      cc' = proposals (isJust cals) meanTree
      -- Monitor.
      mon' = monitor (VB.toList cb) (VB.toList cs)
      -- Starting state.
      start' = initWith meanTree

  -- Create a seed value for the random number generator. Actually, the
  -- 'create' function is deterministic, but useful during development. For
  -- real analyses, use 'createSystemRandom'.
  g <- create

  -- Construct a Metropolis-Hastings-Green Markov chain.
  let mlS =
        MLSettings
          (AnalysisName an)
          SteppingStoneSampling
          nPoints
          initialBurnIn
          repetitiveBurnIn
          iterations
          Overwrite
          LogStdOutAndFile
          Debug

  -- Run the Markov chain.
  void $ marginalLikelihood mlS pr' lh' cc' mon' start' g

main :: IO ()
main = do
  cmd <- parseArgs
  case cmd of
    Prepare p -> prepare p
    Run s -> runMetropolisHastingsGreen s
    Continue s -> continueMetropolisHastingsGreen s
    MarginalLikelihood s -> runMarginalLikelihood s
