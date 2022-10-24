{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Main
-- Description :  Approximate phylogenetic likelihood with multivariate normal distribution
-- Copyright   :  2021 Dominik Schrempf
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
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List hiding (cycle)
import qualified Data.Matrix as MB
import Data.Maybe
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as L
import Options
import System.IO
import System.Random.Stateful
import Prelude hiding (cycle)

-- Disable the syntax formatter Ormolu to highlight relevant module imports.
{- ORMOLU_DISABLE -}
-- The ELynx library includes functions to work on trees.
import qualified ELynx.Topology as T
import ELynx.Tree

-- The Mcmc library includes the Metropolis-Hastings-Green algorithm.
import Mcmc hiding (Continue)
import Mcmc.Tree

-- We need this to load an old state (see 'initFromSave').
import Mcmc.Chain.Chain (cycle, link)
import Mcmc.Chain.Link (state)
import Mcmc.Cycle (ccProposals)

-- Local modules (see comment above).
import Definitions
import Hamiltonian
import Probability
import State
import Tools
import qualified Statistics.Covariance as S
{- ORMOLU_ENABLE -}

getMeanTreeFn :: String -> FilePath
getMeanTreeFn s = s <> ".meantree"

-- The rooted tree with posterior mean branch lengths will be stored in a file
-- with this name.
getMeanTree :: String -> IO (Tree Length Name)
getMeanTree = oneTree Standard . getMeanTreeFn

getDataFn :: String -> FilePath
getDataFn s = s <> ".data"

data LikelihoodDataStore
  = FullS (L.Vector Double) [L.Vector Double] Double
  | SparseS (L.Vector Double) [((Int, Int), Double)] Double
  | UnivariateS (L.Vector Double) (L.Vector Double)
  | NoLikelihoodS

$(deriveJSON defaultOptions ''LikelihoodDataStore)

-- Get the posterior branch length means, the inverted covariance matrix, and
-- the determinant of the covariance matrix.
getData :: String -> IO LikelihoodData
getData s = do
  r <- decodeFileStrict' $ getDataFn s
  case r of
    Nothing -> error $ "getData: Could not decode data file: " <> getDataFn s <> "."
    Just (FullS mu sigmaInvRows logDetSigma) -> do
      -- We can trust that the matrix is symmetric here, because the matrix was
      -- created by 'meanCov'.
      let sigmaInv = L.trustSym $ L.fromRows sigmaInvRows
      pure $ Full mu sigmaInv logDetSigma
    Just (SparseS mu sigmaInvSparseAssocList logDetSigma) -> do
      let sigmaInvS = L.mkSparse sigmaInvSparseAssocList
      pure $ Sparse mu sigmaInvS logDetSigma
    Just (UnivariateS mu vs) -> pure $ Univariate mu vs
    Just NoLikelihoodS -> pure NoData

-- Get the posterior matrix of branch lengths. Merge the two branch lengths
-- leading to the root.
getPosteriorMatrixMergeBranchesToRoot :: [Tree Double a] -> L.Matrix Double
getPosteriorMatrixMergeBranchesToRoot = L.fromRows . map (sumFirstTwo . getBranches)

-- Get the posterior matrix of branch lengths.
getPosteriorMatrix :: [Tree Length a] -> L.Matrix Double
getPosteriorMatrix = L.fromRows . map (VS.fromList . map fromLength . branches)

-- -- Check if value is large enough to be kept in the sparse matrix.
-- keepValueWith :: Double -> Int -> Int -> L.Matrix Double -> Maybe ((Int, Int), Double)
-- keepValueWith r i j m =
--   if (abs x < (vI * r)) && (abs x < (vJ * r))
--     then Nothing
--     else Just ((i, j), x)
--   where
--     x = m `L.atIndex` (i, j)
--     vI = abs $ m `L.atIndex` (i, i)
--     vJ = abs $ m `L.atIndex` (j, j)

-- makeSparseWith ::
--   -- Relative threshold ratio used to erase offdiagonal elements of the covariance
--   -- matrix.
--   Double ->
--   L.Matrix Double ->
--   -- Also return the association list (because otherise I cannot convert it back
--   -- to a dense matrix); and the proportion of elements kept.
--   (L.GMatrix, L.AssocMatrix, Double)
-- makeSparseWith r sigma
--   | n /= m = error "makeSparse: Matrix not square."
--   | otherwise = (L.mkSparse xs', xs', fromIntegral (length xs') / fromIntegral (n * n))
--   where
--     n = L.rows sigma
--     m = L.cols sigma
--     xs' =
--       catMaybes $
--         [ keepValueWith r i j sigma
--           | i <- [0 .. (n - 1)],
--             j <- [0 .. (n - 1)]
--         ]

toAssocMatrix :: L.Matrix Double -> L.AssocMatrix
toAssocMatrix xs
  | n /= m = error "toAssocMatrix: Matrix not square."
  | otherwise =
      catMaybes
        [ if abs e > eps then Just ((i, j), e) else Nothing
          | i <- [0 .. (n - 1)],
            j <- [0 .. (n - 1)],
            let e = xs `L.atIndex` (i, j)
        ]
  where
    n = L.rows xs
    m = L.cols xs
    eps = 1e-8

-- Read in all trees, calculate posterior means and covariances of the branch
-- lengths, and find the midpoint root of the mean tree.
prepare :: Handle -> PrepSpec -> IO ()
prepare h (PrepSpec an rt ts lhsp) = do
  hPutStrLn h "Read trees."
  treesAll <- someTrees Standard ts
  let nTrees = length treesAll
  hPutStrLn h $ show nTrees ++ " trees read."

  let nBurnInTrees = nTrees `div` 6
  hPutStrLn h $ "Skip a burn in of " ++ show nBurnInTrees ++ " trees."
  let trs = drop nBurnInTrees treesAll

  hPutStrLn h "Check if trees have unique leaves."
  if any duplicateLeaves treesAll
    then error "prepare: Trees have duplicate leaves."
    else hPutStrLn h "OK."

  hPutStrLn h "Read rooted tree."
  treeRooted <- onePTree Standard rt

  hPutStrLn h "Root the trees at the same point as the given rooted tree."
  let og = fst $ fromBipartition $ either error id $ bipartition treeRooted
      !treesRooted = force $ map (either error id . outgroup og) trs

  hPutStrLn h "Check if topologies of the trees in the tree list are equal."
  hPutStrLn h "Topology AND sub tree orders need to match."
  let differentTrees = nub $ map T.fromBranchLabelTree treesRooted
  if length differentTrees == 1
    then hPutStrLn h "OK."
    else do
      hPutStrLn h "Trees have different topologies or sub tree orders:"
      BL.hPutStrLn h $ BL.unlines $ map toNewickTopology differentTrees
      error "prepare: A single topology and equal sub tree orders are required."

  hPutStrLn h "Check the topology of the rooted tree."
  hPutStrLn h "The topology has to match the one of the trees in the tree list."
  hPutStrLn h "The sub tree orders may differ."
  let topoRooted = T.fromBranchLabelTree treeRooted
      topoHead = T.fromBranchLabelTree $ head treesRooted
  if T.equal' topoRooted topoHead
    then hPutStrLn h "OK."
    else do
      hPutStrLn h "Trees have different topologies."
      BL.hPutStrLn h $ toNewickTopology topoRooted
      BL.hPutStrLn h $ toNewickTopology topoHead
      error "prepare: A single topology is required."

  hPutStrLn h ""
  hPutStrLn h "Get the posterior means and the posterior covariance matrix."
  let pmR = getPosteriorMatrixMergeBranchesToRoot $ map (first fromLength) treesRooted
      (mu, sigma) = second L.unSym $ L.meanCov pmR
  hPutStrLn h $ "Number of branches: " <> show (L.size mu) <> "."
  hPutStrLn h "The mean branch lengths are:"
  hPrint h mu
  hPutStrLn h $ "Minimum mean branch length: " <> show (VS.minimum mu)
  hPutStrLn h $ "Maximum mean branch length: " <> show (VS.maximum mu)
  hPutStrLn h $ "Minimum absolute covariance: " ++ show (L.minElement $ L.cmap abs sigma)
  hPutStrLn h $ "Maximum absolute covariance: " ++ show (L.maxElement $ L.cmap abs sigma)
  let variances = L.takeDiag sigma
  hPutStrLn h "The variances are: "
  hPrint h variances
  let minVariance = L.minElement variances
  when (minVariance <= 0) $ error "prepare: Minimum variance is zero or negative."
  hPutStrLn h $ "Minimum variance: " ++ show minVariance
  hPutStrLn h $ "Maximum variance: " ++ show (L.maxElement variances)
  hPutStrLn h "Comparison with complete covariance matrix:"
  let f x = if abs x >= minVariance then (1.0 :: Double) else 0.0
      nSmaller = L.sumElements $ L.cmap f sigma
  hPutStrLn h $ "Number of elements of complete covariance matrix that are smaller than the minimum variance: " <> show nSmaller <> "."

  hPutStrLn h ""
  hPutStrLn h "Prepare the covariance matrix for phylogenetic likelihood calculation."
  let (sigmaInv, (logDetSigma, sign)) = L.invlndet sigma
  when (sign /= 1.0) $ error "prepare: Determinant of covariance matrix is negative?"
  let (n, m) = L.size sigmaInv
  hPutStrLn h $ "Average element size of inverse covariance matrix: " <> show (L.sumElements sigmaInv / fromIntegral (n * m)) <> "."
  hPutStrLn h $ "The logarithm of the determinant of the covariance matrix is: " ++ show logDetSigma

  hPutStrLn h ""
  lhd <- case lhsp of
    FullMultivariateNormal -> do
      hPutStrLn h "Use full covariance matrix."
      pure $ FullS mu (L.toRows sigmaInv) logDetSigma
    -- SparseMultivariateNormal rho -> do
    --   hPutStrLn "Use a sparse covariance matrix to speed up phylogenetic likelihood calculation."
    --   hPutStrLn "Table of \"Relative threshold, proportion of entries kept\"."
    --   let rs = [1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-8, 1e-16]
    --   hPutStrLn $
    --     intercalate "\n" $
    --       [ show r ++ ", " ++ show p
    --         | r <- rs,
    --           let (_, _, p) = makeSparseWith r sigmaInv
    --       ]
    --   hPutStrLn $ "Use a (provided) relative threshold of: " <> show rho <> "."
    --   let (_, sigmaInvSL, _) = makeSparseWith rho sigmaInv
    --       sigmaS = L.inv (L.toDense sigmaInvSL)
    --       (_, (logDetSigmaS, signS)) = L.invlndet sigmaS
    --   when (signS /= 1.0) $ error "prepare: Determinant of sparse covariance matrix is negative?"
    --   pure $ SparseS mu sigmaInvSL logDetSigmaS
    SparseMultivariateNormal rho -> do
      hPutStrLn h "Use a sparse covariance/precision matrix to speed up phylogenetic likelihood calculation."
      hPutStrLn h "Estimate amtrices with glasso (graphical lasso)."
      hPutStrLn h $ "Use a (provided) penalty parameter of: " <> show rho <> "."
      let (muS, ssS, xsNormalizedS) = S.scale pmR
          -- The precision matrix is the inverted correlation matrix sigma.
          (sigmaNormalizedSparse, precNormalizedSparse) = either error id $ S.graphicalLasso rho xsNormalizedS
          sigmaSparse = S.rescaleSWith ssS $ L.unSym sigmaNormalizedSparse
          precSparse = S.rescalePWith ssS $ L.unSym precNormalizedSparse
      let -- Get log of determinant of sparse sigma.
          (_, (logDetSigmaS, signS)) = L.invlndet sigmaSparse
      when (signS /= 1.0) $ error "prepare: Determinant of sparse covariance matrix is negative?"
      let -- Need an association list.
          precSparseList = toAssocMatrix precSparse
      let -- Some debug output.
          nFull = L.rows sigmaSparse * L.rows sigmaSparse
          nSparse = length precSparseList
      hPutStrLn h $ "Number of elements of full matrix: " <> show nFull
      hPutStrLn h $ "Number of elements of sparse matrix: " <> show nSparse
      hPutStrLn h $ "Proportion of elements kept: " <> show (fromIntegral nSparse / fromIntegral nFull :: Double)
      pure $ SparseS muS precSparseList logDetSigmaS
    UnivariateNormal -> do
      hPutStrLn h "Use univariate normal distributions to speed up phylogenetic likelihood calculation."
      let vs = L.takeDiag sigma
      pure $ UnivariateS mu vs
    NoLikelihood -> do
      hPutStrLn h "Do not use the phylogenetic likelihood. Only estimate the prior."
      pure NoLikelihoodS
  hPutStrLn h $ "Save the posterior means and (co)variances to " <> getDataFn an <> "."
  encodeFile (getDataFn an) lhd

  hPutStrLn h "Prepare the rooted tree with mean branch lengths (used as initial state)."
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
  hPutStrLn h "The rooted tree with mean branch lengths is:"
  BL.hPutStrLn h $ toNewick $ lengthToPhyloTree meanTreeRooted
  hPutStrLn h $ "Save the rooted tree with mean branch lengths to " <> getMeanTreeFn an <> "."
  BL.writeFile (getMeanTreeFn an) (toNewick $ lengthToPhyloTree meanTreeRooted)

getCalibrations ::
  Handle ->
  HandleProblematicCalibrations ->
  Tree e Name ->
  Maybe FilePath ->
  IO (VB.Vector (Calibration Double))
getCalibrations _ _ _ Nothing = return VB.empty
getCalibrations h frc t (Just f) = loadCalibrations h frc t f

getConstraints ::
  Handle ->
  HandleProblematicConstraints ->
  Tree e Name ->
  Maybe FilePath ->
  IO (VB.Vector (Constraint Double))
getConstraints _ _ _ Nothing = return VB.empty
getConstraints h frc t (Just f) = loadConstraints h frc t f

getBraces :: Handle -> Tree e Name -> Maybe FilePath -> IO (VB.Vector (Brace Double))
getBraces _ _ Nothing = return VB.empty
getBraces h t (Just f) = loadBraces h t f

getLikelihoodFunction :: Handle -> String -> LikelihoodSpec -> IO (LikelihoodFunction I)
getLikelihoodFunction h an lhsp = do
  lhd <- getData an
  -- Assert that likelihood specification on command line, and stored likelihood
  -- data are in agreement.
  case (lhsp, lhd) of
    (FullMultivariateNormal, Full {}) -> hPutStrLn h "Using full multivariate normal distribution."
    (SparseMultivariateNormal _, Sparse {}) -> hPutStrLn h "Using sparse multivariate normal distribution."
    (UnivariateNormal, Univariate _ _) -> hPutStrLn h "Using univariate normal distributions."
    (NoLikelihood, NoData) -> hPutStrLn h "No likelihood; use prior only."
    (l, r) -> do
      hPutStrLn h $ "Likelihood specification: " <> show l
      hPutStrLn h $ "Likelihood data: " <> show r
      error "Likelihood specification and data do not match (see above)."
  pure $ likelihoodFunction lhd

getHTarget ::
  String ->
  LikelihoodSpec ->
  -- Approximate absolute time tree height.
  Double ->
  RelaxedMolecularClockModel ->
  VB.Vector (Calibration Double) ->
  VB.Vector (Constraint Double) ->
  VB.Vector (Brace Double) ->
  IO (HTarget IG)
getHTarget an lhsp ht md cb cs bs = do
  lhd <- getData an
  case (lhsp, lhd) of
    (FullMultivariateNormal, Full mu s d) -> do
      let muBoxed = VB.convert mu
          sigmaInvBoxed = MB.fromRows $ map VB.convert $ L.toRows $ L.unSym s
      pure $ htargetWith ht md cb cs bs muBoxed sigmaInvBoxed d
    (_, _) -> do
      let msg = "Generalized likelihood function not implemented for sparse matrices and the univariate."
      throwIO $ PatternMatchFail msg

getMcmcProps ::
  Handle ->
  Spec ->
  Maybe Algorithm ->
  IO
    ( I,
      PriorFunctionG (IG Double) Double,
      LikelihoodFunction I,
      Cycle I,
      Monitor I,
      Settings
    )
getMcmcProps h (Spec an mPrepName cls clsFlag cns cnsFlag brs ifs prof ham lhsp rmcm) malg = do
  let prepName = fromMaybe an mPrepName
  -- Read the mean tree and the posterior means and covariances.
  meanTree <- getMeanTree prepName

  -- Use the mean tree, and the posterior means and covariances to initialize
  -- various objects.
  --
  -- Calibrations.
  cb <- getCalibrations h clsFlag meanTree cls
  let ht = fromMaybe 1.0 $ getMeanRootHeight cb
  -- Constraints.
  cs <- getConstraints h cnsFlag meanTree cns
  -- Braces.
  bs <- getBraces h meanTree brs
  -- Likelihood function.
  lh' <- getLikelihoodFunction h prepName lhsp
  -- Generalized posterior function for Hamiltonian proposal.
  mHTarget <-
    if ham
      then Just <$> getHTarget prepName lhsp ht rmcm cb cs bs
      else pure Nothing

  let -- Naive starting state and proposal cycle.
      startNaive = initWith meanTree
      ccNaive = proposals (VB.toList bs) (isJust cls) startNaive mHTarget

  let -- Prior function.
      pr' = priorFunction ht rmcm cb cs bs
      -- Monitor.
      mon' = monitor ht rmcm cb cs bs

  -- Starting state.
  let eWith m = error $ "getMcmcProps: " <> m <> " Try without '--init-from-save'."
  (start', cc', burnIn') <-
    case ifs of
      Nothing -> pure (startNaive, ccNaive, burnIn)
      Just anSave -> case malg of
        Just Mc3A -> eWith "Loading initial state not implemented for MC3 algorithm."
        Nothing -> eWith "Loading initial state not possible."
        Just MhgA -> do
          hPutStrLn h "Loading old state; if this fails, try without '--init-from-save'."
          mhgA <- mhgLoadUnsafe pr' lh' ccNaive mon' (AnalysisName anSave)
          let startInformed = state $ link $ fromMHG mhgA
              ccInformed = cycle $ fromMHG mhgA
          hPutStrLn h "Success."
          hPutStrLn h "Initializing chain with last state from save."
          if length (ccProposals ccInformed) == length (ccProposals ccNaive)
            then do
              hPutStrLn h "Using tuning parameters from save."
              pure (startInformed, ccInformed, burnInInformed)
            else do
              hPutStrLn h "Cycle has changed, start with untuned proposals."
              pure (startInformed, ccNaive, burnIn)

  -- Construct a Metropolis-Hastings-Green Markov chain.
  let burnIn'' = if prof then burnInProf else burnIn'
      iterations' = if prof then iterationsProf else iterations
      mcmcS =
        Settings
          (AnalysisName an)
          burnIn''
          iterations'
          TraceAuto
          Overwrite
          Parallel
          Save
          LogStdOutAndFile
          Debug

  return (start', pr', lh', cc', mon', mcmcS)

-- Run the Metropolis-Hastings-Green algorithm.
runMetropolisHastingsGreen :: Handle -> Spec -> Algorithm -> IO ()
runMetropolisHastingsGreen h spec alg = do
  (i, p, l, c, m, s) <- getMcmcProps h spec (Just alg)

  -- Create a seed value for the random number generator. Actually, the
  -- 'create' function is deterministic, but useful during development. For
  -- real analyses, use 'createSystemRandom'.
  let g = mkStdGen 0

  case alg of
    MhgA -> do
      a <- mhg s p l c m i g
      void $ mcmc s a
    Mc3A -> do
      let mc3S = MC3Settings (NChains 4) (SwapPeriod 2) (NSwaps 3)
      a <- mc3 mc3S s p l c m i g
      void $ mcmc s a

-- -- Test automatic differentiation.
--
-- chain' <- fromMHG <$> mcmc mcmcS a
-- trace' <- VB.map state <$> takeT 100 (trace chain')
-- let x = VB.head trace'
--     gAd = _rateMean $ gradLogPosteriorFunc cb cs bs muBoxed sigmaInvBoxed logDetSigma x
--     startX = x & rateMean -~ 0.002
--     startY = x & rateMean +~ 0.002
--     dNm = numDiffLogPosteriorFunc cb cs muBoxed sigmaInvBoxed logDetSigma startX startY 0.004
-- hPutStrLn $ "GRAD: " <> show gAd
-- hPutStrLn $ "NUM: " <> show dNm
-- error "Debug."

continueMetropolisHastingsGreen :: Handle -> Spec -> Algorithm -> IO ()
continueMetropolisHastingsGreen h spec alg = do
  (_, p, l, c, m, _) <- getMcmcProps h spec Nothing
  let an = AnalysisName $ analysisName spec
  s <- settingsLoad an
  let is = if profile spec then iterationsProf else iterations
  case alg of
    MhgA -> do
      a <- mhgLoad p l c m an
      void $ mcmcContinue is s a
    Mc3A -> do
      a <- mc3Load p l c m an
      void $ mcmcContinue is s a

runMarginalLikelihood :: Handle -> Spec -> IO ()
runMarginalLikelihood h spec = do
  (i, p, l, c, m, _) <- getMcmcProps h spec Nothing
  -- Create a seed value for the random number generator. Actually, the
  -- 'create' function is deterministic, but useful during development. For
  -- real analyses, use 'createSystemRandom'.
  let g = mkStdGen 0

  -- Construct a Metropolis-Hastings-Green Markov chain.
  let prof = profile spec
      nPoints' = if prof then nPointsProf else nPoints
      burnIn' = if prof then burnInProf else burnIn
      repetitiveBurnIn' = if prof then repetitiveBurnInProf else repetitiveBurnIn
      iterations' = if prof then iterationsProf else iterations
      mlS =
        MLSettings
          (AnalysisName $ analysisName spec)
          SteppingStoneSampling
          nPoints'
          burnIn'
          repetitiveBurnIn'
          iterations'
          Overwrite
          LogStdOutAndFile
          Debug

  -- Run the Markov chain.
  void $ marginalLikelihood mlS p l c m i g

main :: IO ()
main = do
  cmd <- parseArgs
  case cmd of
    Prepare p -> do
      let fn = prepAnalysisName p <> ".prepare.log"
      withFile fn WriteMode $ \h -> prepare h p
    Run s a -> do
      let fn = analysisName s <> ".run.log"
      withFile fn WriteMode $ \h ->
        hSetBuffering h LineBuffering
          >> runMetropolisHastingsGreen h s a
    Continue s a -> do
      let fn = analysisName s <> ".continue.log"
      withFile fn WriteMode $ \h ->
        hSetBuffering h LineBuffering
          >> continueMetropolisHastingsGreen h s a
    MarginalLikelihood s -> do
      let fn = analysisName s <> ".mlh.log"
      withFile fn WriteMode $ \h ->
        hSetBuffering h LineBuffering
          >> runMarginalLikelihood h s
