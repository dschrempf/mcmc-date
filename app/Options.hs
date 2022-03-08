-- |
-- Module      :  Options
-- Description :  Options for mcmc-date
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Mar  1 18:06:05 2021.
module Options
  ( Algorithm (..),
    Spec (..),
    LikelihoodSpec (..),
    PrepSpec (..),
    Mode (..),
    parseArgs,
  )
where

import Data.Version (showVersion)
import Options.Applicative
import Options.Applicative.Help.Pretty
import Paths_mcmc_date (version)

data Algorithm = MhgA | Mc3A
  deriving (Eq, Read, Show)

data Spec = Spec
  { analysisName :: String,
    -- | If no calibrations are given, a normalized tree with height 1.0 is
    -- inferred.
    calibrations :: Maybe FilePath,
    constraints :: Maybe FilePath,
    braces :: Maybe FilePath,
    -- | Activate profiling (change the number of iterations).
    profile :: Bool,
    -- | Activate Hamiltonian proposal.
    hamiltonian :: Bool
  }
  deriving (Eq, Show, Read)

analysisNameP :: Parser String
analysisNameP =
  strOption
    ( long "analysis-name"
        <> help "Analysis name"
        <> metavar "NAME"
    )

calibrationsP :: Parser FilePath
calibrationsP =
  strOption
    ( long "calibrations"
        <> help "File name specifying calibrations"
        <> metavar "FILE"
    )

constraintsP :: Parser FilePath
constraintsP =
  strOption
    ( long "constraints"
        <> help "File name specifying constraints"
        <> metavar "FILE"
    )

bracesP :: Parser FilePath
bracesP =
  strOption
    ( long "braces"
        <> help "File name specifying braces"
        <> metavar "FILE"
    )

algorithmP :: Parser Algorithm
algorithmP =
  flag
    MhgA
    Mc3A
    ( long "mc3"
        <> help "Use MC3 instead of MHG algorithm"
    )

profileP :: Parser Bool
profileP =
  switch
    ( long "profile"
        <> help "Activate profiling"
    )

hamiltonianP :: Parser Bool
hamiltonianP =
  switch
    ( long "hamiltonian"
        <> help "Activate Hamiltonian proposal"
    )

specP :: Parser Spec
specP =
  Spec
    <$> analysisNameP
    <*> optional calibrationsP
    <*> optional constraintsP
    <*> optional bracesP
    <*> profileP
    <*> hamiltonianP

data LikelihoodSpec
  = FullMultivariateNormal
  | -- | Sparse covariance matrix with given threshold.
    SparseMultivariateNormal Double
  | UnivariateNormal
  deriving (Eq, Read, Show)

data PrepSpec = PrepSpec
  { prepAnalysisName :: String,
    -- | File storing the rooted topology used for the time tree. The in
    -- trees read from 'in_trees' will be rooted at the same position and
    -- are expected to have the same topology.
    prepInRootedTree :: FilePath,
    -- | File storing (rooted or unrooted) trees obtained from a Bayesian
    -- phylogenetic analysis. The posterior means and covariances of the
    -- branch lengths are obtained from these trees and used to approximate
    -- the phylogenetic likelihood.
    prepInTrees :: FilePath,
    -- | Prepare a sparse matrix.
    prepLikelihoodSpec :: LikelihoodSpec
  }
  deriving (Eq, Read, Show)

prepInRootedTreeP :: Parser FilePath
prepInRootedTreeP =
  strOption
    ( long "rooted-tree"
        <> help "Rooted tree to be dated (Newick format)"
        <> metavar "FILE"
    )

prepInTreesP :: Parser FilePath
prepInTreesP =
  strOption
    ( long "trees"
        <> help "Posterior distribution of trees (one tree per line, Newick format)"
        <> metavar "FILE"
    )

likelihoodSpecP :: Parser LikelihoodSpec
likelihoodSpecP = option auto (long "likelihood-spec" <> help "Likelihood specification (see below).")

prepSpecP :: Parser PrepSpec
prepSpecP = PrepSpec <$> analysisNameP <*> prepInRootedTreeP <*> prepInTreesP <*> likelihoodSpecP

data Mode
  = Prepare PrepSpec
  | Run Spec Algorithm
  | Continue Spec Algorithm
  | MarginalLikelihood Spec
  deriving (Eq, Show, Read)

prepareP :: Parser Mode
prepareP = Prepare <$> prepSpecP

runP :: Parser Mode
runP = Run <$> specP <*> algorithmP

continueP :: Parser Mode
continueP = Continue <$> specP <*> algorithmP

marginalLikelihoodP :: Parser Mode
marginalLikelihoodP = MarginalLikelihood <$> specP

modeP :: Parser Mode
modeP =
  hsubparser
    ( command
        "prepare"
        (info prepareP (progDesc "Prepare data" <> footerDoc f))
        <> command
          "run"
          (info runP (progDesc "Run MCMC sampler"))
        <> command
          "continue"
          (info continueP (progDesc "Continue MCMC sampler"))
        <> command
          "marginal-likelihood"
          (info marginalLikelihoodP (progDesc "Calculate marginal likelihood"))
    )
  where
    f =
      Just $
        vsep $
          map
            string
            [ "Likelihood specification:",
              "  - FullMultivariateNormal",
              "  - SparseMultivariateNormal THRESHOLD",
              "  - UnivariateNormal"
            ]

parseArgs :: IO Mode
parseArgs =
  execParser $
    info
      (modeP <**> helper)
      ( fullDesc
          <> progDesc "Date a phylogenetic tree using calibrations and constraints"
          <> header h
      )
  where
    h = "mcmc-date; version " ++ showVersion version
