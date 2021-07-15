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
  ( Spec (..),
    PrepSpec (..),
    Mode (..),
    parseArgs,
  )
where

import Data.Version (showVersion)
import Options.Applicative
import Paths_mcmc_dating (version)

data Spec = Spec
  { analysisName :: String,
    -- | If no calibrations are given, a normalized tree with height 1.0 is
    -- inferred.
    calibrations :: Maybe FilePath,
    constraints :: Maybe FilePath
  }
  deriving (Eq, Show, Read)

analysisNameP :: Parser String
analysisNameP =
  strOption
    ( short 'a'
        <> long "analysis-name"
        <> help "Analysis name"
        <> metavar "NAME"
    )

calibrationsP :: Parser FilePath
calibrationsP =
  strOption
    ( short 'c'
        <> long "calibrations"
        <> help "File name specifying calibrations"
        <> metavar "FILE"
    )

constraintsP :: Parser FilePath
constraintsP =
  strOption
    ( short 'k'
        <> long "constraints"
        <> help "File name specifying constraints"
        <> metavar "FILE"
    )

specP :: Parser Spec
specP = Spec <$> analysisNameP <*> optional calibrationsP <*> optional constraintsP

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
    prepInTrees :: FilePath
  }
  deriving (Eq, Read, Show)

prepInRootedTreeP :: Parser FilePath
prepInRootedTreeP =
  strOption
    ( short 't'
        <> long "rooted-tree"
        <> help "Rooted tree to be dated (Newick format)"
        <> metavar "FILE"
    )

prepInTreesP :: Parser FilePath
prepInTreesP =
  strOption
    ( short 'd'
        <> long "in-trees"
        <> help "Posterior distribution of trees (one tree per line, Newick format)"
        <> metavar "FILE"
    )

prepSpecP :: Parser PrepSpec
prepSpecP = PrepSpec <$> analysisNameP <*> prepInRootedTreeP <*> prepInTreesP

data Mode
  = Prepare PrepSpec
  | Run Spec
  | Continue Spec
  | MarginalLikelihood Spec
  deriving (Eq, Show, Read)

prepareP :: Parser Mode
prepareP = Prepare <$> prepSpecP

runP :: Parser Mode
runP = Run <$> specP

continueP :: Parser Mode
continueP = Continue <$> specP

marginalLikelihoodP :: Parser Mode
marginalLikelihoodP = MarginalLikelihood <$> specP

modeP :: Parser Mode
modeP =
  hsubparser
    ( command
        "prepare"
        (info prepareP (progDesc "Prepare data"))
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
