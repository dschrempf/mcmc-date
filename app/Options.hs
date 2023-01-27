-- |
-- Module      :  Options
-- Description :  Options for mcmc-date
-- Copyright   :  2021 Dominik Schrempf
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
    CalibrationSpec (..),
    PrepSpec (..),
    Mode (..),
    parseArgs,
  )
where

import Data.Version (showVersion)
import Mcmc.Tree
import Options.Applicative
import Options.Applicative.Help.Pretty
import Paths_mcmc_date (version)
import Probability

data Algorithm = MhgA | Mc3A
  deriving (Eq, Read, Show)

data LikelihoodSpec
  = FullMultivariateNormal
  | -- | Sparse covariance matrix with given penalty parameter. The parameter
    --   determines the sparseness of the precision matrix.
    SparseMultivariateNormal Double
  | UnivariateNormal
  | -- | Do not use the phylogenetic likelihood. This is useful when analyzing
    --   the effect of the prior.
    NoLikelihood
  deriving (Eq, Read, Show)

likelihoodSpecP :: Parser LikelihoodSpec
likelihoodSpecP = option auto (long "likelihood-spec" <> help "Likelihood specification (see below).")

data CalibrationSpec = CalibrationsCsv FilePath | CalibrationsTree FilePath
  deriving (Show, Eq, Read)

data Spec = Spec
  { analysisName :: String,
    -- | Multiple analyses with different names can use the same preparation files.
    preparationName :: Maybe String,
    -- | If no calibrations are given, a normalized tree with height 1.0 is
    -- inferred.
    calibrations :: Maybe CalibrationSpec,
    handleProblematicCalibrations :: HandleProblematicCalibrations,
    constraints :: Maybe FilePath,
    handleProblematicConstraints :: HandleProblematicConstraints,
    braces :: Maybe FilePath,
    -- | Reuse state and proposal tuning parameters from a previous run; if
    -- successful also reduce burn in.
    initFromSave :: Maybe FilePath,
    -- | Activate profiling (change the number of iterations).
    profile :: Bool,
    -- | Activate Hamiltonian proposal.
    hamiltonian :: Bool,
    -- | Likelihood specification.
    likelihoodSpec :: LikelihoodSpec,
    -- | Relaxed molecular clock.
    relaxedMolecularClock :: RelaxedMolecularClockModel
  }
  deriving (Eq, Show, Read)

analysisNameP :: Parser String
analysisNameP =
  strOption
    ( long "analysis-name"
        <> help "Analysis name"
        <> metavar "NAME"
    )

preparationNameP :: Parser String
preparationNameP =
  strOption
    ( long "preparation-name"
        <> help "Preparation name"
        <> metavar "NAME"
    )

readCalibrationSpec :: String -> Either String CalibrationSpec
readCalibrationSpec xs = case words xs of
  ["csv", fn] -> Right $ CalibrationsCsv fn
  ["tree", fn] -> Right $ CalibrationsTree fn
  _ -> err
  where
    err = Left $ "readCalibrationSpec: expected \"csv filename\" or \"tree filename\"; got " <> xs

calibrationsP :: Parser CalibrationSpec
calibrationsP =
  option
    (eitherReader readCalibrationSpec)
    ( long "calibrations"
        <> help "Specify calibrations (SPEC is either csv or tree)"
        <> metavar "\"SPEC FILE\" (mind the quotes)"
    )

handleProblematicCalibrationsP :: Parser HandleProblematicCalibrations
handleProblematicCalibrationsP =
  flag ErrorOnProblematicCalibrations WarnAboutProblematicCalibrations $
    long "ignore-problematic-calibrations" <> help "Ignore and use problematic calibrations"

constraintsP :: Parser FilePath
constraintsP =
  strOption
    ( long "constraints"
        <> help "File name specifying constraints"
        <> metavar "FILE"
    )

handleProblematicConstraintsP :: Parser HandleProblematicConstraints
handleProblematicConstraintsP =
  flag ErrorOnProblematicConstraints WarnAboutAndDropProblematicConstraints $
    long "ignore-problematic-constraints" <> help "Ignore and drop problematic constraints"

bracesP :: Parser FilePath
bracesP =
  strOption
    ( long "braces"
        <> help "File name specifying braces"
        <> metavar "FILE"
    )

initFromSaveP :: Parser FilePath
initFromSaveP =
  strOption
    ( long "init-from-save"
        <> help "Reuse state and proposal tuning parameters from a previous run; if successful, also reduce burn in"
        <> metavar "ANALYSIS_NAME"
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

relaxedMolecularClockP :: Parser RelaxedMolecularClockModel
relaxedMolecularClockP =
  option auto $
    long "relaxed-molecular-clock" <> help "Relaxed molecular clock model (see below)."

specP :: Parser Spec
specP =
  Spec
    <$> analysisNameP
    <*> optional preparationNameP
    <*> optional calibrationsP
    <*> handleProblematicCalibrationsP
    <*> optional constraintsP
    <*> handleProblematicConstraintsP
    <*> optional bracesP
    <*> optional initFromSaveP
    <*> profileP
    <*> hamiltonianP
    <*> likelihoodSpecP
    <*> relaxedMolecularClockP

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
    -- | Likelihood specification.
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

prepSpecP :: Parser PrepSpec
prepSpecP = PrepSpec <$> analysisNameP <*> prepInRootedTreeP <*> prepInTreesP <*> likelihoodSpecP

data Mode
  = Prepare PrepSpec
  | Run Spec Algorithm
  | Continue Spec Algorithm
  | MarginalLikelihood Spec
  deriving (Eq, Show, Read)

algorithmP :: Parser Algorithm
algorithmP =
  flag
    MhgA
    Mc3A
    ( long "mc3"
        <> help "Use MC3 instead of MHG algorithm"
    )

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
        (info prepareP (progDesc' "Prepare data"))
        <> command
          "run"
          (info runP (progDesc' "Run MCMC sampler"))
        <> command
          "continue"
          (info continueP (progDesc' "Continue MCMC sampler"))
        <> command
          "marginal-likelihood"
          (info marginalLikelihoodP (progDesc' "Calculate marginal likelihood"))
    )
  where
    f =
      footerDoc $
        Just $
          vsep $
            map
              string
              [ "Relaxed molecular clock model:",
                "  - UncorrelatedGamma",
                "  - AutocorrelatedLogNormal",
                "Likelihood specification:",
                "  - FullMultivariateNormal",
                "  - SparseMultivariateNormal PENALTY (usually 0.1)",
                "  - UnivariateNormal"
              ]
    progDesc' x = progDesc x <> f

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
