{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Mcmc.Tree.Prior.Node.Calibration
-- Description :  Relative node order constraints and node calibrations
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Jul 27 10:49:11 2020.
module Mcmc.Tree.Prior.Node.Calibration
  ( -- * Calibrations
    Interval,
    Calibration,
    getCalibrationName,
    getCalibrationPath,
    getCalibrationNodeIndex,
    getCalibrationInterval,
    CalibrationData (..),
    HandleProblematicCalibrations (..),
    loadCalibrations,
    getMeanRootHeight,
    calibrateSoftS,
    calibrateSoftF,
    calibrateSoft,
    transformCalibration,

    -- * Misc
    realToFracCalibration,
  )
where

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Csv hiding (Name)
import Data.Function
import Data.List
import qualified Data.Vector as VB
import ELynx.Tree hiding (partition)
import GHC.Generics
import Mcmc.Prior hiding (positive)
import Mcmc.Tree.Lens
import Mcmc.Tree.Mrca
import Mcmc.Tree.Prior.Node.Internal
import Mcmc.Tree.Types
import System.IO

-- Non-negative lower boundary with probability mass.
data LowerBoundary a = Zero | PositiveLowerBoundary a (ProbabilityMass a)
  deriving (Eq)

instance Show a => Show (LowerBoundary a) where
  show (PositiveLowerBoundary x p) = show x ++ "[" ++ show p ++ "]"
  show Zero = ""

positiveLowerBoundary :: (Ord a, Num a) => a -> ProbabilityMass a -> Either String (LowerBoundary a)
positiveLowerBoundary x p
  | x <= 0 = Left "positiveLowerBoundary: Zero or negative value."
  | otherwise = Right $ PositiveLowerBoundary x p

-- | Finite positive upper boundary or infinity.
data UpperBoundary a = PositiveUpperBoundary a (ProbabilityMass a) | Infinity
  deriving (Eq)

instance Show a => Show (UpperBoundary a) where
  show (PositiveUpperBoundary x p) = show x ++ "[" ++ show p ++ "]"
  show Infinity = "Infinity"

positiveUpperBoundary :: (Ord a, Num a) => a -> ProbabilityMass a -> Either String (UpperBoundary a)
positiveUpperBoundary x p
  | x <= 0 = Left "positiveUpperBoundary: Zero or negative value."
  | otherwise = Right $ PositiveUpperBoundary x p

-- | Interval \([a,b]\) or \([a,\infty]\) with \(a < b\), \(a \in [0, \infty)\)
-- and \(b \in (0, \infty]\). If a is nonzero, a probability mass is provided
-- for the left boundary. If b is finite, a probability mass is provided for the
-- right boundary.
data Interval a = Interval (LowerBoundary a) (UpperBoundary a)
  deriving (Eq)

instance Show a => Show (Interval a) where
  show (Interval a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

-- | Transform an interval by applying a multiplicative change.
--
-- Useful when the tree is normalized and height values have to be converted
-- from relative heights to absolute heights.
transformInterval :: RealFloat a => a -> Interval a -> Interval a
transformInterval x (Interval l r)
  | x <= 0 = error "transformInterval: Multiplier is zero or negative."
  | otherwise = Interval l' r'
  where
    l' = case l of
      Zero -> Zero
      PositiveLowerBoundary a pa -> PositiveLowerBoundary (x * a) pa
    r' = case r of
      PositiveUpperBoundary b pb -> PositiveUpperBoundary (x * b) pb
      Infinity -> Infinity
{-# SPECIALIZE transformInterval :: Double -> Interval Double -> Interval Double #-}

-- | Calibrate node heights.
--
-- That is, ensure that a node height is within a given interval.
--
-- A calibration is specified by a name, a node at given path, height
-- boundaries, and an 'Interval'.
--
-- Calibrations are abstract data types and can be created using
-- 'loadCalibrations'. The reason is that finding the nodes on the tree is a
-- slow process not to be repeated at each proposal.
data Calibration a = Calibration
  { calibrationName :: String,
    calibrationNodePath :: Path,
    calibrationNodeIndex :: Int,
    calibrationInterval :: Interval a
  }
  deriving (Eq)

-- | Get name.
getCalibrationName :: Calibration a -> String
getCalibrationName = calibrationName

-- | Get path.
getCalibrationPath :: Calibration a -> Path
getCalibrationPath = calibrationNodePath

-- | Get node index.
getCalibrationNodeIndex :: Calibration a -> Int
getCalibrationNodeIndex = calibrationNodeIndex

-- | Get interval.
getCalibrationInterval :: Calibration a -> Interval a
getCalibrationInterval = calibrationInterval

prettyPrintCalibration :: Show a => Calibration a -> String
prettyPrintCalibration (Calibration n p i l) =
  "Calibration: "
    <> n
    <> " with path "
    <> show p
    <> ", index "
    <> show i
    <> ", and interval "
    <> show l
    <> "."

-- Create a calibration.
--
-- See also 'loadCalibrations' which validates calibrations against each other.
--
-- Call 'error' if the node cannot be found on the tree.
calibration ::
  (Ord a, Show a, Num b, Ord b) =>
  Tree e a ->
  -- | Name.
  String ->
  -- | The most recent common ancestor of the given leaves is the calibrated node.
  [a] ->
  Interval b ->
  Calibration b
calibration t n xs l = Calibration n p i l
  where
    err msg = error $ "calibration: " ++ n ++ ": " ++ msg
    p = either err id $ mrca xs t
    -- NOTE: Identifying the tree multiple times may be slow when creating many
    -- calibrations. But this is only done once in the beginning.
    i = label $ getSubTreeUnsafe p $ identify t
{-# SPECIALIZE calibration ::
  (Ord a, Show a) =>
  Tree e a ->
  String ->
  [a] ->
  Interval Double ->
  Calibration Double
  #-}

-- | Data structure used to encode and decode the calibration CSV file.
--
-- Useful to save calibrations.
data CalibrationData a
  = CalibrationData
      String
      -- ^ Name.
      String
      -- ^ Leaf a.
      String
      -- ^ Leaf b.
      (Maybe a)
      -- ^ Lower boundary.
      (Maybe a)
      -- ^ Lower boundary probability mass.
      (Maybe a)
      -- ^ Upper boundary.
      (Maybe a)
      -- ^ Upperboundary probability mass.
  deriving (Generic, Show)

instance FromField a => FromRecord (CalibrationData a)

instance ToField a => ToRecord (CalibrationData a)

calibrationDataToCalibration :: (Ord a, Num a) => Tree e Name -> CalibrationData a -> Calibration a
calibrationDataToCalibration t (CalibrationData n la lb ma mpa mb mpb) = calibration t n [la', lb'] i
  where
    la' = Name $ BL.pack la
    lb' = Name $ BL.pack lb
    i = either err id $ case (ma, mpa, mb, mpb) of
      -- Lower bound only.
      (Just a, Just pa, Nothing, Nothing) -> do
        pa' <- probabilityMass pa
        a' <- positiveLowerBoundary a pa'
        pure $ Interval a' Infinity
      -- Upper bound only.
      (Nothing, Nothing, Just b, Just pb) -> do
        pb' <- probabilityMass pb
        b' <- positiveUpperBoundary b pb'
        pure $ Interval Zero b'
      -- Lower and upper bounds.
      (Just a, Just pa, Just b, Just pb) ->
        if a >= b
          then Left "Lower boundary larger equal upper boundary."
          else do
            pa' <- probabilityMass pa
            pb' <- probabilityMass pb
            a' <- positiveLowerBoundary a pa'
            b' <- positiveUpperBoundary b pb'
            pure $ Interval a' b'
      -- Errors.
      (Nothing, Just _, _, _) -> Left "Lower probability mass given but no lower boundary."
      (_, _, Nothing, Just _) -> Left "Upper probability mass given but no upper boundary."
      (Just _, Nothing, _, _) -> Left "Lower boundary given but no lower probability mass."
      (_, _, Just _, Nothing) -> Left "Upper boundary given but no upper probability mass."
      (Nothing, Nothing, Nothing, Nothing) -> Left "No boundaries provided."
    err m = error $ "calibrationDataToCalibration: " <> n <> ": " <> m

-- Get duplicate pairs of a list.
findDupsBy :: (a -> a -> Bool) -> [a] -> [[a]]
findDupsBy _ [] = []
findDupsBy eq (x : xs) = case partition (eq x) xs of
  ([], _) -> findDupsBy eq xs
  (ys, xs') -> (x : ys) : findDupsBy eq xs'

-- | Warn or error when problematic calibrations are found?
--
-- For now, duplicates, conflicts, and redundancies qualify as problematic.
data HandleProblematicCalibrations
  = WarnAboutProblematicCalibrations
  | ErrorOnProblematicCalibrations
  deriving (Eq, Read, Show)

-- | Load and validate calibrations from file.
--
-- The calibration file is a comma separated values (CSV) file with rows of the
-- following format:
--
-- > CalibrationName,LeafA,LeafB,LowerBoundary,LowerBoundaryProbabilityMass,UpperBoundary,UpperBoundaryProbabilityMass
--
-- The calibrated node is uniquely defined as the most recent common ancestor
-- (MRCA) of @LeafA@ and @LeafB@. A boundary comes with a corresponding
-- 'ProbabilityMass'. Either boundary and their corresponding probability mass
-- can be omitted.
--
-- The following line defines a calibration with a lower boundary at 1e6 with
-- probability mass 0.025 only:
--
-- > Primates,Human,Chimpanzees,1e6,0.025,,
--
-- Call 'error' if:
--
-- - The file contains syntax errors.
--
-- - An MRCA is not found.
loadCalibrations ::
  -- | Log file handle.
  Handle ->
  HandleProblematicCalibrations ->
  Tree e Name ->
  FilePath ->
  IO (VB.Vector (Calibration Double))
loadCalibrations h frc t f = do
  d <- BL.readFile f
  let mr = decode HasHeader d :: Either String (VB.Vector (CalibrationData Double))
      cds = either error id mr
  when (VB.null cds) $ error $ "loadCalibrations: No calibrations found in file: " <> f <> "."
  let calsAll = VB.map (calibrationDataToCalibration t) cds
  -- Check for duplicates and conflicts.
  let calsErrs = findDupsBy ((==) `on` calibrationNodePath) $ VB.toList calsAll
  -- TODO: Actually check for conflicting calibrations.
  if null calsErrs
    then hPutStrLn h "No duplicate/conflicting/redundant calibrations have been detected."
    else do
      -- Calibrations could also be removed. But then, which one should be removed?
      let render xs =
            unlines $
              "Redundant and/or conflicting calibration:" : map prettyPrintCalibration xs
      mapM_ (hPutStr h . render) calsErrs
      case frc of
        WarnAboutProblematicCalibrations ->
          hPutStr h "WARNING: Duplicate/conflicting/redundant calibrations have been detected."
        ErrorOnProblematicCalibrations ->
          error "loadCalibrations: Duplicate/conflicting/redundant calibrations have been detected."
  return calsAll

-- | If the root node is calibrated, get the mean of the root node height.
--
-- The mean root node height is useful when calibrating the rate mean prior.
getMeanRootHeight :: RealFloat a => VB.Vector (Calibration a) -> Maybe a
getMeanRootHeight cs =
  VB.filter isRootCalibration cs
    & assertZeroOrOneResults
    <&> calibrationInterval
    >>= getMean
  where
    isRootCalibration :: Calibration a -> Bool
    isRootCalibration c = calibrationNodePath c == []
    assertZeroOrOneResults :: VB.Vector (Calibration a) -> Maybe (Calibration a)
    assertZeroOrOneResults xs = if VB.length xs == 1 then Just $ VB.unsafeHead xs else Nothing
    getMean :: RealFloat a => Interval a -> Maybe a
    getMean (Interval a' (PositiveUpperBoundary b _)) = case a' of
      Zero -> Just $ b / 2.0
      (PositiveLowerBoundary a _) -> Just $ (a + b) / 2.0
    getMean _ = Nothing

-- | Calibrate height of a single node.
--
-- When the height of the node is within the given bounds, a uniform
-- distribution is used.
--
-- When the height of the node is out of bounds, a one-sided normal distribution
-- with given standard deviation is used. The normal distribution is normalized
-- such that the complete distribution of the constraint is continuous. Use of
-- the normal distribution also ensures that the first derivative is continuous.
--
-- If the upper bound is not given, no upper bound is used.
--
-- For reasons of computational efficiency, the path is not checked for
-- correctness.
--
-- Call 'error' if the path is invalid.
calibrateSoftS ::
  RealFloat a =>
  Calibration a ->
  PriorFunctionG (HeightTree a) a
calibrateSoftS c (HeightTree t) = calibrateSoftF l h
  where
    p = calibrationNodePath c
    h = t ^. subTreeAtL p . branchL
    l = calibrationInterval c
{-# SPECIALIZE calibrateSoftS :: Calibration Double -> PriorFunction (HeightTree Double) #-}

-- | See 'calibrateSoftS'.
calibrateSoftF :: RealFloat a => Interval a -> PriorFunctionG a a
calibrateSoftF (Interval a' b') h
  | h < 0 = error "calibrateSoftF: Height is negative."
  | otherwise = lowerCheck * upperCheck
  where
    lowerCheck = case a' of
      Zero -> 1
      PositiveLowerBoundary a pa ->
        if h < a
          then let d = getD pa in d (a - h) / d 0
          else 1
    upperCheck = case b' of
      Infinity -> 1
      PositiveUpperBoundary b pb ->
        if h > b
          then let d = getD pb in d (h - b) / d 0
          else 1
    -- NOTE: One could store the normal distribution at the boundary directly in
    -- the 'LowerBoundary', and 'UpperBoundary'; but then I do not think this is
    -- a big issue.
    --
    -- FYI: sqrt (2/pi) = 0.7978845608028654.
    getD p = let s = 0.7978845608028654 * (getProbabilityMass p) in normal 0 s
{-# SPECIALIZE calibrateSoftF :: Interval Double -> PriorFunction Double #-}

-- | Calibrate nodes of a tree using 'calibrateSoftS'.
--
-- Calculate the calibration prior for a given vector of calibrations, the
-- absolute height of the tree, and the tree with relative heights.
--
-- Call 'error' if:
--
-- - A path is invalid.
--
-- - The height multiplier is zero or negative.
calibrateSoft ::
  RealFloat a =>
  -- | Height multiplier of tree. Useful when working on normalized trees.
  a ->
  VB.Vector (Calibration a) ->
  PriorFunctionG (HeightTree a) a
calibrateSoft h cs t
  | h <= 0 = 0.0
  | otherwise = VB.product $ VB.map f cs
  where
    f c = calibrateSoftS (transformCalibration h c) t
{-# SPECIALIZE calibrateSoft ::
  Double ->
  VB.Vector (Calibration Double) ->
  PriorFunction (HeightTree Double)
  #-}

-- | Transform a calibration using a height multiplier.
--
-- See 'calibrateSoft'.
--
-- Call 'error' if the height multiplier is zero or negative.
transformCalibration :: RealFloat a => a -> Calibration a -> Calibration a
transformCalibration h c
  | h <= 0 = error "transformCalibration: Height multiplier is zero or negative."
  | h == 1 = c
  | otherwise = c {calibrationInterval = transformInterval (recip h) $ calibrationInterval c}

rf :: (Real a, Fractional b) => a -> b
rf = realToFrac

rf' :: (Real a, Fractional b) => ProbabilityMass a -> ProbabilityMass b
rf' = realToFracProbabilityMass

realToFracI :: Fractional a => Interval Double -> Interval a
realToFracI (Interval (PositiveLowerBoundary a pa) (PositiveUpperBoundary b pb)) =
  Interval (PositiveLowerBoundary (rf a) (rf' pa)) (PositiveUpperBoundary (rf b) (rf' pb))
realToFracI (Interval (PositiveLowerBoundary a pa) Infinity) =
  Interval (PositiveLowerBoundary (rf a) (rf' pa)) Infinity
realToFracI (Interval Zero (PositiveUpperBoundary b pb)) =
  Interval Zero (PositiveUpperBoundary (rf b) (rf' pb))
realToFracI (Interval Zero Infinity) =
  Interval Zero Infinity

-- | Convert a calibration on 'Double' to a more general one.
--
-- Useful for automatic differentiation.
realToFracCalibration :: Fractional a => Calibration Double -> Calibration a
realToFracCalibration c = c {calibrationInterval = i'}
  where
    i' = realToFracI $ calibrationInterval c
{-# SPECIALIZE realToFracCalibration :: Calibration Double -> Calibration Double #-}
