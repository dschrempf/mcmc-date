-- |
-- Module      :  Mcmc.Tree.Prior.Node.CalibrationFromTree
-- Description :  Load node age calibrations from trees
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Dec  1 13:42:32 2022.
module Mcmc.Tree.Prior.Node.CalibrationFromTree
  ( loadCalibrationsFromTree,
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as VB
import ELynx.Tree
import Mcmc.Tree.Prior.Node.Calibration
import System.IO

-- See McmcTree manual page 49.
data LBounded = LBounded
  { lLower :: Double,
    lCauchyP :: Maybe Double, -- ignored
    lCuachyC :: Maybe Double, -- ignored
    lPrLower :: Maybe Double
  }
  deriving (Show)

pLBounded :: Parser LBounded
pLBounded = do
  _ <- char 'L'
  _ <- char '('
  l <- double
  cauchyC <- optional (char ',' *> double)
  cauchyP <- optional (char ',' *> double)
  p <- optional (char ',' *> double)
  _ <- char ')'
  pure $ LBounded l cauchyC cauchyP p

-- See McmcTree manual page 49.
data UBounded = UBounded
  { uLower :: Double,
    uPrLower :: Maybe Double
  }
  deriving (Show)

pUBounded :: Parser UBounded
pUBounded = do
  _ <- char 'U'
  _ <- char '('
  l <- double
  p <- optional (char ',' *> double)
  _ <- char ')'
  pure $ UBounded l p

-- See McmcTree manual page 49.
data BBounded = BBounded
  { bLower :: Double,
    bUpper :: Double,
    bPrLower :: Maybe Double,
    bPrUpper :: Maybe Double
  }
  deriving (Show)

pBBounded :: Parser BBounded
pBBounded = do
  _ <- char 'B'
  _ <- char '('
  l <- double
  _ <- char ','
  u <- double
  pL <- optional (char ',' *> double)
  pU <- optional (char ',' *> double)
  _ <- char ')'
  pure $ BBounded l u pL pU

data ABounded = Abl LBounded | Abu UBounded | Abb BBounded
  deriving (Show)

pABounded :: Parser ABounded
pABounded = (Abl <$> pLBounded) <|> (Abu <$> pUBounded) <|> (Abb <$> pBBounded)

type CalibrationS = ((Name, Name), ABounded)

aBoundedToCalibrationData :: CalibrationS -> CalibrationData Double
aBoundedToCalibrationData ((a, b), bnd) = CalibrationData (aS <> "-" <> bS) aS bS l lp u up
  where
    aS = BL.unpack $ fromName a
    bS = BL.unpack $ fromName b
    (l, lp, u, up) = case bnd of
      (Abl (LBounded l' _ _ lp')) -> (Just l', lp', Nothing, Nothing)
      (Abu (UBounded u' up')) -> (Nothing, Nothing, Just u', up')
      (Abb (BBounded l' u' lp' up')) -> (Just l', Just u', lp', up')

filterBoundedNodes :: Tree e Name -> [CalibrationS]
filterBoundedNodes t@(Node _ nm ts) = case parseOnly pABounded nmS of
  Left _ -> concatMap filterBoundedNodes ts
  Right x -> ((getL t, getR t), x) : concatMap filterBoundedNodes ts
  where
    nmS = BS.toStrict $ fromName nm
    getL (Node _ x []) = x
    getL (Node _ _ xs) = getL $ head xs
    getR (Node _ x []) = x
    getR (Node _ _ xs) = getR $ last xs

loadCalibrationsFromTree ::
  Handle ->
  HandleProblematicCalibrations ->
  Tree e Name ->
  FilePath ->
  IO (VB.Vector (Calibration Double))
loadCalibrationsFromTree h frc tree fn = do
  t <- readOneNewick Standard fn
  let ns = filterBoundedNodes t
      ds = VB.fromList $ map aBoundedToCalibrationData ns
  when (VB.null ds) $ error $ "loadCalibrationsFromTree: no calibrations found in file: " <> fn
  checkAndConvertCalibrationData h frc tree ds
