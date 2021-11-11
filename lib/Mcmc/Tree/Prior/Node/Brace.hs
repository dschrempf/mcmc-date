{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Mcmc.Tree.Prior.Node.Brace
-- Description :  Brace nodes
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov 10 14:40:55 2021.
module Mcmc.Tree.Prior.Node.Brace
  ( -- * Braces
    Brace (..),
    brace,
    loadBraces,
    braceHardS,
    braceSoftS,
    braceSoftF,
  )
where

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Csv hiding (Name)
import Data.List
import Data.Maybe
import qualified Data.Vector as VB
import ELynx.Tree hiding (partition)
import GHC.Generics
import Mcmc
import Mcmc.Tree.Lens
import Mcmc.Tree.Mrca
import Mcmc.Tree.Types

-- | Brace.
data Brace = Brace
  { braceName :: String,
    braceNodeAPath :: Path,
    braceNodeAIndex :: Int,
    braceNodeBPath :: Path,
    braceNodeBIndex :: Int
  }
  deriving (Eq, Read, Show)

-- | Create a brace.
--
-- Call 'error' if:
--
-- - A node cannot be found on the tree.
--
-- - The nodes are equal.
brace ::
  (Ord a, Show a) =>
  Tree e a ->
  -- | Name.
  String ->
  -- | The most recent common ancestor of the given leaves will be braced.
  [a] ->
  -- | The most recent common ancestor of the given leaves will be braced.
  [a] ->
  Brace
brace t n xs ys
  | iX == iY = err "The nodes are equal."
  | otherwise = Brace n pX iX pY iY
  where
    err msg = error $ "brace: " ++ n ++ ": " ++ msg
    iTr = identify t
    pX = either err id $ mrca xs t
    iX = label $ getSubTreeUnsafe pX iTr
    pY = either err id $ mrca ys t
    iY = label $ getSubTreeUnsafe pY iTr

data BraceData
  = BraceData
      String -- Brace name.
      String -- Node X, leaf A.
      String -- Node X, leaf B.
      String -- Node Y, leaf A.
      String -- Node Y, leaf B.
  deriving (Generic, Show)

instance FromRecord BraceData

braceDataToBrace :: Tree e Name -> BraceData -> Brace
braceDataToBrace t (BraceData n nXA nXB nYA nYB) = brace t n [pn nXA, pn nXB] [pn nYA, pn nYB]
  where
    pn = Name . BL.pack

-- Check if two braces conflict or are duplicates.
checkBraces :: Brace -> Brace -> [String]
checkBraces x y@(Brace nY apY aiY bpY biY) = checkBraces' x y ++ checkBraces' x y'
  where
    y' = Brace nY bpY biY apY aiY

-- Assume node order is fixed.
checkBraces' :: Brace -> Brace -> [String]
checkBraces' (Brace nX apX aiX bpX biX) (Brace nY apY aiY bpY _) =
  catMaybes
    [ indicesMismatch apX apY aiX aiY,
      indicesMismatch bpX apY biX aiY,
      pathsMismatch apX apY aiX aiY,
      pathsMismatch bpX apY biX aiY,
      equalNodePaths
    ]
  where
    msg m = "Braces " ++ nX ++ " and " ++ nY ++ ":" ++ m
    -- True if paths match but indices do not match (weird error).
    indicesMismatch pX pY iX iY
      | (pX == pY) && (iX /= iY) = Just $ msg "Node paths match but node indices do not."
      | otherwise = Nothing
    pathsMismatch pX pY iX iY
      | (pX /= pY) && (iX == iY) = Just $ msg "Node indices match but node paths do not."
      | otherwise = Nothing
    equalNodePaths =
      if (apX == apY) && (bpX == bpY)
        then Just $ msg "Paths are equal."
        else Nothing

-- | Load and validate braces from file.
--
-- The brace file is a comma separated values (CSV) file with rows of the
-- following format:
--
-- @
-- BraceName,NodeXLeafA,NodeXLeafB,NodeYLeafA,NodeYLeafB
-- @
--
-- The braced nodes are uniquely defined as the most recent common ancestors
-- (MRCA) of @NodeXLeafA@ and @NodeXLeafB@, as well as @NodeYLeafA@ and
-- @NodeYLeafB@.
--
-- Call 'error' if:
--
-- - The file contains syntax errors.
--
-- - An MRCA cannot be found.
--
-- - Node X and Y of a brace are equal.
--
-- - There are duplicate braces.
loadBraces :: Tree e Name -> FilePath -> IO (VB.Vector Brace)
loadBraces t f = do
  d <- BL.readFile f
  let mr = decode NoHeader d :: Either String (VB.Vector BraceData)
      bs = either error id mr
  when (VB.null bs) $ error $ "loadBraces: No braces found in file: " <> f <> "."
  let bsAll = VB.map (braceDataToBrace t) bs
  -- Check for duplicates and conflicts.
  let bsErrs = concat [checkBraces x y | (x : ys) <- tails (VB.toList bsAll), y <- ys]
  if null bsErrs
    then putStrLn "No duplicates and no conflicting braces have been detected."
    else do
      mapM_ putStr bsErrs
      error "loadBraces: Duplicates and/or conflicting braces have been detected."
  return bsAll

braceHardS :: RealFloat a => Brace -> PriorFunctionG (HeightTree a) a
braceHardS (Brace _ x _ y _) (HeightTree t)
  | hX == hY = 1.0
  | otherwise = 0.0
  where
    hX = t ^. subTreeAtL x . branchL
    hY = t ^. subTreeAtL y . branchL
{-# SPECIALIZE braceHardS :: Brace -> PriorFunctionG (HeightTree Double) Double #-}

braceSoftS ::
  RealFloat a =>
  StandardDeviation a ->
  Brace ->
  PriorFunctionG (HeightTree a) a
braceSoftS s (Brace _ x _ y _) (HeightTree t)
  | hX == hY = 1.0
  | otherwise = braceSoftF s (hX, hY)
  where
    hX = t ^. subTreeAtL x . branchL
    hY = t ^. subTreeAtL y . branchL
{-# SPECIALIZE braceSoftS :: Double -> Brace -> PriorFunctionG (HeightTree Double) Double #-}

-- | See 'braceSoftS'.
braceSoftF ::
  RealFloat a =>
  StandardDeviation a ->
  PriorFunctionG (a, a) a
braceSoftF s' (hX, hY)
  | hX == hY = 1.0
  | otherwise = d (hX - hY) - d 0
  where
    s = realToFrac s'
    d = normal 0 s
{-# SPECIALIZE braceSoftF :: Double -> PriorFunction (Double, Double) #-}
