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
    braceSoft,
  )
where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
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
--
-- Braces can be created using 'brace' or 'loadBraces'.
data Brace = Brace
  { braceName :: String,
    braceNodes :: [NodeInfo]
  }
  deriving (Eq, Read, Show)

-- | Create a brace.
--
-- Call 'error' if:
--
-- - A node cannot be found on the tree.
--
-- - Nodes are equal.
brace ::
  (Ord a, Show a) =>
  Tree e a ->
  -- | Name.
  String ->
  -- | The most recent common ancestors of the given leaf lists will be braced.
  [[a]] ->
  Brace
brace t n xss
  | null xss = err "Empty node list."
  | length (nub is) /= length is = err "Some nodes have equal indices."
  | length (nub ps) /= length ps = err "some nodes have equal paths."
  | otherwise = Brace n $ sort (zipWith NodeInfo is ps)
  where
    err msg = error $ "brace: " ++ n ++ ": " ++ msg
    iTr = identify t
    ps = map (\xs -> either err id $ mrca xs t) xss
    is = map (\p -> label $ getSubTreeUnsafe p iTr) ps

data BraceData = BraceData
  { braceDataName :: String,
    -- List of leaf pairs defining nodes.
    braceDataNodes :: [(String, String)]
  }
  deriving (Generic, Show)

instance ToJSON BraceData

instance FromJSON BraceData

braceDataToBrace :: Tree e Name -> BraceData -> Brace
braceDataToBrace t (BraceData n lvss) = brace t n [[pn lvA, pn lvB] | (lvA, lvB) <- lvss]
  where
    pn = Name . BL.pack

-- Check if two braces conflict or are duplicates.
checkBraces :: Brace -> Brace -> [String]
checkBraces (Brace nX nsX) (Brace nY nsY) =
  catMaybes $
    equalNodeIndices :
    [indicesMismatch x y | x <- nsX, y <- nsY]
      ++ [pathsMismatch x y | x <- nsX, y <- nsY]
  where
    msg m = "Braces " ++ nX ++ " and " ++ nY ++ ":" ++ m
    indicesMismatch (NodeInfo iX pX) (NodeInfo iY pY)
      | (pX == pY) && (iX /= iY) = Just $ msg "Node paths match but node indices do not."
      | otherwise = Nothing
    pathsMismatch (NodeInfo iX pX) (NodeInfo iY pY)
      | (pX /= pY) && (iX == iY) = Just $ msg "Node indices match but node paths do not."
      | otherwise = Nothing
    equalNodeIndices =
      if map nodeIndex nsX == map nodeIndex nsY
        then Just $ msg "Braced nodes are equal."
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
  let mr = eitherDecode d :: Either String (VB.Vector BraceData)
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

-- Test if all elements of a list are equal; returns True for empty list.
allEqual :: Eq a => [a] -> Bool
-- Well, maybe it should be False, but then, it is True that all elements are
-- equal :).
allEqual [] = True
allEqual xs = all (== head xs) (tail xs)

-- | Brace a single list of nodes.
--
-- If the node heights are equal, the prior is 1.0. Otherwise, the prior is 0.0.
--
-- Call 'error' if a path is invalid.
braceHardS :: RealFloat a => Brace -> PriorFunctionG (HeightTree a) a
braceHardS (Brace _ xs) (HeightTree t)
  | allEqual hs = 1.0
  | otherwise = 0.0
  where
    hs = map (\ni -> t ^. subTreeAtL (nodePath ni) . branchL) xs
{-# SPECIALIZE braceHardS :: Brace -> PriorFunctionG (HeightTree Double) Double #-}

-- | Brace a single list of nodes.
--
-- Use a normal distribution with given standard deviation.
--
-- Call 'error' if a path is invalid.
braceSoftS ::
  RealFloat a =>
  StandardDeviation a ->
  Brace ->
  PriorFunctionG (HeightTree a) a
braceSoftS s (Brace _ xs) (HeightTree t)
  | s <= 0.0 = error "braceSoftS: Standard deviation is zero or negative."
  | otherwise = braceSoftF s hs
  where
    hs = map (\ni -> t ^. subTreeAtL (nodePath ni) . branchL) xs
{-# SPECIALIZE braceSoftS :: Double -> Brace -> PriorFunctionG (HeightTree Double) Double #-}

-- | See 'braceSoftS'.
braceSoftF ::
  RealFloat a =>
  StandardDeviation a ->
  PriorFunctionG [a] a
braceSoftF s' hs
  | allEqual hs = 1.0
  | otherwise = product $ map f hs
  where
    s = realToFrac s'
    d = normal 0 s
    d0 = d 0
    hMean = sum hs / fromIntegral (length hs)
    f x = d (x - hMean) / d0
{-# SPECIALIZE braceSoftF :: Double -> PriorFunction [Double] #-}

-- | Brace pairs of nodes using 'braceSoftS'.
--
-- Call 'error' if a path is invalid.
braceSoft ::
  RealFloat a =>
  -- | NOTE: The same standard deviation is used for all braces.
  StandardDeviation a ->
  VB.Vector Brace ->
  PriorFunctionG (HeightTree a) a
braceSoft s bs t = VB.product $ VB.map (\b -> braceSoftS s b t) bs

{-# SPECIALIZE braceSoftS :: Double -> Brace -> PriorFunctionG (HeightTree Double) Double #-}
