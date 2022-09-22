{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Mcmc.Tree.Prior.Node.Brace
-- Description :  Brace nodes
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov 10 14:40:55 2021.
module Mcmc.Tree.Prior.Node.Brace
  ( -- * Braces
    Brace,
    getBraceName,
    getBraceNodes,
    getBraceStandardDeviation,
    loadBraces,
    braceSoftS,
    braceSoftF,
    braceSoft,

    -- * Misc
    realToFracBrace,
  )
where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as VB
import ELynx.Tree hiding (partition)
import GHC.Generics
import Mcmc
import Mcmc.Tree.Lens
import Mcmc.Tree.Prior.Node.Internal
import Mcmc.Tree.Types
import System.IO

-- | Brace.
--
-- The standard deviation is a positive floating number that specifies the
-- steepness of the decline of the prior function when the braced nodes have
-- different heights. A normal distribution is used.
--
-- Abstract data type to ensure brace validity. Braces can be loaded using
-- 'loadBraces'.
data Brace a = Brace
  { braceName :: String,
    braceNodes :: [NodeInfo],
    braceStandardDeviation :: StandardDeviation a
  }
  deriving (Eq, Read, Show)

-- | Name.
getBraceName :: Brace a -> String
getBraceName = braceName

-- | Node infos.
getBraceNodes :: Brace a -> [NodeInfo]
getBraceNodes = braceNodes

-- | Standard deviation.
getBraceStandardDeviation :: Brace a -> StandardDeviation a
getBraceStandardDeviation = braceStandardDeviation

-- Create a brace.
--
-- Call 'error' if:
--
-- - A node cannot be found on the tree.
--
-- - Nodes are equal.
--
-- - The leaf list is empty or a singleton.
brace ::
  (Ord a, Show a, Num b, Ord b) =>
  Tree e a ->
  -- | Name.
  String ->
  -- | The most recent common ancestors of the given leaf lists will be braced.
  [[a]] ->
  StandardDeviation b ->
  Brace b
brace t n xss s
  | null xss = err "No node found."
  | length xss == 1 = err "Only one node found."
  | any null ps = err "Cannot brace root node."
  | length (nub is) /= length is = err "Some nodes have equal indices."
  | length (nub ps) /= length ps = err "Some nodes have equal paths."
  | not $ null psErrs = error $ unlines $ psErrs ++ ["brace: Errors detected (see above)."]
  | s <= 0 = err "Standard deviation is zero or negative."
  | otherwise = Brace n ns s
  where
    msg m = "brace: " ++ n ++ ": " ++ m
    err m = error $ msg m
    iTr = identify t
    ps = map (\xs -> either err id $ getPathToMrca (S.fromList xs) t) xss
    is = map (\p -> label $ getSubTreeUnsafe p iTr) ps
    -- We also check for equal paths above, but well.
    getErr Equal = Just $ msg "Bogus brace; two nodes are equal (?)."
    getErr LeftIsAncestorOfRight = Just $ msg "Bogus brace; two nodes are direct ancestors (?)."
    getErr LeftIsDescendantOfRight = Just $ msg "Bogus brace; two nodes are direct ancestors (?)."
    getErr Unrelated = Nothing
    psErrs = catMaybes [getErr $ areDirectDescendants x y | (x : ys) <- tails ps, y <- ys]
    ns = sort (zipWith NodeInfo is ps)
{-# SPECIALIZE brace :: (Ord a, Show a) => Tree e a -> String -> [[a]] -> Double -> Brace Double #-}

data BraceData a = BraceData
  { braceDataName :: String,
    -- List of leaf pairs defining nodes.
    braceDataNodes :: [(String, String)],
    braceDataStandardDeviation :: StandardDeviation a
  }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (BraceData a)

instance FromJSON a => FromJSON (BraceData a)

braceDataToBrace :: (Num a, Ord a) => Tree e Name -> BraceData a -> Brace a
braceDataToBrace t (BraceData n lvss w) = brace t n [[pn lvA, pn lvB] | (lvA, lvB) <- lvss] w
  where
    pn = Name . BL.pack

-- Check if two braces conflict or are duplicates.
checkBraces :: Brace a -> Brace a -> [String]
checkBraces (Brace nX nsX _) (Brace nY nsY _) =
  catMaybes $
    equalNodeIndices
      : [indicesMismatch x y | x <- nsX, y <- nsY]
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
-- The brace file is a JSON file in the following format:
--
-- > [{"braceDataName":"Brace1","braceDataNodes":[["NodeXLeafA","NodeXLeafB"],["NodeYLeafA","NodeYLeafB"],["NodeZLeafA","NodeZLeafB"]],"braceDataStandardDeviation":1e-4},
-- >  {"braceDataName":"Brace2","braceDataNodes":[["NodeXLeafA","NodeXLeafB"],["NodeYLeafA","NodeYLeafB"],["NodeZLeafA","NodeZLeafB"]],"braceDataStandardDeviation":1e-4}]
--
-- The braced nodes are uniquely defined as the most recent common ancestors
-- (MRCA) of @NodeXLeafA@ and @NodeXLeafB@, as well as @NodeYLeafA@ and
-- @NodeYLeafB@. The steepness of the brace prior function is defined using the
-- standard deviation, see 'Brace'.
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
loadBraces ::
  -- | Log file handle.
  Handle ->
  Tree e Name ->
  FilePath ->
  IO (VB.Vector (Brace Double))
loadBraces h t f = do
  d <- BL.readFile f
  let mr = eitherDecode d :: Either String (VB.Vector (BraceData Double))
      bs = either error id mr
  when (VB.null bs) $ error $ "loadBraces: No braces found in file: " <> f <> "."
  let bsAll = VB.map (braceDataToBrace t) bs
  -- Check for duplicates and conflicts. Only check each pair once.
  let bsErrs = concat [checkBraces x y | (x : ys) <- tails (VB.toList bsAll), y <- ys]
  if null bsErrs
    then hPutStrLn h "No duplicates and no conflicting braces have been detected."
    else do
      mapM_ (hPutStr h) bsErrs
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
-- 'loadBraces' ensures that the node list is not empty nor a singleton.
--
-- Use a normal distribution with given standard deviation.
--
-- Call 'error' if a path is invalid.
braceSoftS ::
  RealFloat a =>
  Brace a ->
  PriorFunctionG (HeightTree a) a
braceSoftS (Brace _ xs s) (HeightTree t) = braceSoftF s hs
  where
    hs = map (\ni -> t ^. subTreeAtL (nodePath ni) . branchL) xs
{-# SPECIALIZE braceSoftS :: Brace Double -> PriorFunctionG (HeightTree Double) Double #-}

-- | See 'braceSoftS'.
braceSoftF ::
  RealFloat a =>
  StandardDeviation a ->
  PriorFunctionG [a] a
braceSoftF s hs
  | s <= 0 = error "braceSoftF: Standard deviation is zero or negative."
  | allEqual hs = 1
  | otherwise = product $ map f hs
  where
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
  VB.Vector (Brace a) ->
  PriorFunctionG (HeightTree a) a
braceSoft bs t = VB.product $ VB.map (`braceSoftS` t) bs

{-# SPECIALIZE braceSoftS :: Brace Double -> PriorFunctionG (HeightTree Double) Double #-}

-- | Convert a brace on 'Double' to a more general one.
--
-- Useful for automatic differentiation.
realToFracBrace :: Fractional a => Brace Double -> Brace a
realToFracBrace c = c {braceStandardDeviation = s'}
  where
    s' = realToFrac $ braceStandardDeviation c
{-# SPECIALIZE realToFracBrace :: Brace Double -> Brace Double #-}
