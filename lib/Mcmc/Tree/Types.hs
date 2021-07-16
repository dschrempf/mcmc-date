{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Oct 27 19:14:20 2020.
--
-- Type synonyms to improve code readability.

-- |
-- Module      :  Mcmc.Tree.Types
-- Description :  Different tree types
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
module Mcmc.Tree.Types
  ( -- ** Stem
    HandleStem (..),

    -- ** Nodes
    Path,
    HandleNode,
    allNodes,
    withoutRootNode,

    -- ** Ultrametric trees
    LengthTree (..),
    HeightTree (..),
    toHeightTreeUltrametric,
    heightTreeToLengthTree,
  )
where

import Data.Aeson
import Data.Bifunctor
import ELynx.Tree
import GHC.Generics

-- | Should the stem be handled, when traversing branches of a tree?
data HandleStem = WithStem | WithoutStem

-- | Which nodes should be handled, when traversing a tree?
--
-- Useful when creating proposals on trees.
--
-- For an example, see 'withoutRootNode'.
type HandleNode = Path -> Bool

-- | Handle all nodes.
--
-- In particular:
--
-- - Include the stem, if handling branches.
--
-- - Include the root label, if handling node labels.
allNodes :: HandleNode
allNodes = const True

-- | Exclude the root label.
--
-- @
-- withoutRoot = (>0)
-- @
withoutRootNode :: HandleNode
withoutRootNode = not . null

-- | Tree with branch lengths.
newtype LengthTree a = LengthTree {fromLengthTree :: Tree a Name}
  deriving (Generic)

instance Functor LengthTree where
  fmap f = LengthTree . first f . fromLengthTree

instance Foldable LengthTree where
  foldMap f (LengthTree t) = foldMap f (ZipBranchTree t)

instance Traversable LengthTree where
  traverse f (LengthTree t) = LengthTree . getZipBranchTree <$> traverse f (ZipBranchTree t)

-- TODO: For the applicative instance, we need to ignore the names.
-- instance Applicative LengthTree where

instance ToJSON a => ToJSON (LengthTree a)

instance FromJSON a => FromJSON (LengthTree a)

-- | Tree with node heights.
newtype HeightTree a = HeightTree {fromHeightTree :: Tree a Name}
  deriving (Generic)

instance Functor HeightTree where
  fmap f = HeightTree . first f . fromHeightTree

instance Foldable HeightTree where
  foldMap f (HeightTree t) = foldMap f (ZipBranchTree t)

instance Traversable HeightTree where
  traverse f (HeightTree t) = HeightTree . getZipBranchTree <$> traverse f (ZipBranchTree t)

-- TODO: For the applicative instance, we need to ignore the names.
-- instance Applicative HeightTree where

instance ToJSON a => ToJSON (HeightTree a)

instance FromJSON a => FromJSON (HeightTree a)

-- | Calculate node heights for a given tree.
--
-- The __stem length__ is __removed__.
--
-- This function is expensive and has not been optimized yet. The run time is
-- @O(n^2)@ where @n@ is the number of inner nodes.
--
-- Return 'Left' if:
--
-- - The tree is not ultrametric. The height of leaves is set to zero. If the
--   tree is not ultrametric, the node heights are not defined and the height
--   tree has to be instantiated manually.
toHeightTreeUltrametric ::
  HasLength a =>
  Tree a Name ->
  HeightTree Double
-- A leaf.
toHeightTreeUltrametric t
  | ultrametric t = toHeightTreeUltrametric' t
  | otherwise = error "toHeightTreeUltrametric: Tree is not ultrametric."

-- Assume the tree is ultrametric.
toHeightTreeUltrametric' :: HasLength a => Tree a Name -> HeightTree Double
toHeightTreeUltrametric' t@(Node _ lb ts) = HeightTree $
  Node (assertNonNegative "toHeightTreeUltrametric'" $ (realToFrac . rootHeight) t) lb $
    map (fromHeightTree . toHeightTreeUltrametric') ts

-- | Calculate branch lengths and remove node heights.
heightTreeToLengthTree :: (Ord a, Num a, Show a) => HeightTree a -> LengthTree a
heightTreeToLengthTree t' = LengthTree $ go (branch t) t
  where
    t = fromHeightTree t'
    go hParent (Node hNode lb ts) =
      Node (assertNonNegative "heightTreeToLengthTree" (hParent - hNode)) lb $ map (go hNode) ts
{-# SPECIALIZE heightTreeToLengthTree :: HeightTree Double -> LengthTree Double #-}

assertNonNegative :: (Ord a, Num a, Show a) => String -> a -> a
assertNonNegative n val
  | val < 0 = error $ n <> ": Negative value: " <> show val <> "."
  | otherwise = val
