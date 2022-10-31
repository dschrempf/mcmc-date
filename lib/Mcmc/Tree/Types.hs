{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
module Mcmc.Tree.Types
  ( -- ** Stem
    HandleStem (..),

    -- ** Nodes
    Path,
    NodeInfo (..),
    HandleNode,
    allNodes,
    withoutRootNode,

    -- ** Ultrametric trees
    LengthTree (..),
    isValidLengthTree,
    HeightTree (..),
    isValidHeightTree,
    toHeightTreeUltrametric,
    heightTreeToLengthTree,

    -- ** Rate class tree
    RateClassTree (..),
    rateClassTreeFromTree,
  )
where

import Control.Applicative
import Data.Aeson
import Data.Bifunctor
import ELynx.Tree
import GHC.Generics

-- | Should the stem be handled, when traversing branches of a tree?
data HandleStem = WithStem | WithoutStem

-- | Index and path of a node on a tree.
data NodeInfo = NodeInfo
  { nodeIndex :: Int,
    nodePath :: Path
  }
  deriving (Eq, Ord, Read, Show)

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
newtype LengthTree a = LengthTree {getLengthTree :: Tree a Name}
  deriving (Eq, Generic)

instance Functor LengthTree where
  fmap f = LengthTree . first f . getLengthTree

instance Foldable LengthTree where
  foldMap f (LengthTree t) = foldMap f (ZipBranchTree t)

instance Traversable LengthTree where
  traverse f (LengthTree t) = LengthTree . getZipBranchTree <$> traverse f (ZipBranchTree t)

-- | Zip-like applicative instance.
--
-- - The right node labels are ignored. Strictly speaking, this violates the
-- - laws of functors with application. In our case, this is not important,
-- - because the names are the same across all trees anyways.
--
-- - The empty label is used for 'pure'.
--
-- We could use the applicative instance of 'ZipBranchTree'. However, the
-- 'Monoid' instance of 'Name' is not really what we want. We don't want to
-- concatenate the names, but just ignore one or the other.
instance Applicative LengthTree where
  pure br = LengthTree $ Node br "" $ repeat (getLengthTree $ pure br)
  (LengthTree ~(Node brF lbF tsF)) <*> (LengthTree ~(Node brX _ tsX)) =
    LengthTree $ Node (brF brX) lbF (zipWith f tsF tsX)
    where
      f x y = getLengthTree $ LengthTree x <*> LengthTree y
  liftA2 f (LengthTree ~(Node brX lbX tsX)) (LengthTree ~(Node brY _ tsY)) =
    LengthTree $ Node (f brX brY) lbX (zipWith g tsX tsY)
    where
      g x y = getLengthTree $ liftA2 f (LengthTree x) (LengthTree y)
  (LengthTree ~(Node _ lbX tsX)) *> (LengthTree ~(Node brY _ tsY)) =
    LengthTree $ Node brY lbX (zipWith f tsX tsY)
    where
      f x y = getLengthTree $ LengthTree x *> LengthTree y
  (LengthTree ~(Node brX lbX tsX)) <* (LengthTree ~(Node _ _ tsY)) =
    LengthTree $ Node brX lbX (zipWith f tsX tsY)
    where
      f x y = getLengthTree $ LengthTree x <* LengthTree y

instance ToJSON a => ToJSON (LengthTree a)

instance FromJSON a => FromJSON (LengthTree a)

-- | Check if 'LengthTree' is valid.
--
-- Stem length has to be greater equal zero.
--
-- All other lengths have to be greater than zero.
isValidLengthTree :: LengthTree Double -> Bool
isValidLengthTree (LengthTree (Node br _ ts)) = br >= 0 && all (all (> 0) . ZipBranchTree) ts

-- | Tree with node heights.
newtype HeightTree a = HeightTree {getHeightTree :: Tree a Name}
  deriving (Eq, Generic)

instance Functor HeightTree where
  fmap f = HeightTree . first f . getHeightTree

instance Foldable HeightTree where
  foldMap f (HeightTree t) = foldMap f (ZipBranchTree t)

instance Traversable HeightTree where
  traverse f (HeightTree t) = HeightTree . getZipBranchTree <$> traverse f (ZipBranchTree t)

-- | See the 'Applicative' instance of 'LengthTree'.
instance Applicative HeightTree where
  pure br = HeightTree $ Node br "" $ repeat (getHeightTree $ pure br)
  (HeightTree ~(Node brF lbF tsF)) <*> (HeightTree ~(Node brX _ tsX)) =
    HeightTree $ Node (brF brX) lbF (zipWith f tsF tsX)
    where
      f x y = getHeightTree $ HeightTree x <*> HeightTree y
  liftA2 f (HeightTree ~(Node brX lbX tsX)) (HeightTree ~(Node brY _ tsY)) =
    HeightTree $ Node (f brX brY) lbX (zipWith g tsX tsY)
    where
      g x y = getHeightTree $ liftA2 f (HeightTree x) (HeightTree y)
  (HeightTree ~(Node _ lbX tsX)) *> (HeightTree ~(Node brY _ tsY)) =
    HeightTree $ Node brY lbX (zipWith f tsX tsY)
    where
      f x y = getHeightTree $ HeightTree x *> HeightTree y
  (HeightTree ~(Node brX lbX tsX)) <* (HeightTree ~(Node _ _ tsY)) =
    HeightTree $ Node brX lbX (zipWith f tsX tsY)
    where
      f x y = getHeightTree $ HeightTree x <* HeightTree y

instance ToJSON a => ToJSON (HeightTree a)

instance FromJSON a => FromJSON (HeightTree a)

-- | Check if 'HeightTree' is valid.
--
-- Heights of leaves are 0.
--
-- Height of parent node is greater than height of daughter node.
isValidHeightTree :: HeightTree Double -> Bool
isValidHeightTree = go (1 / 0) . getHeightTree
  where
    go hParent (Node h _ []) = hParent > h && h == 0
    go hParent (Node h _ ts) = hParent > h && all (go h) ts

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
{-# SPECIALIZE toHeightTreeUltrametric :: Tree Length Name -> HeightTree Double #-}

-- Assume the tree is ultrametric.
toHeightTreeUltrametric' :: HasLength a => Tree a Name -> HeightTree Double
toHeightTreeUltrametric' t@(Node _ lb ts) =
  HeightTree $
    Node (assureNonNegative "toHeightTreeUltrametric'" h) lb $
      map (getHeightTree . toHeightTreeUltrametric') ts
  where
    h = fromLength $ rootHeight t
    -- We explicitly call 'error' here and do not use 'assert', because we want
    -- to check if the initial tree is faulty in production.
    assureNonNegative n val
      | val < 0 = error $ n <> ": Negative value: " <> show val <> "."
      | otherwise = val

-- | Calculate branch lengths and remove node heights.
heightTreeToLengthTree :: (Ord a, Num a) => HeightTree a -> LengthTree a
heightTreeToLengthTree t' = LengthTree $ go (branch t) t
  where
    t = getHeightTree t'
    go hParent (Node hNode lb ts) =
      let !l = hParent - hNode
       in -- Do not check for negative values here, because the assertion may
          -- trigger when calculating the gradient.
          Node l lb $! map (go hNode) ts
{-# SPECIALIZE heightTreeToLengthTree :: HeightTree Double -> LengthTree Double #-}

-- | The rate at each branch is drawn from a class.
newtype RateClassTree = RateClassTree {getRateClassTree :: Tree Bool Name}
  deriving (Eq, Generic)

instance ToJSON RateClassTree

instance FromJSON RateClassTree

-- | Create a rate class tree from a tree. Alternate classes.
rateClassTreeFromTree :: Tree a Name -> RateClassTree
rateClassTreeFromTree = RateClassTree . alternateLabels False
  where
    alternateLabels b (Node _ x xs) =
      let bs = cycle [b, not b]
       in Node (not b) x $ zipWith alternateLabels bs xs
