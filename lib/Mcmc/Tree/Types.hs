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

import Control.Applicative
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

-- IDEA: Call these 'BTree a' and 'NTree a', and use 'LengthTree = BTree
-- Length', 'HeightTree = NTree Height'. This would ensure that the values are
-- non-negative.

-- | Tree with branch lengths.
newtype LengthTree a = LengthTree {getLengthTree :: Tree a Name}
  deriving (Generic)

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
    LengthTree $ Node (brF brX) lbF (getLengthTree <$> zipWith f tsF tsX)
    where
      f x y = LengthTree x <*> LengthTree y
  liftA2 f (LengthTree ~(Node brX lbX tsX)) (LengthTree ~(Node brY _ tsY)) =
    LengthTree $ Node (f brX brY) lbX (getLengthTree <$> zipWith f' tsX tsY)
    where
      f' x y = liftA2 f (LengthTree x) (LengthTree y)
  (LengthTree ~(Node _ lbX tsX)) *> (LengthTree ~(Node brY _ tsY)) =
    LengthTree $ Node brY lbX (getLengthTree <$> zipWith f tsX tsY)
    where
      f x y = LengthTree x *> LengthTree y
  (LengthTree ~(Node brX lbX tsX)) <* (LengthTree ~(Node _ _ tsY)) =
    LengthTree $ Node brX lbX (getLengthTree <$> zipWith f tsX tsY)
    where
      f x y = LengthTree x <* LengthTree y

instance ToJSON a => ToJSON (LengthTree a)

instance FromJSON a => FromJSON (LengthTree a)

-- | Tree with node heights.
newtype HeightTree a = HeightTree {getHeightTree :: Tree a Name}
  deriving (Generic)

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
    HeightTree $ Node (brF brX) lbF (getHeightTree <$> zipWith f tsF tsX)
    where
      f x y = HeightTree x <*> HeightTree y
  liftA2 f (HeightTree ~(Node brX lbX tsX)) (HeightTree ~(Node brY _ tsY)) =
    HeightTree $ Node (f brX brY) lbX (getHeightTree <$> zipWith f' tsX tsY)
    where
      f' x y = liftA2 f (HeightTree x) (HeightTree y)
  (HeightTree ~(Node _ lbX tsX)) *> (HeightTree ~(Node brY _ tsY)) =
    HeightTree $ Node brY lbX (getHeightTree <$> zipWith f tsX tsY)
    where
      f x y = HeightTree x *> HeightTree y
  (HeightTree ~(Node brX lbX tsX)) <* (HeightTree ~(Node _ _ tsY)) =
    HeightTree $ Node brX lbX (getHeightTree <$> zipWith f tsX tsY)
    where
      f x y = HeightTree x <* HeightTree y

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
{-# SPECIALIZE toHeightTreeUltrametric :: Tree Length Name -> HeightTree Double #-}

-- Assume the tree is ultrametric.
toHeightTreeUltrametric' :: HasLength a => Tree a Name -> HeightTree Double
toHeightTreeUltrametric' t@(Node _ lb ts) =
  HeightTree $
    Node (assertNonNegative "toHeightTreeUltrametric'" h) lb $
      map (getHeightTree . toHeightTreeUltrametric') ts
  where
    h = fromLength $ rootHeight t

-- | Calculate branch lengths and remove node heights.
heightTreeToLengthTree :: (Ord a, Num a, Show a) => HeightTree a -> LengthTree a
heightTreeToLengthTree t' = LengthTree $ go (branch t) t
  where
    t = getHeightTree t'
    go hParent (Node hNode lb ts) =
      let l = hParent - hNode
       -- XXX: This assertion triggers when calculating the gradient for negative lengths.
       -- in Node (assertNonNegative "heightTreeToLengthTree" l) lb $ map (go hNode) ts
       in Node l lb $ map (go hNode) ts
{-# SPECIALIZE heightTreeToLengthTree :: HeightTree Double -> LengthTree Double #-}

assertNonNegative :: (Ord a, Num a, Show a) => String -> a -> a
assertNonNegative n val
  | val < 0 = error $ n <> ": Negative value: " <> show val <> "."
  | otherwise = val
