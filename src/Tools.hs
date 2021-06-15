{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Tools
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Oct 22 16:53:59 2020.
module Tools
  ( getBranches,
    sumFirstTwo,
    toNewickTopology,
    tupleLens,
    tripleLens,
  )
where

import Control.Lens
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector.Storable as V
import qualified ELynx.Topology as T
import ELynx.Tree

-- | Get all branches of a rooted tree. Store the branches in a vector such that
-- the two branches leading to the root are the first two entries of the vector.
-- Ignore the root branch.
getBranches :: Tree Length a -> V.Vector Double
getBranches (Node _ _ [l, r]) =
  {-# SCC getBranches #-}
  V.fromList $ head ls : head rs : tail ls ++ tail rs
  where
    ls = map fromLength $ branches l
    rs = map fromLength $ branches r
getBranches _ = error "getBranches: Root node is not bifurcating."

-- | Sum the first two elements of a vector. Needed to merge the two branches
-- leading to the root.
sumFirstTwo :: V.Vector Double -> V.Vector Double
sumFirstTwo v = (v V.! 0 + v V.! 1) `V.cons` V.drop 2 v

-- | Convert a topology to Newick format.
toNewickTopology :: T.Topology Name -> BL.ByteString
toNewickTopology = toNewick . first (const $ Phylo Nothing Nothing) . T.toLabeledTreeWith ""

-- | Create lenses for tuples.
--
-- Useful for combining proposals.
tupleLens :: Lens' a b1 -> Lens' a b2 -> Lens' a (b1, b2)
tupleLens l1 l2 =
  lens
    (\x -> (x ^. l1, x ^. l2))
    (\x (y1', y2') -> x & l1 .~ y1' & l2 .~ y2')

-- | Create lenses for triples.
--
-- Useful for combining proposals.
tripleLens :: Lens' a b1 -> Lens' a b2 -> Lens' a b3 -> Lens' a (b1, b2, b3)
tripleLens l1 l2 l3 =
  lens
    (\x -> (x ^. l1, x ^. l2, x ^. l3))
    (\x (y1', y2', y3') -> x & l1 .~ y1' & l2 .~ y2' & l3 .~ y3')
