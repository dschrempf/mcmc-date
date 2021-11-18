-- |
-- Module      :  Mcmc.Tree.Prior.Node.Internal
-- Description :  Functions used by more modules
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Nov 18 09:03:24 2021.
module Mcmc.Tree.Prior.Node.Internal
  ( isAncestor,
    isDescendant,
    Relationship (..),
    areDirectDescendants,
  )
where

import Data.List

-- | Is the left node an ancestor of the right node?
isAncestor :: Eq a => [a] -> [a] -> Bool
isAncestor = isPrefixOf

-- | Is the left node a descendant of the right node?
isDescendant :: Eq a => [a] -> [a] -> Bool
isDescendant = flip isPrefixOf

-- | Relationship of two nodes.
data Relationship
  = Equal
  | -- | Same as 'RightIsDescendentOfLeft'.
    LeftIsAncestorOfRight
  | -- | Same as 'RightIsAncestorOfLeft'.
    LeftIsDescendantOfRight
  | Unrelated
  deriving (Eq, Show, Read)

-- | Fast function avoiding two consecutive uses of `isPrefixOf`.
--
-- Are the two nodes direct descent of each other? Basically check both:
-- 'isAncestor' and 'isDescendent'.
areDirectDescendants :: Eq a => [a] -> [a] -> Relationship
areDirectDescendants [] [] = Equal
areDirectDescendants (x : xs) (y : ys)
  | x == y = areDirectDescendants xs ys
  | otherwise = Unrelated
areDirectDescendants (_ : _) [] = LeftIsDescendantOfRight
areDirectDescendants [] (_ : _) = LeftIsAncestorOfRight
