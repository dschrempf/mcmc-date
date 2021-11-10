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
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Csv hiding (Name)
import qualified Data.Vector as VB
import ELynx.Tree
import GHC.Generics
import Mcmc.Tree.Mrca

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
  undefined
