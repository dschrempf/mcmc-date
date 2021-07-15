-- |
-- Module      :  Mcmc.Tree.Monitor
-- Description :  Monitors for trees
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jul 25 14:06:14 2020.
module Mcmc.Tree.Monitor
  ( monitorTree,
    monitorTreeG,
  )
where

import Data.Bifunctor
import ELynx.Tree
import Mcmc

-- | Monitor a tree in Newick format.
monitorTree ::
  -- | Name.
  String ->
  MonitorParameter (Tree Double Name)
monitorTree n =
  MonitorParameter
    n
    (toNewickBuilder . lengthToPhyloTree . setLengths)
  where
    setLengths = first toLengthUnsafe

-- | See 'monitorTree'.
--
-- Slower but more general.
monitorTreeG ::
  (HasLength e, HasName a) =>
  -- | Name.
  String ->
  MonitorParameter (Tree e a)
monitorTreeG n = MonitorParameter n (toNewickBuilder . lengthToPhyloTree)

