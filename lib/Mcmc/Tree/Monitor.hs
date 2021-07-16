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
    monitorLengthTree,
  )
where

import Data.Bifunctor
import ELynx.Tree
import Mcmc
import Mcmc.Tree.Types

-- | See 'monitorTree'.
--
-- Slower but more general.
monitorTree ::
  (HasLength e, HasName a) =>
  -- | Name.
  String ->
  MonitorParameter (Tree e a)
monitorTree n = MonitorParameter n (toNewickBuilder . lengthToPhyloTree)

-- | Monitor a tree in Newick format.
monitorLengthTree ::
  -- | Name.
  String ->
  MonitorParameter (LengthTree Double)
monitorLengthTree n =
  MonitorParameter
    n
    (toNewickBuilder . lengthToPhyloTree . setLengths . fromLengthTree)
  where
    setLengths = first toLengthUnsafe
