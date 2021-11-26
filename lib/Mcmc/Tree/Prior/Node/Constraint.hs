{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Mcmc.Tree.Prior.Node.Constraint
-- Description :  Relative node order constraints and node calibrations
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Jul 27 10:49:11 2020.
module Mcmc.Tree.Prior.Node.Constraint
  ( -- * Constraints
    Constraint,
    getConstraintName,
    getConstraintYoungNodeIndex,
    getConstraintYoungNodePath,
    getConstraintOldNodeIndex,
    getConstraintOldNodePath,
    getConstraintWeight,
    constraint,
    loadConstraints,
    constrainHardS,
    constrainSoftS,
    constrainSoftF,
    constrainSoft,

    -- * Misc
    realToFracConstraint,
  )
where

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Csv hiding (Name)
import Data.List
import qualified Data.Vector as VB
import ELynx.Tree
import GHC.Generics
import Mcmc.Prior
import Mcmc.Statistics.Types
import Mcmc.Tree.Lens
import Mcmc.Tree.Mrca
import Mcmc.Tree.Prior.Node.Internal
import Mcmc.Tree.Types

-- | Constraints define node orders.
--
-- For example,
--
-- @
--   Constraint "Name" YOUNGER OLDER
-- @
--
-- ensures that the node with path @YOUNGER@ is younger than the node with path
-- @OLDER@.
--
-- Constraints can be created using 'constraint' or 'loadConstraints'. The
-- reason is that finding the nodes on the tree is a slow process not to be
-- repeated at each proposal.
data Constraint a = Constraint
  { constraintName :: String,
    -- | Path to younger node (closer to the leaves).
    constraintYoungNodePath :: Path,
    -- | Index of younger node.
    constraintYoungNodeIndex :: Int,
    -- | Path to older node (closer to the root).
    constraintOldNodePath :: Path,
    -- | Index of older node.
    constraintOldNodeIndex :: Int,
    -- | Weight.
    constraintWeight :: a
  }
  deriving (Eq, Read, Show)

-- | Get name.
getConstraintName :: Constraint a -> String
getConstraintName = constraintName

-- | Get index of younger node.
getConstraintYoungNodeIndex :: Constraint a -> Int
getConstraintYoungNodeIndex = constraintYoungNodeIndex

-- | Get path to younger node.
getConstraintYoungNodePath :: Constraint a -> Path
getConstraintYoungNodePath = constraintYoungNodePath

-- | Get index of older node.
getConstraintOldNodeIndex :: Constraint a -> Int
getConstraintOldNodeIndex = constraintOldNodeIndex

-- | Get path to older node.
getConstraintOldNodePath :: Constraint a -> Path
getConstraintOldNodePath = constraintOldNodePath

-- | Get weight.
getConstraintWeight :: Constraint a -> a
getConstraintWeight = constraintWeight

-- Do the constraints affect the same nodes?
duplicate :: Constraint a -> Constraint a -> Bool
duplicate (Constraint _ _ yL _ oL _) (Constraint _ _ yR _ oR _) = (yL == yR) && (oL == oR)

prettyPrintConstraint :: Show a => Constraint a -> String
prettyPrintConstraint (Constraint n yP yI oP oI w) =
  "Constraint: "
    <> n
    <> "\n  Young node index and path: "
    <> show yI
    <> ", "
    <> show yP
    <> "\n  Old node index and path: "
    <> show oI
    <> ", "
    <> show oP
    <> "\n Weight: "
    <> show w

-- Check if a constraint is valid.
--
-- See also 'validateConstraints' and 'validateConstraintVector' which perform
-- checks on a multiple possibly redundant or conflicting constraints.
--
-- Return 'Left' if:
--
-- - The younger node is a direct ancestor of the old node.
--
-- - The older node is a direct ancestor of the young node.
--
-- NOTE: Any node is a direct ancestor of itself, and so, bogus constraints
-- including the same node twice are also filtered out.
validateConstraint :: Constraint a -> Either String (Constraint a)
validateConstraint c = case areDirectDescendants y o of
  Equal ->
    Left $
      getErrMsg "Bogus constraint; both nodes are equal (?)."
  LeftIsAncestorOfRight ->
    Left $
      getErrMsg "Bogus constraint; younger node is direct ancestor of older node (?)."
  LeftIsDescendantOfRight ->
    Left $
      getErrMsg "Redundant constraint; old node is direct ancestors of young node."
  Unrelated -> Right c
  where
    n = constraintName c
    y = constraintYoungNodePath c
    o = constraintOldNodePath c
    getErrMsg msg = "validateConstraint: " ++ show n ++ ": " ++ msg

-- | Create and validate a constraint.
--
-- Call 'error' if:
--
-- - A node cannot be found on the tree.
--
-- - The younger node is a direct ancestor of the old node.
--
-- - The older node is a direct ancestor of the young node.
constraint ::
  (Ord a, Show a, Num b, Ord b) =>
  Tree e a ->
  -- | Name.
  String ->
  -- | The most recent common ancestor of the given leaves is the younger node.
  [a] ->
  -- | The most recent common ancestor of the given leave is the older node.
  [a] ->
  -- | Weight.
  b ->
  Constraint b
constraint t n ys os w
  | w <= 0 = err "Weight is zero or negative."
  | otherwise =
    either error id $
      validateConstraint $
        Constraint n pY iY pO iO w
  where
    err msg = error $ "constraint: " ++ show n ++ ": " ++ msg
    -- NOTE: Identifying the tree multiple times may be slow when creating many
    -- constraints. But this is only done once in the beginning.
    iTr = identify t
    pY = either err id $ mrca ys t
    iY = label $ getSubTreeUnsafe pY iTr
    pO = either err id $ mrca os t
    iO = label $ getSubTreeUnsafe pO iTr
{-# SPECIALIZE constraint ::
  (Ord a, Show a) =>
  Tree e a ->
  String ->
  [a] ->
  [a] ->
  Double ->
  Constraint Double
  #-}

data ConstraintData a = ConstraintData String String String String String a
  deriving (Generic, Show)

instance FromField a => FromRecord (ConstraintData a)

constraintDataToConstraint :: (Num a, Ord a) => Tree e Name -> ConstraintData a -> Constraint a
constraintDataToConstraint t (ConstraintData n yL yR oL oR w) =
  constraint t n [f yL, f yR] [f oL, f oR] w
  where
    f = Name . BL.pack

-- Given a left constraint, check if a right constraint is redundant.
--
-- Let D(x,y) be true iff x is a descendant of y (see 'isDescendent').
--
-- Let A(x,y) be true iff x is an ancestor of y (see 'isAncestor').
--
-- Given the left constraint a < b, check the right constraint c < d.
--
-- The right constraint is redundant iff: D(c,a) AND A(d,b).
isRedundantWith :: Constraint a -> Constraint a -> Bool
isRedundantWith (Constraint _ a _ b _ _) (Constraint _ c _ d _ _) =
  (c `isDescendant` a) && (d `isAncestor` b)

-- Given a left constraint, check if a right constraint is conflicting.
--
-- See 'isRedundantWith'.
--
-- The right constraint is conflicting iff: A(c,b) AND ( D(d,a) OR D(d,b) ).
isConflictingWith :: Constraint a -> Constraint a -> Bool
isConflictingWith (Constraint _ a _ b _ _) (Constraint _ c _ d _ _) =
  (c `isAncestor` b) && ((d `isDescendant` a) || (d `isDescendant` b))

-- Pairwise comparison using a given function. Keep comparisons returning 'True'.
--
-- Do not assume commutativity of the operator. Compare (x, y) and (y, x).
validateWith :: Eq a => (a -> a -> Bool) -> [a] -> [(a, a)]
validateWith p xs = [(x, y) | x <- xs, y <- xs, x /= y, p x y]

-- Pairwise comparison using a given function. Keep comparisons returning 'True'.
--
-- Assume commutativity of the operator. Only compare (x, y).
validateWithCommutative :: Eq a => (a -> a -> Bool) -> [a] -> [(a, a)]
validateWithCommutative p xs = [(x, y) | (x, ys) <- zip xs (tails xs), y <- ys, x /= y, p x y]

describeConflicting :: (Constraint a, Constraint a) -> String
describeConflicting (l, r) =
  "Constraint " <> constraintName r <> " is conflicting given constraint " <> constraintName l <> "."

describeEqual :: (Constraint a, Constraint a) -> String
describeEqual (x, y) =
  "Constraints " <> constraintName x <> " and " <> constraintName y <> " affect the same nodes."

describeRedundant :: (Constraint a, Constraint a) -> String
describeRedundant (l, r) =
  "Constraint " <> constraintName r <> " is redundant given constraint " <> constraintName l <> "."

-- | Load and validate constraints from file.
--
-- The constraint file is a comma separated values (CSV) file with rows of the
-- following format:
--
-- @
-- ConstraintName,YoungLeafA,YoungLeafB,OldLeafA,OldLeafB,Weight
-- @
--
-- The young and old nodes are uniquely defined as the most recent common
-- ancestors (MRCA) @YoungLeafA@ and @YoungLeafB@, as well as @OldLeafA@ and
-- @OldLeafB@.
--
-- The following line defines a constraint where the ancestor of leaves A and B
-- is younger than the ancestor of leaves C and D:
--
-- @
-- ExampleConstraint,A,B,C,D,1.0
-- @
--
-- Redundant constraints are removed.
--
-- Call 'error' if:
--
-- - The file contains syntax errors.
--
-- - An MRCA cannot be found.
--
-- - Conflicting constraints are found.
loadConstraints :: Tree e Name -> FilePath -> IO (VB.Vector (Constraint Double))
loadConstraints t f = do
  d <- BL.readFile f
  let mr = decode NoHeader d :: Either String (VB.Vector (ConstraintData Double))
      cds = either error id mr
  when (VB.null cds) $ error $ "loadConstraints: No constraints found in file: " <> f <> "."
  let allConstraints = VB.toList $ VB.map (constraintDataToConstraint t) cds
  putStrLn $ "The total number constraints is: " <> show (length allConstraints) <> "."
  -- Call 'error' when constraints are conflicting. We don't want to repair
  -- this.
  let conflictingCs = validateWith isConflictingWith allConstraints
  if null conflictingCs
    then putStrLn "No conflicting constraints have been detected."
    else do
      mapM_ (putStrLn . describeConflicting) conflictingCs
      error "loadConstraints: Conflicting constraints have been detected."
  -- We do remove duplicate constraints, but we are very verbose about it.
  let equalCs = validateWithCommutative duplicate allConstraints
  uniqueConstraints <-
    if null equalCs
      then do
        putStrLn "No duplicate constraints have been detected."
        return allConstraints
      else do
        putStrLn "The following duplicates have been detected:"
        mapM_ (putStrLn . describeEqual) equalCs
        -- Extract the unique duplicate constraints (they are the right ones of
        -- each tuple).
        let uniqueDuplicateConstraints = nub $ map snd equalCs
        putStrLn $
          "The number of unique duplicate constraints is: "
            <> show (length uniqueDuplicateConstraints)
            <> "."
        return $ allConstraints \\ uniqueDuplicateConstraints
  -- We do remove redundant constraints, but we are very verbose about it.
  let redundantCs = validateWith isRedundantWith uniqueConstraints
  informativeConstraints <-
    if null redundantCs
      then do
        putStrLn "No redundant constraints have been detected."
        return uniqueConstraints
      else do
        putStrLn "The following redundancies have been detected:"
        mapM_ (putStrLn . describeRedundant) redundantCs
        -- Extract the unique redundant constraints (they are the right ones of
        -- each tuple).
        let uniqueRedundantConstraints = nub $ map snd redundantCs
        putStrLn $
          "The number of unique redundant constraints is: "
            <> show (length uniqueRedundantConstraints)
            <> "."
        return $ uniqueConstraints \\ uniqueRedundantConstraints
  putStrLn $
    "The number of informative constraints is: "
      <> show (length informativeConstraints)
      <> "."
  putStrLn "The informative constraints are:"
  mapM_ (putStrLn . prettyPrintConstraint) informativeConstraints
  return $ VB.fromList informativeConstraints

-- | Hard constrain order of a single pair of nodes with given paths.
--
-- A truncated, improper uniform distribution is used.
--
-- For reasons of computational efficiency, the paths are not checked for
-- validity. Please do so beforehand using 'constraint'.
constrainHardS ::
  RealFloat a =>
  Constraint a ->
  PriorFunctionG (HeightTree a) a
constrainHardS c (HeightTree t)
  | (t ^. subTreeAtL y . branchL) < (t ^. subTreeAtL o . branchL) = 1
  | otherwise = 0
  where
    y = constraintYoungNodePath c
    o = constraintOldNodePath c
{-# SPECIALIZE constrainHardS :: Constraint Double -> PriorFunction (HeightTree Double) #-}

-- | Soft constrain order of a single pair of nodes with given paths.
--
-- When the node order is correct, a uniform distribution is used.
--
-- When the node order is incorrect, a one-sided normal distribution with given
-- standard deviation is used. The normal distribution is normalized such that
-- the complete distribution of the constraint is continuous. Use of the normal
-- distribution also ensures that the first derivative is continuous.
--
-- For reasons of computational efficiency, the paths are not checked for
-- validity. Please do so beforehand using 'constraint'.
constrainSoftS ::
  RealFloat a =>
  StandardDeviation a ->
  Constraint a ->
  PriorFunctionG (HeightTree a) a
constrainSoftS s c (HeightTree t) = constrainSoftF s w (hY, hO)
  where
    hY = t ^. subTreeAtL y . branchL
    hO = t ^. subTreeAtL o . branchL
    y = constraintYoungNodePath c
    o = constraintOldNodePath c
    w = constraintWeight c
{-# SPECIALIZE constrainSoftS :: Double -> Constraint Double -> PriorFunction (HeightTree Double) #-}

-- | See 'constrainSoftS'.
constrainSoftF ::
  RealFloat a =>
  StandardDeviation a ->
  -- | Weight.
  a ->
  PriorFunctionG (a, a) a
constrainSoftF s' w (hY, hO)
  | s <= 0 = error "constrainSoftF: Standard deviation is zero or negative."
  -- Should not be necessary because we use an abstract data type, and the check
  -- is performed in 'constraint'.
  | w <= 0 = error "constrainSoftF: Weight is zero or negative."
  | hY < hO = 1
  | otherwise = d (hY - hO) / d 0
  where
    s = realToFrac s'
    d = normal 0 (s / w)
{-# SPECIALIZE constrainSoftF :: Double -> Double -> PriorFunction (Double, Double) #-}

-- | Constrain nodes of a tree using 'constrainSoftS'.
--
-- Calculate the constraint prior for a given vector of constraints, and a
-- tree with relative heights.
--
-- Call 'error' if a path is invalid.
constrainSoft ::
  RealFloat a =>
  StandardDeviation a ->
  VB.Vector (Constraint a) ->
  PriorFunctionG (HeightTree a) a
constrainSoft sd cs t = VB.product $ VB.map (\c -> constrainSoftS sd c t) cs
{-# SPECIALIZE constrainSoft :: Double -> VB.Vector (Constraint Double) -> PriorFunction (HeightTree Double) #-}

-- | Convert a constraint on 'Double' to a more general one.
--
-- Useful for automatic differentiation.
realToFracConstraint :: Fractional a => Constraint Double -> Constraint a
realToFracConstraint c = c {constraintWeight = w'}
  where
    w' = realToFrac $ constraintWeight c
{-# SPECIALIZE realToFracConstraint :: Constraint Double -> Constraint Double #-}
