{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Data.BST where

import Control.Exception (throw)
import qualified Data.List as L
import Language.Nano.Types (Error (..))
import qualified Test.QuickCheck as QC

-------------------------------------------------------------------------------

-- | BST data type

-------------------------------------------------------------------------------

data BST a
  = -- | empty tree
    Leaf
  | -- | node with left and right subtrees
    Node a (BST a) (BST a)
  deriving (Show)

-- | Binary-Search Ordering Invariant
isOrdered :: (Ord a) => BST a -> Bool
isOrdered Leaf = True
isOrdered (Node e l r) =
  forall l (< e) -- all elts in `l` are less    than `e`
    && forall r (e <) -- all elts in `r` are greater than `e`
    && isOrdered l -- left subtree `isOrdered`
    && isOrdered r -- right subtree `isOrdered`

forall :: BST a -> (a -> Bool) -> Bool
forall Leaf _ = True
forall (Node e l r) p = p e && forall l p && forall r p

-------------------------------------------------------------------------------

-- | The empty BST

-------------------------------------------------------------------------------
empty :: BST a
empty = Leaf

-------------------------------------------------------------------------------

-- | Build a tree from a list

-------------------------------------------------------------------------------
build :: (Ord a) => [a] -> BST a
build [] = Leaf
build (x : xs) = Node x lNode rNode
  where
    lNode = build [l | l <- xs, l < x]
    rNode = build [r | r <- xs, r > x]

-------------------------------------------------------------------------------

-- | Check membership in BST

-------------------------------------------------------------------------------
contains :: (Ord a) => a -> BST a -> Bool
contains _ Leaf = False
contains x (Node y l r) = x == y || contains x l || contains x r

t2 :: BST Int
t2 = Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

-------------------------------------------------------------------------------

-- | In-order traversal (fold)

-------------------------------------------------------------------------------
fold :: (b -> a -> b) -> b -> BST a -> b
fold _ b Leaf = b
fold f b (Node x l r) = fold f (f (fold f b l) x) r

toList :: BST a -> [a]
toList = reverse . fold (\xs x -> x : xs) []

toString :: (Show a) => BST a -> String
toString t = "build " ++ show (toList t)

-------------------------------------------------------------------------------

-- | Adding an element

-------------------------------------------------------------------------------
add :: (Ord a) => a -> BST a -> BST a
add x Leaf = Node x Leaf Leaf
add x t@(Node y l r)
  | x < y = Node y (add x l) r
  | x > y = Node y l (add x r)
  | otherwise = t

-------------------------------------------------------------------------------

-- | Removing the minumum element

-------------------------------------------------------------------------------
removeMin :: (Ord a) => BST a -> (a, BST a)
removeMin Leaf = throw (Error "empty tree: removeMin")
removeMin (Node x Leaf r) = (x, r)
removeMin (Node x l r) = (fst minNode, Node x (snd minNode) r)
  where
    minNode = removeMin l

-------------------------------------------------------------------------------

-- | Removing an element

-------------------------------------------------------------------------------
remove :: (Ord a) => a -> BST a -> BST a
remove _ Leaf = Leaf
remove x (Node y l r)
  | x > y = Node y l (remove x r)
  | x < y = Node y (remove x l) r
  | otherwise = appendMin l r

-- Append the first tree to the second tree and keep the deriving tree a binary-search tree.
appendMin :: (Ord a) => BST a -> BST a -> BST a
appendMin Leaf t2 = t2
appendMin t1 Leaf = t1
appendMin t1 (Node x Leaf r) = Node x t1 r
appendMin t1 (Node x l r) = Node x (appendMin t1 l) r
appendMin _ _ = throw (Error "type error: appendMin")

-------------------------------------------------------------------------------

-- | QuickCheck Properties

-------------------------------------------------------------------------------

--  Holds after `build`
prop_build :: [Int] -> Bool
prop_build xs = isOrdered (build xs)

--  Holds after `contains` and `build`
prop_contains_elt :: Int -> [Int] -> Bool
prop_contains_elt x xs = (x `elem` xs) == (contains x (build xs))

--  Holds after `contains` and `fold`
prop_contains_elts :: BST Int -> Bool
prop_contains_elts t = and [contains x t | x <- toList t]

-- Holds after `add`
prop_add_elt :: Int -> BST Int -> Bool
prop_add_elt elt t = contains elt (add elt t)

-- Holds after `add`
prop_add_elts_old :: Int -> BST Int -> Bool
prop_add_elts_old elt t = forall t (\x -> contains x t')
  where
    t' = add elt t

-- Holds after `add`
prop_add_isOrd :: Int -> BST Int -> Bool
prop_add_isOrd elt t = isOrdered (add elt t)

-- Holds after `add`: Fix this property
prop_multiset :: [Int] -> Bool
prop_multiset xs = toList (build xs) == (map head . L.group . L.sort) xs -- <<<< TBD: you need to fix this property

-- Holds after `removeMin`
prop_remove_min :: BST Int -> Bool
prop_remove_min Leaf = True
prop_remove_min t = contains x t && forall t' (\y -> x < y)
  where
    (x, t') = removeMin t

-- Holds after `remove`
prop_remove :: Int -> BST Int -> Bool
prop_remove elt t = not (contains elt t')
  where
    t' = remove elt t

-- Holds after `remove`
prop_remove_old :: Int -> BST Int -> Bool
prop_remove_old elt t = forall t (\x -> x == elt || contains x t')
  where
    t' = remove elt t

-- Holds after `remove`
prop_remove_isOrd :: Int -> BST Int -> Bool
prop_remove_isOrd elt t = isOrdered (remove elt t)

-------------------------------------------------------------------------------

-- | QuickCheck Instance

-------------------------------------------------------------------------------
quickCheck :: (QC.Testable prop) => prop -> IO ()
quickCheck = QC.quickCheck

instance (Ord a, QC.Arbitrary a) => QC.Arbitrary (BST a) where
  arbitrary = build <$> QC.arbitrary
