{-|
Module      : Trees.Trees
Description : Deriving tidy drawings of trees
Maintainer  : kren.domen@gmail.com, rok.kolesa@gmail.com
Stability   : experimental

This module concentrates on deriving tidy drawings of trees.
-}
module Trees.Trees where

import Control.DeepSeq

-- | Btree stands for Binary Tree.
data Btree x 
	-- | Leaf Lf labelled with a single element x.
	= Lf x 
	-- | Branch Br consisting of two children and a label x.
	| Br (Btree x) x (Btree x)
		deriving Show
		
instance NFData x => NFData (Btree x) where
	rnf (Lf x) = seq (Lf x) ()
	rnf (Br l x r) = (rnf r) `seq` (rnf l) `seq` (rnf x) 
	

-- | Converts an element to singleton list.
wrapl :: a -> [a]
wrapl a = [a]

-- | Returns the smallest element of the given list.
smallest :: Ord a => [a] -> a
smallest [x] = x
smallest (x:xs) = min x (smallest xs)

-- | Returns the largest element of the given list.
largest :: Ord a => [a] -> a
largest [x] = x
largest (x:xs) = max x (largest xs)

-- | Map function on binary trees.
bmap :: (a -> b) -> Btree a -> Btree b
bmap f (Lf x) = Lf (f x)
bmap f (Br l x r) = Br (bmap f l) (f x) (bmap f r)

-- | Subtrees takes a Btree and returns a tree of trees. The result is the same shape as the original tree, but each element is replaced by its descendants, that is, by the subtree of the original tree rooted at that element.
subtrees :: Btree a -> Btree (Btree a)
subtrees (Lf x) = Lf (Lf x)
subtrees (Br l x r) = Br (subtrees l) (Br r x l) (subtrees r)

infixr :++>:

infixr :<++:

-- | TPath constructor.
data TPath x 
 -- | Single element TPath.
 = Sep x 
 -- | TPath of two elements joined with right turn.
 | TPath x :++>: TPath x
 -- | TPath of two elements joined with left turn.
 | TPath x :<++: TPath x
	deriving(Show)
 
-- | Wrap takes x to Sep x
wrapp :: a -> TPath a
wrapp a = Sep a

-- | Zip two lists using function f. Short version.
szip :: (a -> a -> a) -> ([a], [a]) -> [a]
szip f ([], _) = []
szip f (_, []) = []
szip f (x:xs, y:ys) = [f x y] ++ szip f (xs, ys)

-- | Zip two lists using function f. Long version.
lzip :: (a -> a -> a) -> ([a], [a]) -> [a]
lzip f ([], y) = y
lzip f (x, []) = x
lzip f (x:xs, y:ys) = [f x y] ++ lzip f (xs, ys)

-- | Takes a binary tree and returns a list of lists, each containing elements that belong to the same level.
tLevels :: Btree a -> [[a]]
tLevels (Lf x) = (wrapl.wrapl) x
tLevels (Br l x r) = (wrapl.wrapl) x ++ (lzip (++) (tLevels l, tLevels r))

-- | Infix function that calculates the width of the narrowest part of the gap between the trees.
(<+>) :: (Num a, Ord a) => Btree a -> Btree a -> a
p <+> q = smallest(szip (\x -> \y -> y - x) (map largest (tLevels p), map smallest (tLevels q)))

-- | Replaces every element with x coordinates relative to root node.
bdraw :: Btree a -> Btree Double
bdraw (Lf a) = Lf 0
bdraw (Br l x r) = Br (bmap (+(negate d)) (bdraw l)) 0 (bmap (+d) (bdraw r)) where d = tSep(Br l x r)

-- | Calculates optimal offset for children nodes for the given tree.
tSep (Lf a) = 0
tSep (Br l x r) = (1 - (bdraw l <+> bdraw r)) / 2