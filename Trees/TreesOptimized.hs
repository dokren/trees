{-|
Module      : Trees.TreesOptimized
Description : Deriving tidy drawings of trees
Maintainer  : kren.domen@gmail.com, rok.kolesa@gmail.com
Stability   : experimental

This module concentrates on deriving tidy drawings of trees efficiently.
-}
module Trees.TreesOptimized where

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
 
-- | Replaces every element with x coordinates relative to root node.
bdraw :: Btree a -> Btree Double
bdraw = btabs . rel

-- | Replaces every element with relative coordinates according to their children.
rel :: Btree t -> Btree Double
rel = bmap spread . up contours

-- | Returns an outline of left-most and right-most children by levels.
contours :: (Ord t1, Fractional t1) => Btree t -> ([t1], [t1])
contours (Lf a) = ([0], [0])
contours (Br l x r) = (contours l) <:-:> (contours r)

-- | Calculates outline relative to two given outlines of children.
(<:-:>) :: (Ord a, Fractional a) => ([a], [a]) -> ([a], [a]) -> ([a], [a])
(w, x) <:-:> (y, z) = ( 0 : lzipfst (mapplus (negate s, w), mapplus (s, y)),
			0 : lzipsnd (mapplus (negate s, x), mapplus (s, z)))
				where s = (1 - (x <:*:> y)) / 2

-- | Adds a number to the head of given list.
mapplus :: Num t => (t, [t]) -> [t]
mapplus (b, [x]) = [x + b]
mapplus (b, x : xs) = (x + b) : xs 

-- | Returns the left outline.
lzipfst :: Num t => ([t], [t]) -> [t]
lzipfst (x, y) = if (nst (x, y)) 
		then x
		else (x ++ mapplus (sum v - sum x, w))
		where (v, w) = splitList (length x, y)

-- | Returns the right outline.
lzipsnd :: Num t => ([t], [t]) -> [t]
lzipsnd (x, y) = lzipfst (y, x)

-- | Checks if first given list is not shorter than second.
nst :: ([t1], [t]) -> Bool
nst(x, [b]) = True
nst([x], b : bs) = False
nst(x : xs, b : bs) = nst (xs, bs)

-- | Splits the given list according to the given number.
splitList :: (Num t, Eq t) => (t, [t1]) -> ([t1], [t1])
splitList (1, x : xs) = ([x], xs)
splitList (n, x : xs) = (x : v, w) where (v, w) = splitList (n - 1, xs)

-- | Returns distance to direct children.
spread :: (Ord a, Num a) => ([a], [a]) -> a
spread ([0], [0]) = 0
spread (0 : xs, 0 : ys ) = (negate (head xs)) <.> (head ys) 
	where a <.> b = min a b

-- | Calculates overlap of given outlines.
(<:*:>) :: (Ord a, Num a) => [a] -> [a] -> a
p <:*:> q = smallest (abszip p q 0 0)

-- | Calculates absolute outlines from relative and zips them together.
abszip :: Num t => [t] -> [t] -> t -> t -> [t]
abszip [] _ _ _ = []
abszip _ [] _ _ = []
abszip (x:xs) (y:ys) delx dely = ((dely + y) - (delx + x)) : abszip xs ys (delx + x) (dely + y)

-- | Calculates absolute coordinates from relative.
btabs :: Btree Double -> Btree Double
btabs = bmap fst . down pabsb

-- | Returns absolute and relative coordinate, which are computed from TPath of ancestors.
pabsb :: TPath Double -> (Double, Double)
pabsb = fork (pabs, bottom)

-- | Returns absolute coordinate for given path of ancestors.
pabs :: Num a => TPath a -> a
pabs (Sep a) = 0
pabs (x :<++: (Sep a)) = pabs x - bottom x
pabs (x :++>: (Sep a)) = pabs x + bottom x
pabs (x :++>: y) = pabs y + bottom x
pabs (x :<++: y) = pabs y - bottom x

-- | Returns relative coordinate for given path of ancestors.
bottom :: TPath t -> t
bottom (Sep y) = y
bottom (x :++>: y) = bottom y  
bottom (x :<++: y) = bottom y

-- | Downwards accumulation. Applies f on every TPath from paths.
down :: (TPath a -> b) -> Btree a -> Btree b
down f = bmap f . paths

-- | Paths replaces each element of a tree with that element's ancestors.
paths :: Btree a -> Btree (TPath a)
paths (Lf x) = Lf (wrapp x)
paths(Br l x r) = Br (bmap ((wrapp x) :<++:) (paths l)) (wrapp x) (bmap ((wrapp x) :++>:) (paths r))

-- | Upwards accumulation. Applies f on every subtree from subtrees.
up :: (Btree a -> b) -> Btree a -> Btree b
up f = bmap f . subtrees

-- | The fork (f, g) of two functions f and g takes a single value and returns a pair.
fork :: (a -> b, a -> d) -> a -> (b, d)
fork (f, g) a = (f a, g a)

-- | Subtrees takes a Btree and returns a tree of trees. The result is the same shape as the original tree, but each element is replaced by its descendants, that is, by the subtree of the original tree rooted at that element.
smallest :: Ord a => [a] -> a
smallest [x] = x
smallest (x:xs) = min x (smallest xs)

-- | Implementation of map on Btrees.
bmap :: (a -> b) -> Btree a -> Btree b
bmap f (Lf x) = Lf (f x)
bmap f (Br l x r) = Br (bmap f l) (f x) (bmap f r)

-- | Wraps given element to single element path (Sep).
wrapp :: a -> TPath a
wrapp a = Sep a		

-- | Subtrees takes a Btree and returns a tree of trees. The result is the same shape as the original tree, but each element is replaced by its descendants, that is, by the subtree of the original tree rooted at that element.
subtrees :: Btree a -> Btree (Btree a)
subtrees (Lf x) = Lf (Lf x)
subtrees (Br l x r) = Br (subtrees l) (Br l x r) (subtrees r)		
