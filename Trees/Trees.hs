{-|
Module      : Trees.Trees
Description : Deriving tidy drawings of trees
Maintainer  : kren.domen@gmail.com, rok.kolesa@gmail.com
Stability   : experimental

This module concentrates on deriving tidy drawings of trees.
-}
module Trees.Trees where

-- | Btree stands for Binary Tree.
data Btree x 
	-- | Leaf Lf labelled with a single element x.
	= Lf x 
	-- | Branch Br consisting of two children and a label x.
	| Br (Btree x) x (Btree x)
		deriving Show
	
-- | The fork (f, g) of two functions f and g takes a single value and returns a pair; thus fork (f, g) a = (f a, g a)
fork :: (a -> b, a -> d) -> a -> (b, d)
fork (f, g) a = (f a, g a)

-- | Converts an element to singleton list.
wrapl :: a -> [a]
wrapl a = [a]

-- | Map function on binary trees.
bmap :: (a -> b) -> Btree a -> Btree b
bmap f (Lf x) = Lf (f x)
bmap f (Br l x r) = Br (bmap f l) (f x) (bmap f r)

-- | Returns the root of given Btree.
root :: Btree a -> a
root (Lf x) = x
root (Br l x r) = x

-- | Returns number of elements in the given Btree.
size :: Num b => Btree a -> b
size (Lf x) = 1
size (Br l x r) = size l + 1 + size r

-- | Returns depth of the given Btree.
depth :: (Num b, Ord b) => Btree a -> b
depth (Lf x) = 1
depth (Br l x r) = 1 + max (size l) (size r)

-- | Reverses the given Btree.
brev :: Btree a -> Btree a
brev (Lf x) = Lf x
brev (Br l x r) = Br r x l 

-- | Subtrees takes a Btree and returns a tree of trees. The result is the same shape as the original tree, but each element is replaced by its descendants, that is, by the subtree of the original tree rooted at that element.
subtrees :: Btree a -> Btree (Btree a)
subtrees (Lf x) = Lf (Lf x)
subtrees (Br l x r) = Br (subtrees l) (Br r x l) (subtrees r)

-- | Upwards accumulation. Applies f on every subtree from subtrees.
up :: (Btree a -> b) -> Btree a -> Btree b
up f = bmap f . subtrees

-- | Replaces every element with the number of descendants it has.
ndesc :: Num b => Btree a -> Btree b
ndesc = up size

infixr :++>:

infixr :<++:

-- | Path constructor.
data Path x 
 -- | Single element path.
 = Sep x 
 -- | Path of two elements joined with right turn.
 | Path x :++>: Path x
 -- | Path of two elements joined with left turn.
 | Path x :<++: Path x
	deriving(Show)
 
-- | Wrap takes x to Sep x
wrapp :: a -> Path a
wrapp a = Sep a

-- | Map on path.
mapp :: (a -> b) -> Path a -> Path b
mapp f (Sep x) = wrapp (f x)
mapp f (x :<++: y) = (mapp f x) :<++: (mapp f y)
mapp f (x :++>: y) = (mapp f x) :++>: (mapp f y)

-- | Paths replaces each element of a tree with that element's ancestors.
paths :: Btree a -> Btree (Path a)
paths (Lf x) = Lf (wrapp x)
paths(Br l x r) = Br (bmap ((wrapp x) :<++:) (paths l)) (wrapp x) (bmap ((wrapp x) :++>:) (paths r))

-- | Downwards accumulation. Applies f on every path from paths.
down :: (Path a -> b) -> Btree a -> Btree b
down f = bmap f . paths

-- | Path length
plen :: Num b => Path a -> b
plen (Sep a) = 1
plen (a :<++: b) = plen a + plen b
plen (a :++>: b) = plen a + plen b

-- | Replaces every element with the length of its path of ancestors. 
depths :: Btree a -> Btree Integer
depths = down plen


-- test variables
t = Br (Lf "b") "a" (Br (Lf "d") "c" (Lf "e"))
