{-|
Module      : Trees.Trees
Description : Deriving tidy drawings of trees
Maintainer  : kren.domen@gmail.com, rok.kolesa@gmail.com
Stability   : experimental

This module concentrates on deriving tidy drawings of trees.
-}
module Trees.Trees where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Tree
import Diagrams.TwoD.Layout.Tree

-- PART I: Computation

-- | Btree stands for Binary Tree.
data Btree x 
	-- | Leaf Lf labelled with a single element x.
	= Lf x 
	-- | Branch Br consisting of two children and a label x.
	| Br (Btree x) x (Btree x)
		deriving Show

newtype LabelS = LabelS String
instance Show LabelS where
	show (LabelS a) = a

newtype LabelC = LabelC Char
instance Show LabelC where
	show (LabelC a) = a:[]

-- | The fork (f, g) of two functions f and g takes a single value and returns a pair; thus fork (f, g) a = (f a, g a)
fork :: (a -> b, a -> d) -> a -> (b, d)
fork (f, g) a = (f a, g a)

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

-- | Returns the root of given Btree.
root :: Btree a -> a
root (Lf x) = x
root (Br l x r) = x

-- | Returns number of elements in the given Btree.
bSize :: Num b => Btree a -> b
bSize (Lf x) = 1
bSize (Br l x r) = bSize l + 1 + bSize r

-- | Returns depth of the given Btree.
depth :: (Num b, Ord b) => Btree a -> b
depth (Lf x) = 1
depth (Br l x r) = 1 + max (bSize l) (bSize r)

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
ndesc = up bSize

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

-- | Map on TPath.
mapp :: (a -> b) -> TPath a -> TPath b
mapp f (Sep x) = wrapp (f x)
mapp f (x :<++: y) = (mapp f x) :<++: (mapp f y)
mapp f (x :++>: y) = (mapp f x) :++>: (mapp f y)

-- | Paths replaces each element of a tree with that element's ancestors.
paths :: Btree a -> Btree (TPath a)
paths (Lf x) = Lf (wrapp x)
paths(Br l x r) = Br (bmap ((wrapp x) :<++:) (paths l)) (wrapp x) (bmap ((wrapp x) :++>:) (paths r))

-- | Downwards accumulation. Applies f on every TPath from paths.
down :: (TPath a -> b) -> Btree a -> Btree b
down f = bmap f . paths

-- | TPath length
plen :: Num b => TPath a -> b
plen (Sep a) = 1
plen (a :<++: b) = plen a + plen b
plen (a :++>: b) = plen a + plen b

-- | Replaces every element with the length of its path of ancestors. 
depths :: Btree a -> Btree Integer
depths = down plen


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

-- | Gives the relative position of every parent.
rel :: Btree a -> Btree Double
rel (Lf _) = Lf 0
rel (Br l x r) = Br (rel l)  (tSep (Br l x r)) (rel r)

tabs (Lf _) = Lf 0
tabs (Br l x r) = Br (bmap (+(negate x)) (tabs l)) 0 (bmap (+x) (tabs r))


-- PART II: Drawing
drawSVG :: Show a => FilePath -> Btree a -> IO ()
drawSVG fname t = renderSVG fname (mkSizeSpec (Just 1000.0) (Just 1000.0)) (tRender t)

tRender t = renderTree (\n -> (alignedText 0.5 0.7 (show n) # fontSizeL 0.3 # fc white <> circle 0.3 # fc black # lc white)) (~~) (mkTree t (addLevels (bdraw t) 0)) # centerXY # pad 1.1

mkTree :: Btree a -> Btree (Double, Double) -> Tree (a, P2)
mkTree (Lf el) (Lf (x,y)) = Node(el, p2(x,y)) []
mkTree (Br l el r) (Br l' (x,y) r') = Node(el, p2(x,y)) [mkTree l l', mkTree r r']
mkTree _ _= error "Trees are not the same size!"

addLevels :: Btree a -> Double -> Btree (a, Double)
addLevels (Lf x) i = Lf (x, i)
addLevels (Br l x r) i = Br (addLevels l (i-1)) (x, i) (addLevels r (i-1)) 

-- test variables
t1 = Br (Lf (LabelS "b")) (LabelS "a") (Br (Lf (LabelS "d")) (LabelS "c") (Lf (LabelS "e")))
t2 = Br (Br (Lf (LabelS "f")) (LabelS "b") (Lf (LabelS "g"))) (LabelS "a") (Br (Lf (LabelS "d")) (LabelS "c") (Lf (LabelS "e")))
t3 = Br (Br (Lf (LabelC 'f')) (LabelC 'b') (Lf (LabelC 'g'))) (LabelC 'a') (Br (Lf (LabelC 'd')) (LabelC 'c') (Lf (LabelC 'e')))
t4 = Br (Lf 2) 1 (Br (Lf 4) 3 (Lf 5))
t5 = Br (Br (Br (Br (Lf 16) (8) (Lf 17)) (4) (Br (Lf 18) (9) (Lf 19))) (2) (Br (Br (Lf 20) (10) (Lf 21)) (5) (Br (Lf 22) (11) (Lf 23)))) (1) (Br (Br (Br (Lf 24) (12) (Lf 25)) (6) (Br (Lf 26) (13) (Lf 27))) (3) (Br (Br (Lf 28) (14) (Lf 29)) (7) (Br (Lf 30) (15) (Lf 31))))
t6 = Br (Br (Br (Br (Lf 16) (8) (Lf 17)) (4) (Br (Lf 18) (9) (Lf 19))) (2) (Br (Br (Lf 20) (10) (Lf 21)) (5) (Br (Lf 22) (11) (Lf 23)))) (1) (Lf 3)
t7 = Br (Br (Br (Br (Lf 16) (8) (Lf 17)) (4) (Br (Lf 18) (9) (Lf 19))) (2) (Br (Br (Lf 20) (10) (Lf 21)) (5) (Br (Lf 22) (11) (Lf 23)))) (1) (Br (Lf 6) 3 (Lf 7) )
t8 = Br (Br (Br (Br (Lf 16) (8) (Lf 17)) (4) (Lf 9)) (2) (Br (Br (Lf 20) (10) (Lf 21)) (5) (Br (Lf 22) (11) (Lf 23)))) (1) (Br (Lf 6) 3 (Lf 7) )

testsString = [t1, t2]
testsChar=[t3]
testsInt=[t4, t5, t6, t7, t8]

stringTrees = map (\(i,x) -> drawSVG ("stringTree"++(show i)++".svg") x) (zip [1..] testsString)
charTrees = map (\(i,x) -> drawSVG ("charTree"++(show i)++".svg") x) (zip [1..] testsChar)
intTrees = map (\(i,x) -> drawSVG ("intTree"++(show i)++".svg") x) (zip [1..] testsInt)

main = do 
	sequence stringTrees
	sequence charTrees
	sequence intTrees