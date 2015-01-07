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
 

bdraw :: Btree a -> Btree Double
bdraw = btabs . rel

rel :: Btree t -> Btree Double
rel = bmap spread . up contours

contours :: (Ord t1, Fractional t1) => Btree t -> ([t1], [t1])
contours (Lf a) = ([0], [0])
contours (Br l x r) = (contours l) <:-:> (contours r)

(<:-:>) :: (Ord a, Fractional a) => ([a], [a]) -> ([a], [a]) -> ([a], [a])
(w, x) <:-:> (y, z) = ( 0 : lzipfst (mapplus (negate s, w), mapplus (s, y)),
						0 : lzipsnd (mapplus (negate s, x), mapplus (s, z)))
							where s = (1 - (x <:*:> y)) / 2

mapplus :: Num t => (t, [t]) -> [t]
mapplus (b, [x]) = [x + b]
mapplus (b, x : xs) = (x + b) : xs 

lzipfst :: Num t => ([t], [t]) -> [t]
lzipfst (x, y) = if (nst (x, y)) then x
								 else (x ++ mapplus (sum v - sum x, w))
						where (v, w) = splitList (length x, y)

lzipsnd :: Num t => ([t], [t]) -> [t]
lzipsnd (x, y) = lzipfst (y, x)

nst :: ([t1], [t]) -> Bool
nst(x, [b]) = True
nst([x], b : bs) = False
nst(x : xs, b : bs) = nst (xs, bs)

splitList :: (Num t, Eq t) => (t, [t1]) -> ([t1], [t1])
splitList (1, x : xs) = ([x], xs)
splitList (n, x : xs) = (x : v, w) where (v, w) = splitList (n - 1, xs)

spread :: (Ord a, Num a) => ([a], [a]) -> a
spread ([0], [0]) = 0
spread (0 : xs, 0 : ys ) = (negate (head xs)) <.> (head ys) 
	where a <.> b = min a b

-- PROBLEMATIC PART
-- =============================
-- This function needed to be changed as it needs absolute positions of children.
(<:*:>) :: (Ord a, Num a) => [a] -> [a] -> a
p <:*:> q = smallest (abszip p q 0 0)

-- This was created to compute those absolute positions.
abszip :: Num t => [t] -> [t] -> t -> t -> [t]
abszip [] _ _ _ = []
abszip _ [] _ _ = []
abszip (x:xs) (y:ys) delx dely = ((dely + y) - (delx + x)) : abszip xs ys (delx + x) (dely + y)
-- =============================

btabs = bmap fst . down pabsb

pabsb = fork (pabs, bottom)

pabs (Sep a) = 0
pabs (x :<++: (Sep a)) = pabs x - bottom x
pabs (x :++>: (Sep a)) = pabs x + bottom x
pabs (x :++>: y) = pabs y + bottom x
pabs (x :<++: y) = pabs y - bottom x

bottom (Sep y) = y
bottom (x :++>: y) = bottom y  
bottom (x :<++: y) = bottom y

down :: (TPath a -> b) -> Btree a -> Btree b
down f = bmap f . paths


paths :: Btree a -> Btree (TPath a)
paths (Lf x) = Lf (wrapp x)
paths(Br l x r) = Br (bmap ((wrapp x) :<++:) (paths l)) (wrapp x) (bmap ((wrapp x) :++>:) (paths r))

up :: (Btree a -> b) -> Btree a -> Btree b
up f = bmap f . subtrees


fork :: (a -> b, a -> d) -> a -> (b, d)
fork (f, g) a = (f a, g a)

smallest :: Ord a => [a] -> a
smallest [x] = x
smallest (x:xs) = min x (smallest xs)

bmap :: (a -> b) -> Btree a -> Btree b
bmap f (Lf x) = Lf (f x)
bmap f (Br l x r) = Br (bmap f l) (f x) (bmap f r)

wrapp :: a -> TPath a
wrapp a = Sep a		

subtrees :: Btree a -> Btree (Btree a)
subtrees (Lf x) = Lf (Lf x)
subtrees (Br l x r) = Br (subtrees l) (Br l x r) (subtrees r)		
