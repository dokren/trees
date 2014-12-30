import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import System.Random


import System.CPUTime

randomList seed = randoms (mkStdGen seed) :: [Double]

values :: Int -> [Int]
values seed = map fst $ scanl (\(r, gen) _ -> random gen) (random (mkStdGen seed)) $ repeat ()

generateRandomTree 0 _ = Lf 1
generateRandomTree j n = Br (generateRandomTree m x) 1 (generateRandomTree (i-m) y)
				where i = j - 1
				      (x:y:z:xyzs) = values n
				      m = ceiling (head (randoms (mkStdGen z) :: [Double]) * (fromIntegral i)) :: Int
				      
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

(<:*:>) :: (Ord a, Num a) => [a] -> [a] -> a
p <:*:> q = smallest (szip (\x -> \y -> y - x) (nth p , nth q))

nth [x] = [x]
nth (x:xs) = x : map (+x) (nth xs)

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

szip :: (a -> a -> a) -> ([a], [a]) -> [a]
szip f ([], _) = []
szip f (_, []) = []
szip f (x:xs, y:ys) = [f x y] ++ szip f (xs, ys)

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
				
-- PART III: Drawing
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