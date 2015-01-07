module Draw where

import Trees.Trees
--import Trees.TreesOptimized
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Test

newtype LabelS = LabelS String
instance Show LabelS where
	show (LabelS a) = a

newtype LabelC = LabelC Char
instance Show LabelC where
	show (LabelC a) = a:[]
	
--Drawing
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
testsInt=map bdraw [t4, t5, t6, t7, t8]

stringTrees = map (\(i,x) -> drawSVG ("stringTree"++(show i)++".svg") x) (zip [1..] testsString)
charTrees = map (\(i,x) -> drawSVG ("charTree"++(show i)++".svg") x) (zip [1..] testsChar)
intTrees = map (\(i,x) -> drawSVG ("intTree"++(show i)++".svg") x) (zip [1..] testsInt)

main = do 
	sequence stringTrees
	sequence charTrees
	sequence intTrees
	
t9 = generiraj 100 123

mirror (Lf x) = (Lf x)
mirror (Br l x r) = Br (mirror r) x (mirror l)

draw = do
	let (Br l x r) = generiraj 100 123
	let trees = [(Br l x r), mirror (Br l x r), l, r]
	let drawTrees = map (\(i,x) -> drawSVG ("testTree"++(show i)++".svg") x) (zip [1..] trees)
	sequence drawTrees
