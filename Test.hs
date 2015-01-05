module Test where

--import Trees.Trees
import Trees.TreesOptimized
import System.Random
--import Criterion.Main
import System.CPUTime
import Control.DeepSeq

values :: Int -> [Int]
values seed = map fst $ scanl (\(r, gen) _ -> random gen) (random (mkStdGen seed)) $ repeat ()

--generiraj :: Num x => Int -> Int -> Btree x
generiraj n s = generateRandomTree (round ((fromIntegral n)/2.0)) s

generateRandomTree 0 _ = Lf 1
generateRandomTree j n = Br (generateRandomTree m x) 1 (generateRandomTree (i-m) y)
				where i = j - 1
				      (x:y:z:xyzs) = values n
				      m = ceiling (head (randoms (mkStdGen z) :: [Double]) * (fromIntegral i)) :: Int
				      
generateNdepth 0 = (Lf 1)
generateNdepth n = Br (generateNdepth (n-1)) 1 (generateNdepth (n-1))
				     
-- Benchmarking random trees.
--main = defaultMain(map (\x -> bench (show x) (bm x)) ([100, 200 .. 1000]++[2000, 3000 .. --10000]++[20000, 30000 .. 100000]))

-- Benchmarking full trees.
--main = defaultMain(map (\x -> bench (show x) (bn x)) [1 .. 20])
     
-- Benchmarking different seeds, same node count.
--main = defaultMain(map (\x -> bench (show x) (bs x)) (map (\x -> 2^x) [1 .. 31]))

--bm n = nf (\x -> bdraw(generateRandomTree x 42)) n
--bn n = nf (\x -> bdraw(generateNdepth x)) n
--bs n = nf (\x -> bdraw(generateRandomTree 10000 x)) n

nodes (Lf _) = 1
nodes (Br l _ r) = 1 + nodes l + nodes r

times [] = do return []
times (t:ts) = do
		startTime <- t `deepseq` getCPUTime
		let a = bdraw t
		endTime <- a `deepseq` getCPUTime
		tail <- times ts
		return ((round $ fromIntegral(endTime - startTime)/ 1000000000) : tail)

main = do
	let numOfNodes = [10000, 20000 .. 100000]
	let trees = map (\x -> generiraj x 42) numOfNodes
	benchTimes <- times trees
	return benchTimes
