import           System.Random
import qualified Data.Map as M
import           Data.Map ((!))

fisherYates :: RandomGen g => [a] -> g -> ([a], g)
fisherYates a gen = shuffle 1 gen m
  where m = M.fromList $ zip [1..] a
        shuffle i g k
          | i == M.size m = (M.elems k, g)
          | otherwise     = let (j, g') = randomR (i, M.size m) g
                                k' = M.insert i (k!j) $ M.insert j (k!i) k
                            in shuffle (i+1) g' k'

isSorted :: Ord a => [a] -> Bool
isSorted = all (uncurry (<=)) . (zip <*> tail)

bogoSort :: (Ord a, RandomGen g) => g -> [a] -> [a]
bogoSort g a = fst $ head $
               filter (isSorted . fst) $
               iterate (uncurry fisherYates) (a, g)

main = do
  g <- newStdGen
  print $ bogoSort g [9, 4, 3, 2, 5, 8, 6, 1, 7]
