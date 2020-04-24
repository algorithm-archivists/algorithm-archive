import Data.Complex
import Data.List (partition)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Ratio

dft :: [Complex Double] -> [Complex Double]
dft x = matMult dftMat x
  where
    n = length x
    w = exp $ (-2) * pi * (0 :+ 1) / fromIntegral n
    dftMat = [[w ^ (j * k) | j <- [0 .. n - 1]] | k <- [0 .. n - 1]]
    matMult m x = map (sum . zipWith (*) x) m

fft :: [Complex Double] -> [Complex Double]
fft x = fft' x
  where
    n = length x
    w0 = exp ((-2) * pi * (0 :+ 1) / fromIntegral n)
    w = M.fromList [(k % n, w0 ^ k) | k <- [0 .. n - 1]]
    fft' [x] = [x]
    fft' x =
      let (evens, odds) = partition (even . fst) $ zip [0 ..] x
          e = fft' $ map snd evens
          o = fft' $ map snd odds
          x1 = zipWith3 (\e o k -> e + o * w ! (k %n)) e o [0 ..]
          x2 = zipWith3 (\e o k -> e - o * w ! (k %n)) e o [0 ..]
       in x1 ++ x2

main = do
  print $ dft [0, 1, 2, 3]
  print $ fft [0, 1, 2, 3]
