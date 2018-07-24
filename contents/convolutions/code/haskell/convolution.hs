import Data.List (tails)

convolution :: (Num a) => [a] -> [a] -> [a]
convolution x = map (sum . zipWith (*) (reverse x)) . spread
  where spread = init . tails . (replicate (length x - 1) 0 ++)
