import Data.List (zip4)
import Data.Ratio

thomas :: Fractional a => [a] -> [a] -> [a] -> [a] -> [a]
thomas a b c = init . scanr back 0 . tail . scanl forward (0, 0) . zip4 a b c
  where
    forward (c', d') (a, b, c, d) =
      let denominator = b - a * c'
       in (c / denominator, (d - a * d') / denominator)
    back (c, d) x = d - c * x

main :: IO ()
main = do
  let a = [0, 2, 3] :: [Ratio Int]
      b = [1, 3, 6]
      c = [4, 5, 0]
      d = [7, 5, 3]
  print $ thomas a b c d
