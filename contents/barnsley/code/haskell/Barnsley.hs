import Data.Array (Array, bounds, listArray, (!))
import Data.List (intercalate)
import System.Random

data Point = Point Double Double

chaosGame :: RandomGen g => g -> Int -> [Double] -> Array Int (Point -> Point) -> [Point]
chaosGame g n probabilities hutchinson = take n points
  where
    (x, g') = random g
    (y, g'') = random g'

    picks = randomRs (0, 1) g''
    cumulProbabilities = scanl1 (+) probabilities
    to_choice x = (+ 1) $ length $ takeWhile (x >) cumulProbabilities

    points = Point x y : zipWith (hutchinson !) (map to_choice picks) points

main :: IO ()
main = do
  g <- newStdGen

  let affine [xx, xy, yx, yy] [a, b] (Point x y) =
        Point (a + xx * x + xy * y) (b + yx * x + yy * y)
      barnsley =
        listArray
          (1, 4)
          [ affine [0, 0, 0, 0.16] [0, 0],
            affine [0.85, 0.04, -0.04, 0.85] [0, 1.6],
            affine [0.2, -0.26, 0.23, 0.22] [0, 1.6],
            affine [-0.15, 0.28, 0.26, 0.24] [0, 0.44]
          ]
      probabilities = [0.01, 0.85, 0.07, 0.07]
      points = chaosGame g 100000 probabilities barnsley
      showPoint (Point x y) = show x ++ "\t" ++ show y

  writeFile "out.dat" $ intercalate "\n" $ map showPoint points
