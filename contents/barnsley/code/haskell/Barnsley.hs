import Data.Array (Array, bounds, elems, listArray, (!))
import Data.List (intercalate)
import System.Random

data Point = Point Double Double

chaosGame :: RandomGen g => g -> Int -> Array Int (Double, (Point -> Point)) -> [Point]
chaosGame g n hutchinson = take n points
  where
    (x, g') = random g
    (y, g'') = random g'

    cumulProbabilities = scanl1 (+) $ map fst $ elems hutchinson
    to_choice x = length $ takeWhile (x >) cumulProbabilities

    picks = map to_choice $ randomRs (0, 1) g''
    step = fmap snd hutchinson

    points = Point x y : zipWith (step !) picks points

affine :: (Double, Double, Double, Double) -> (Double, Double) -> Point -> Point
affine (xx, xy, yx, yy) (a, b) (Point x y) = Point (a + xx * x + xy * y) (b + yx * x + yy * y)

showPoint :: Point -> String
showPoint (Point x y) = show x ++ "\t" ++ show y

main :: IO ()
main = do
  g <- newStdGen
  let barnsley =
        listArray
          (0, 3)
          [ (0.01, affine (0, 0, 0, 0.16) (0, 0)),
            (0.85, affine (0.85, 0.04, -0.04, 0.85) (0, 1.6)),
            (0.07, affine (0.2, -0.26, 0.23, 0.22) (0, 1.6)),
            (0.07, affine (-0.15, 0.28, 0.26, 0.24) (0, 0.44))
          ]
      points = chaosGame g 100000 barnsley

  writeFile "out.dat" $ intercalate "\n" $ map showPoint points
