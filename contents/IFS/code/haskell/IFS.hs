import Data.Array ((!), Array, bounds, listArray)
import Data.List (intercalate)
import System.Random

data Point = Point Double Double

chaosGame :: RandomGen g => g -> Int -> Array Int (Point -> Point) -> [Point]
chaosGame g n hutchinson = take n points
  where
    (x, g') = random g
    (y, g'') = random g'
    choices = randomRs (bounds hutchinson) g''
    points = Point x y : zipWith (hutchinson !) choices points

main :: IO ()
main = do
  g <- newStdGen

  let midPoint (Point a b) (Point x y) = Point ((a + x) / 2) ((b + y) / 2)
      sierpinski =
        listArray
          (1, 3)
          [ midPoint (Point 0 0),
            midPoint (Point 0.5 (sqrt 0.75)),
            midPoint (Point 1 0)
          ]
      points = chaosGame g 10000 sierpinski
      showPoint (Point x y) = show x ++ "\t" ++ show y

  writeFile "sierpinski.dat" $ intercalate "\n" $ map showPoint points
