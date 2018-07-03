import Data.Array
import Data.List (intercalate)

type Matrix a = Array (Int, Int) a

cols :: Matrix a -> [Int]
cols m =
  let ((_, c1), (_, cn)) = bounds m
   in [c1 .. cn]

swapRows :: Int -> Int -> Matrix a -> Matrix a
swapRows r1 r2 m
  | r1 == r2 = m
  | otherwise =
    m // concat [[((r2, c), m ! (r1, c)), ((r1, c), m ! (r2, c))] | c <- cols m]

multRow :: (Num a) => Int -> a -> Matrix a -> Matrix a
multRow r a m = m // [((r, k), a * m ! (r, k)) | k <- cols m]

combRows ::
     (Eq a, Fractional a) => (Int, Int) -> a -> Int -> Matrix a -> Matrix a
combRows (r, c) a t m
  | m ! (t, c) == 0 = m
  | otherwise =
    m // [((t, k), a * m ! (t, k) / (m ! (t, c)) - m ! (r, k)) | k <- cols m]

toEchelon :: (Ord a, Fractional a) => Matrix a -> Matrix a
toEchelon mat = go (r1, c1) mat
  where
    ((r1, c1), (rn, cn)) = bounds mat
    go (r, c) m
      | c == cn = m
      | pivot == 0 = go (r, c + 1) m
      | otherwise =
        go (r + 1, c + 1) $
        foldr (combRows (r, c) pivot) (swapRows r target m) [r + 1 .. rn]
      where
        (pivot, target) = maximum [(m ! (k, c), k) | k <- [r .. rn]]

toReducedEchelon :: (Fractional a, Eq a) => Matrix a -> Matrix a
toReducedEchelon mat = foldr go mat (echelonPath (r1, c1))
  where
    ((r1, c1), (rn, cn)) = bounds mat
    echelonPath (r, c)
      | r > rn || c >= cn = []
      | mat ! (r, c) == 0 = echelonPath (r, c + 1)
      | otherwise = (r, c) : echelonPath (r + 1, c + 1)
    go (r, c) m =
      foldr (combRows (r, c) 1) (multRow r (1 / (m ! (r, c))) m) [r1 .. r - 1]

printM :: (Show a) => Matrix a -> String
printM m =
  let ((r1, c1), (rn, cn)) = bounds m
   in unlines
        [ intercalate "\t" [take 7 $ show $ m ! (r, c) | c <- [c1 .. cn]]
        | r <- [r1 .. rn]
        ]

main :: IO ()
main = do
  let m = listArray ((1, 1), (3, 4)) [2, 3, 4, 6, 1, 2, 3, 4, 3, -4, 0, 10]
  putStrLn "Original Matrix:"
  putStrLn $ printM m
  putStrLn "Echelon form"
  putStrLn $ printM $ toEchelon m
  putStrLn "Reduced echelon form"
  putStrLn $ printM $ toReducedEchelon $ toEchelon m
