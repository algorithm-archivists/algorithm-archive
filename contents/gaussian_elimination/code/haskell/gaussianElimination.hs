import Data.Array
import Data.Function (on)
import Data.List (intercalate, maximumBy)
import Data.Ratio

type Matrix a = Array (Int, Int) a

type Vector a = Array Int a

swapRows :: Int -> Int -> Matrix a -> Matrix a
swapRows r1 r2 m
  | r1 == r2 = m
  | otherwise =
    m //
    concat [[((r2, c), m ! (r1, c)), ((r1, c), m ! (r2, c))] | c <- [c1 .. cn]]
  where
    ((_, c1), (_, cn)) = bounds m

subRows ::
     Fractional a
  => (Int, Int) -- pivot location
  -> (Int, Int) -- rows to cover
  -> (Int, Int) -- columns to cover
  -> Matrix a
  -> Matrix a
subRows (r, c) (r1, rn) (c1, cn) m =
  accum
    (-)
    m
    [ ((i, j), m ! (i, c) * m ! (r, j) / m ! (r, c))
    | i <- [r1 .. rn]
    , j <- [c1 .. cn]
    ]

gaussianElimination :: (Fractional a, Ord a) => Matrix a -> Matrix a
gaussianElimination mat = go (r1, c1) mat
  where
    ((r1, c1), (rn, cn)) = bounds mat
    go (r, c) m
      | c == cn = m
      | pivot == 0 = go (r, c + 1) m
      | otherwise = go (r + 1, c + 1) $ subRows (r, c) (r + 1, rn) (c, cn) m'
      where
        (target, pivot) =
          maximumBy (compare `on` abs . snd) [(k, m ! (k, c)) | k <- [r .. rn]]
        m' = swapRows r target m

gaussJordan :: (Fractional a, Eq a) => Matrix a -> Matrix a
gaussJordan mat = go (r1, c1) mat
  where
    ((r1, c1), (rn, cn)) = bounds mat
    go (r, c) m
      | c == cn = m
      | m ! (r, c) == 0 = go (r, c + 1) m
      | otherwise = go (r + 1, c + 1) $ subRows (r, c) (r1, r - 1) (c, cn) m'
      where
        m' = accum (/) m [((r, j), m ! (r, c)) | j <- [c .. cn]]

backSubstitution :: (Fractional a) => Matrix a -> Vector a
backSubstitution m = sol
  where
    ((r1, _), (rn, cn)) = bounds m
    sol =
      listArray (r1, rn) [(m ! (r, cn) - sum' r) / m ! (r, r) | r <- [r1 .. rn]]
    sum' r = sum [m ! (r, k) * sol ! k | k <- [r + 1 .. rn]]

printM :: (Show a) => Matrix a -> String
printM m =
  let ((r1, c1), (rn, cn)) = bounds m
   in unlines
        [ intercalate "\t" [show $ m ! (r, c) | c <- [c1 .. cn]]
        | r <- [r1 .. rn]
        ]

printV :: (Show a) => Vector a -> String
printV = unlines . map show . elems

main :: IO ()
main = do
  let mat = [2, 3, 4, 6, 1, 2, 3, 4, 3, -4, 0, 10] :: [Ratio Int]
      m = listArray ((1, 1), (3, 4)) mat
  putStrLn "Original Matrix:"
  putStrLn $ printM m
  putStrLn "Echelon form"
  putStrLn $ printM $ gaussianElimination m
  putStrLn "Reduced echelon form"
  putStrLn $ printM $ gaussJordan $ gaussianElimination m
  putStrLn "Solution from back substitution"
  putStrLn $ printV $ backSubstitution $ gaussianElimination m
