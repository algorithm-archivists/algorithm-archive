import System.Random

monteCarloPi :: RandomGen g => g -> Int -> Float
monteCarloPi g n = count $ filter inCircle $ makePairs
  where makePairs = take n $ toPair (randomRs (0, 1) g :: [Float])
        toPair (x:y:rest) = (x, y) : toPair rest
        inCircle (x, y) = x^2 + y^2 < 1
        count l = 4 * fromIntegral (length l) / fromIntegral n

main = do
  g <- newStdGen
  let p = monteCarloPi g 100000
  putStrLn $ "Estimated pi: " ++ show p
  putStrLn $ "Percent error: " ++ show (100 * abs (pi - p) / pi)
