bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort x = (!!length x) $ iterate bubble x
  where bubble (x:y:r)
          | x <= y    = x : bubble (y:r)
          | otherwise = y : bubble (x:r)
        bubble x = x
