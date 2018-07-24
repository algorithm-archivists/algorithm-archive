-- contributed by Nicole Mazzuca (ubsan)
euclidSub :: Integer -> Integer -> Integer
euclidSub a b = inner (abs a) (abs b)
  where
    inner x y
      | x == y = x
      | x < y = euclidSub x (y - x)
      | otherwise = euclidSub (x - y) y

euclidMod :: Integer -> Integer -> Integer
euclidMod a b = inner (abs a) (abs b)
  where
    inner x 0 = x
    inner x y = inner y (x `mod` y)

main :: IO ()
main = do
  let chk1 = euclidMod (64 * 67) (64 * 81)
      chk2 = euclidSub (128 * 12) (128 * 77)
  print chk1
  print chk2
