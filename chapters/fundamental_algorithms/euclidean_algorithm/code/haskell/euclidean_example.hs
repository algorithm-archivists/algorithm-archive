-- contributed by Nicole Mazzuca (ubsan)

euclidSub :: Integer -> Integer -> Integer
euclidSub a b = inner (abs a) (abs b) where
  inner a b =
    if a == b then
      a
    else if a < b then
      euclidSub a (b - a)
    else
      euclidSub (a - b) b

euclidMod :: Integer -> Integer -> Integer
euclidMod a b = inner (abs a) (abs b) where
  inner a 0 = a
  inner a b = inner b (a `mod` b)

main :: IO ()
main = do
  let chk1 = euclidMod (64 * 67) (64 * 81)
      chk2 = euclidSub (128 * 12) (128 * 77)
  putStrLn (show chk1)
  putStrLn (show chk2)
  return ()

