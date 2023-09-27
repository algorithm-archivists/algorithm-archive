data Mode = Servois | Easter

computus :: Mode -> Int -> String
computus mode year =
  case mode of
    Servois ->
      -- Value for Servois' table
      show $ (21 + d) `mod` 31
    Easter ->
      -- Determination of the correct month for Easter
      if 22 + d + f > 31
        then "April " ++ show (d + f - 9)
        else "March " ++ show (22 + d + f)
  where
    a, b, c, d, e, f, k, m, n, p, q :: Int
    -- Year's position on the 19 year metonic cycle
    a = year `mod` 19
    -- Century index
    k = year `div` 100
    -- Shift of metonic cycle, add a day offset every 300 years
    p = (13 + 8 * k) `div` 25
    -- Correction for non-observed leap days
    q = k `div` 4
    -- Correction to starting point of calculation each century
    m = (15 - p + k - q) `mod` 30
    -- Number of days from March 21st until the full moon
    d = (19 * a + m) `mod` 30
    -- Finding the next Sunday
    -- Century-based offset in weekly calculation
    n = (4 + k - q) `mod` 7
    -- Correction for leap days
    b = year `mod` 4
    c = year `mod` 7
    -- Days from d to next Sunday
    e = (2 * b + 4 * c + 6 * d + n) `mod` 7
    -- Historical corrections for April 26 and 25
    f =
      if (d == 29 && e == 6) || (d == 28 && e == 6 && a > 10)
        then -1
        else e

-- Here, we will output the date of the Paschal full moon
-- (using Servois notation), and Easter for 2020-2030
main :: IO ()
main = do
  let years :: [Int]
      years = [2020 .. 2030]
      servoisNumbers, easterDates :: [String]
      servoisNumbers = map (computus Servois) years
      easterDates = map (computus Easter) years
  putStrLn "The following are the dates of the Paschal full moon (using Servois notation) and the date of Easter for 2020-2030 AD:"
  putStrLn "Year\tServois number\tEaster"
  let conc :: Int -> String -> String -> String
      conc y s e = show y ++ "\t" ++ s ++ "\t\t" ++ e
  mapM_ putStrLn $ zipWith3 conc years servoisNumbers easterDates
