solveEuler :: Num a => (a -> a) -> a -> a -> [a]
solveEuler f ts = iterate (\x -> x + f x * ts)

checkResult :: (Ord a, Num a, Num t, Enum t) => a -> (t -> a) -> [a] -> Bool
checkResult thresh check =
    and . zipWith (\i k -> abs (check i - k) < thresh) [0..]

kinematics :: Double -> Double
kinematics x = -3 * x

main :: IO ()
main =
    let timestep = 0.01
        n = 100
        threshold = 0.01
        checkResult' = checkResult threshold $ exp . (\x -> -3 * x * timestep)
    in  putStrLn $
        if checkResult' (take n $ solveEuler kinematics timestep 1)
        then "All values within threshold"
        else "Value(s) not in threshold"
