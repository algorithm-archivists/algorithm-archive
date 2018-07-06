solveEuler :: Num a => (a -> a) -> a -> [a]
solveEuler = iterate

checkResult :: (Ord a, Num a, Num t, Enum t) => a -> (t -> a) -> [a] -> Bool
checkResult thresh check =
    and . zipWith (\i k -> abs (check i - k) < thresh) [0..]

kinematics :: Double -> Double -> Double
kinematics timestep x = x - 3 * x * timestep

main :: IO ()
main =
    let timestep = 0.01
        n = 100
        threshold = 0.01
        kinematics' = kinematics timestep
        checkResult' = checkResult threshold $ exp . kinematics'
    in  putStrLn $
        if checkResult' (take n $ solveEuler kinematics' 1)
        then "All values within threshold"
        else "Value(s) not in threshold"
