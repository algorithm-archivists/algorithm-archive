solveEuler :: Num a => (a -> a) -> a -> Int -> [a]
solveEuler func initVal n = take n $ iterate func initVal

checkResult :: (Num a, Ord a, Enum a) => [a] -> (a -> a) -> a -> Bool
checkResult results checkFunc threshold =
    and $ zipWith check' results (fmap checkFunc [0 ..])
    where
        check' result solution = abs (result - solution) < threshold

kinematics :: Double -> Double -> Double
kinematics timestep x = x - 3 * x * timestep

main :: IO ()
main =
    let timestep = 0.01
        n = 100
        threshold = 0.01
        kinematics' = kinematics timestep
        checkFunc'  = exp . kinematics'
    in  putStrLn $
        if checkResult (solveEuler kinematics' 1 n) checkFunc' threshold
        then "All values within threshold"
        else "Value(s) not in threshold"
