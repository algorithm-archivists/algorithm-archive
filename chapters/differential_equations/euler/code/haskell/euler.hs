solveEuler :: Double -> Int -> [Double]
solveEuler timestep n = take n $ iterate f 1
    where
        f x = x - 3 * x * timestep

checkResult :: [Double] -> Double -> Double -> Bool
checkResult results threshold timestep =
    and $ zipWith check' results [exp $ -3 * i * timestep | i <- [0 ..]]
    where
        check' result solution = abs (result - solution) < threshold

main :: IO ()
main =
    let timestep = 0.01
        n = 1
        threshold = 0.01
    in  putStrLn $
        if checkResult (solveEuler timestep n) threshold timestep
        then "All values within threshold"
        else "Value(s) not in threshold"
