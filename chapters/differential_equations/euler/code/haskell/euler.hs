solve_euler timestep n = take n $ iterate f 1
    where f x = x - 3 * x * timestep

check_result results threshold timestep =
    and $ zipWith check' results [exp $ -3 * i * timestep | i <- [0..]]
    where check' result solution = abs (result - solution) < threshold

main = let timestep = 0.01;
           n = 1;
           threshold = 0.01;
        in putStrLn $ if check_result (solve_euler timestep n) threshold timestep
                      then "All values within threshold"
                      else "Value(s) not in threshold"
