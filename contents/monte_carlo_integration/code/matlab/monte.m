pi_estimate = monte_carlo(10000000);

fprintf("The pi estimate is: %f\n", pi_estimate);
fprintf("Percent error is: %f%%\n", 100 * abs(pi_estimate - pi) / pi);

function pi_estimate=monte_carlo(n)

    % a 2 by n array, rows are xs and ys
    xy_array = rand(2, n);   

    % square every element in the array
    squares_array = xy_array.^2;            
    
    % sum the xs and ys and check if it's in the quarter circle
    incircle_array = sum(squares_array)<1;  

    % determine the average number of points in the circle
    pi_estimate = 4*sum(incircle_array)/n;  
    
end

