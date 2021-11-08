using Test 

# This function takes 
#     - v: value in register
#     - a: a  scaling value for the logarithm based on Morris's paper
# It returns n(v,a), the approximate count
function n(v, a)
    a*((1+1/a)^v-1)  
end

# This function takes
#    - v: value in register
#    - a: a scaling value for the logarithm based on Morris's paper
# It returns a new value for v
function increment(v, a)
    # delta is the probability of incrementing our counter
    delta = 1/(n(v+1, a)-n(v, a))

    if rand() <= delta
        return v + 1
    else
        return v
    end
end

# This simulates counting and takes
#     - n_items: number of items to count and loop over
#     - a: a scaling value for the logarithm based on Morris's paper
# It returns n(v,a), the approximate count
function approximate_count(n_items, a)
    v = 0
    for i = 1:n_items
        v = increment(v, a)
    end

    return n(v, a)
end

# This function takes
#     - n_trials: the number of counting trials
#     - n_items: the number of items to count to
#     - a: a scaling value for the logarithm based on Morris's paper
#     - threshold: the maximum percent error allowed
# It returns a true / false test value
function test_approximate_count(n_trials, n_items, a, threshold)
    samples = [approximate_count(n_items, a) for i = 1:n_trials]

    avg = sum(samples)/n_trials

    if (abs((avg - n_items) / n_items) < threshold)
        println("passed")
    else
        println("failed")
    end
end

println("[#]\nCounting Tests, 100 trials")

println("[#]\ntesting 1,000, a = 30, 10% error")
test_approximate_count(100, 1000, 30, 0.1)

println("[#]\ntesting 12,345, a = 10, 10% error")
test_approximate_count(100, 12345, 10, 0.1)

# Note: with a lower a, we need more trials, so a higher % error here.
println("[#]\ntesting 222,222, a = 0.5, 20% error")
test_approximate_count(100, 222222, 0.5, 0.2)
