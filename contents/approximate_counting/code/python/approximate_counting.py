from random import random

# This function takes
#   - v: value in register
#   - a: a scaling value for the logarithm based on Morris's paper
# It returns n(v,a), the approximate_count
def n(v, a):
    return a*((1 + 1/a)**v - 1)

# This function takes
#    - v: value in register
#    - a: a scaling value for the logarithm based on Morris's paper
# It returns a new value for v
def increment(v, a):
    delta = 1/(n(v + 1, a) - n(v, a))
    if random() <= delta:
        return v + 1
    else:
        return v

#This simulates counting and takes
#     - n_items: number of items to count and loop over
#     - a: a scaling value for the logarithm based on Morris's paper
# It returns n(v,a), the approximate count
def approximate_count(n_items, a):
    v = 0
    for i in range(1, n_items + 1):
        v = increment(v, a)
    return n(v, a)

# This function takes
#     - n_trials: the number of counting trials
#     - n_items: the number of items to count to
#     - a: a scaling value for the logarithm based on Morris's paper
#     - threshold: the maximum percent error allowed
# It returns a true / false test value
def test_approximate_count(n_trials, n_items, a, threshold):
    samples = [approximate_count(n_items, a) for i in range(1, n_trials + 1)]
    avg = sum(samples)/n_trials

    if abs((avg - n_items)/n_items) < threshold:
        print("passed")
    else:
        print("failed")

print("[#]\nCounting Tests, 100 trials")
print("[#]\ntesting 1,000, a = 30, 10% error")
test_approximate_count(100, 1000, 30, 0.1)
print("[#]\ntesting 12,345, a = 10, 10% error")
test_approximate_count(100, 12345, 10, 0.1)
print("[#]\ntesting 222,222, a = 0.5, 20% error")
test_approximate_count(100, 222222, 0.5, 0.2)
