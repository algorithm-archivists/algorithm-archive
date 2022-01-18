#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// This function returns a pseudo-random number between 0 and 1
double drand()
{
    return (double)rand() / RAND_MAX;
}

// This function takes
//  - v: value in register
//  - a: a scaling value for the logarithm based on Morris's paper
// It returns the approximate count
double n(double v, double a)
{
    return a * (pow(1 + 1 / a, v) - 1);
}

// This function takes
//  - v: value in register
//  - a: a scaling value for the logarithm based on Morris's paper
// It returns a new value for v
double increment(double v, double a)
{
    // delta is the probability of incrementing our counter
    double delta = 1 / (n(v + 1, a) - n(v, a));

    if (drand() <= delta) {
        return v + 1;
    }
    return v;
}

// This function simulates counting and takes
//  - n_items: number of items to count and loop over
//  - a: a scaling value for the logarithm based on Morris's paper
// It returns n(v, a), the approximate count
double approximate_count(size_t n_items, double a)
{
    double v = 0;
    for (size_t i = 0; i < n_items; ++i) {
        v = increment(v, a);
    }

    return n(v, a);
}

// This function takes
//  - n_trials: the number of counting trials
//  - n_items: the number off items to count
//  - a: a scaling value for the logarithm based on Morris's paper
//  - threshold: the maximum percent error allowed
// It terminates the program on failure
void test_approximation_count(size_t n_trials, size_t n_items, double a,
                              double threshold)
{
    double sum = 0.0;
    for (size_t i = 0; i < n_trials; ++i) {
        sum += approximate_count(n_items, a);
    }
    double avg = sum / (double)n_trials;

    double items = (double)n_items;
    if (fabs((avg - items) / items) < threshold){
        printf("passed\n");
    }
    else{
        printf("failed\n");
    }
}

int main()
{
    srand((unsigned int)time(NULL));

    printf("[#]\nCounting Tests, 100 trials\n");
    printf("[#]\ntesting 1,000, a = 30, 10%% error\n");
    test_approximation_count(100, 1000, 30, 0.1);
    printf("[#]\ntesting 12,345, a = 10, 10%% error\n");
    test_approximation_count(100, 12345, 10, 0.1);
    printf("[#]\ntesting 222,222, a = 0.5, 20%% error\n");
    test_approximation_count(100, 222222, 0.5, 0.2);

    return 0;
}
