<?php
declare(strict_types=1);

function in_circle(float $position_x, float $position_y, float $radius = 1): bool
{
    return pow($position_x 2) + pow($position_y, 2) < pow($radius, 2);
}

function random_zero_to_one(): float
{
    return mt_rand() / mt_getrandmax();
}

function monte_carlo(int $samples, float $radius = 1): float
{
    $in_circle_count = 0;

    for ($i = 0; $i < $samples; $i++) {
        if (in_circle(random_zero_to_one() * $radius, random_zero_to_one() * $radius, $radius))
            $in_circle_count++;
    }

    return 4 * $in_circle_count / $samples;
}

$piEstimate = monte_carlo(100000000);
$percentError = abs($piEstimate - pi()) / pi() * 100;

printf('The estimate of PI is: %s', $piEstimate);
echo PHP_EOL;
printf('The percent error is: %s', $percentError);
