<?php
declare(strict_types=1);

function in_circle(float $positionX, float $positionY, float $radius = 1): bool
{
    return pow($positionX, 2) + pow($positionY, 2) < pow($radius, 2);
}

function random_zero_to_one(): float
{
    return mt_rand() / mt_getrandmax();
}

function monte_carlo(int $samples, float $radius = 1): float
{
    $inCircleCount = 0;

    for ($i = 0; $i < $samples; $i++) {
        if (in_circle(random_zero_to_one() * $radius, random_zero_to_one() * $radius, $radius)) {
            $inCircleCount++;
        }
    }

    return 4 * $inCircleCount / $samples;
}

$piEstimate = monte_carlo(10000000);
$percentError = abs($piEstimate - pi()) / pi() * 100;

printf('The estimate of PI is: %s', $piEstimate);
echo PHP_EOL;
printf('The percent error is: %s', $percentError);
echo PHP_EOL;
