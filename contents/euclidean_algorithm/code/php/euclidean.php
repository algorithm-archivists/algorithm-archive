<?php
declare(strict_types=1);

function euclid_sub(int $a, int $b): int
{
    $a = abs($a);
    $b = abs($b);

    while ($a !== $b) {
        if ($a > $b) {
            $a = $a - $b;
        } else {
            $b = $b - $a;
        }
    }

    return $a;
}

function euclid_mod(int $a, int $b): int
{
    $a = abs($a);
    $b = abs($b);

    while ($b !== 0) {
        list($b, $a) = [$a % $b, $b];
    }

    return $a;
}

printf('Euclidean mod: %s', euclid_mod(64 * 67, 64 * 81));
echo PHP_EOL;
printf('Euclidean sub: %s', euclid_sub(128 * 12, 128 * 77));
echo PHP_EOL;
