<?php
declare(strict_types=1);

function is_sorted(array $array): bool
{
    for ($i = 0, $count = count($array); $i < $count - 1; $i++) {
        if ($array[$i] > $array[$i + 1]) {
            return false;
        }
    }

    return true;
}

function bogo_sort(array $array): array
{
    while (!is_sorted($array)) {
        shuffle($array);
    }

    return $array;
}


$unsorted = [10, 7, 3, 1, 4, 8, 5, 6, 9, 2];
$bogo_sorted = bogo_sort($unsorted);

printf('Unsorted: %s', implode(',', $unsorted));
echo PHP_EOL;
printf('Sorted: %s', implode(',', $bogo_sorted));
echo PHP_EOL;
