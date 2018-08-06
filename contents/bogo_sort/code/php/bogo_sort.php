<?php

function is_sorted(array $array): bool
{
  for ($i = 0, $count = count($array); $i < $count - 1; $i++)
    if ($array[$i] < $array[$i + 1]) return false;

  return true;
}

function bogo_sort(array $array): array
{
  if (!is_sorted($array)) shuffle($array);

  return $array;
}


$unsorted = [1, 2, 3, 4, 5, 6, 8, 7, 9, 10];
$bogo_sorted = bogo_sort($unsorted);

echo sprintf('Unsorted: %s%s', implode(',', $unsorted), PHP_EOL);
echo sprintf('Sorted: %s%s', implode(',', $bogo_sorted), PHP_EOL);
