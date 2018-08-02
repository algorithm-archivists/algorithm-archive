<?php

function bubble_sort(array $arr): array
{
  for ($i = 0, $length = count($arr); $i < $length; $i++) {
    for ($j = 1; $j < $length; $j++) {
      if ($arr[$j - 1] > $arr[$j]) {
        $tmp = $arr[$j - 1];
        $arr[$j - 1] = $arr[$j];
        $arr[$j] = $tmp;
      }
    }
  }
  return $arr;
}

$unsorted = [1, 2, 6, 47, 4, 9, 3, 7, 8, 23, 15];
$bubble_sorted = bubble_sort($unsorted);

echo sprintf('Unsorted: %s%s', implode(',', $unsorted), PHP_EOL);
echo sprintf('Sorted: %s%s', implode(',', $bubble_sorted), PHP_EOL);
