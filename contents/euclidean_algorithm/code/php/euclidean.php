<?php
declare(strict_types=1);

function euclid_sub(float $a, float $b): float
{
  $a = abs($a);
  $b = abs($b);

  while ($a !== $b) {
    if ($a > $b) $a = $a - $b;
    else $b = $b - $a;
  }

  return $a;
}

function euclid_mod(float $a, float $b): float
{
  $a = abs($a);
  $b = abs($b);

  while ($b != 0)
    list($b, $a) = [$a % $b, $b];

  return $a;
}

echo sprintf('Euclidean mod: %s', euclid_mod(64 * 67, 64 * 81));
echo PHP_EOL;
echo sprintf('Euclidean sub: %s', euclid_sub(128 * 12, 128 * 77));