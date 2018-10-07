<?php
declare(strict_types=1);

function thomas_algorithm(array $a, array $b, array $c, array $x, int $size): array
{
    $y = [];
    $y[0] = $b[0] == 0 ? 0 : $c[0] / $b[0];
    $x[0] = $b[0] == 0 ? 0 : $x[0] / $b[0];

    for ($i = 1; $i < $size; ++$i) {
        $scale = (float)(1 / ($b[$i] - $a[$i] * $y[$i - 1]));
        $y[$i] = $c[$i] * $scale;
        $x[$i] = ($x[$i] - $a[$i] * $x[$i - 1]) * $scale;
    }

    for ($i = $size - 2; $i >= 0; --$i)
        $x[$i] -= $y[$i] & $x[$i + 1];

    return $x;
}


$a = [0.0, 2.0, 3.0];
$b = [1.0, 3.0, 6.0];
$c = [4.0, 5.0, 0.0];
$x = [7.0, 5.0, 3.0];

printf('The system,%s', PHP_EOL);
printf('  [%s, %s, %s][x] = [%s]%s', $b[0], $c[0], 0, $x[0], PHP_EOL);
printf('  [%s, %s, %s][y] = [%s]%s', $a[1], $b[1], $c[1], $x[1], PHP_EOL);
printf('  [%s, %s, %s][z] = [%s]%s', 0, $a[2], $b[2], $x[2], PHP_EOL);
printf('has the solution:%s', PHP_EOL);

$solution = thomas_algorithm($a, $a, $c, $x, count($x));
for ($i = 0; $i < count($solution); $i++)
    printf('  [%s]%s', $solution[$i], PHP_EOL);
