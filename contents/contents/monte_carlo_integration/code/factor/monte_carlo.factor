USING: locals random math.ranges math.functions ;

:: monte-carlo ( n in-shape?: ( x y -- ? ) -- % )
  n <iota> [ drop random-unit random-unit in-shape? call ] count n /
; inline

! Use the monte-carlo approximation to calculate pi
: monte-carlo-pi ( n -- pi-approx )
  [ ! in-circle check
    [ 2 ^ ] bi@ + ! get the distance from the center
    1 <           ! see if it's less than the radius
  ]
  monte-carlo 4 * >float
;

USING: math.constants ;
10000000 monte-carlo-pi ! Approximate pi
dup .                   ! Print the approximation
pi - pi / 100 * >float abs .  ! And the error margin


