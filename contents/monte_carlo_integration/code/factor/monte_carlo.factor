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

USING: formatting calendar ;
! Add timing, so we can see how long things take.
:: time-quot ( quot -- quot' )
  [ now quot dip now swap time- duration>milliseconds "Took %dms\n" printf ]
; inline

! Actually calculate pi to 5 levels of precision.
5 <iota> [
  10 swap ^ dup "%d iterations\n" printf monte-carlo-pi "pi: %f\n" printf
] time-quot each

