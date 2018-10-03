:: monte-carlo ( xr yr n in-shape?: ( x y -- b ) -- % )
  n <iota> [ drop xr random yr random in-shape? call ] map [ ] count n / ; inline

! Calculating the area of a square in the first quadrant
-100 100 [a,b] dup   ! two input ranges
1000                 ! number of iterations
[ 0 < swap 0 < and ] ! in-shape test (can be anything that takes two points) -- this one is a square.
monte-carlo          ! run the Monte Carlo integration
100 * >float .       ! prettyprint the results as a percentage

! Calculate the ratio of circle-area to square-area
-10000 >bignum 10000 >bignum [a,b] dup ! big ranges to get more precision
10000                  ! lots of iterations for, again, more precision
[ ! in-circle check
  2 ^ swap 2 ^ + sqrt ! get the distance from the center
  10000 <             ! see if it's less than the radius
]
monte-carlo
100 * >float .

