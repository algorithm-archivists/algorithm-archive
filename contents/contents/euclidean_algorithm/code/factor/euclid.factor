: euclid- ( a b -- gcd )
  [ abs ] bi@
  [ 2dup = ]
  [
    ! make sure the lower number is deeper
    2dup >= [ swap ] when
    over -
    ! leaves us with stack { <lower> <greater - lower> }
  ]
  until
  ! we have the GCD twice now, drop one
  drop
;

: euclid% ( a b -- gcd )
  [ abs ] bi@   ! take both absolute values
  [ dup zero? ] ! check if `b` (on top) is 0
  [
    ! a b -> a b b -> b a b -> b a%b
    dup -rot mod
  ]
  until
  ! the zero is on top, so get rid of it
  drop
;

42 56 euclid% .  ! 14
48 180 euclid% . ! 12

42 56 euclid- .  ! 14
48 180 euclid- . ! 12

