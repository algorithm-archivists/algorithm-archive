: euclid- ( a b -- gcd )
 [ 2dup = ]
 [
  ! make sure the lower number is deeper
  2dup >= [ swap ] when ! a b -> G(reater) L(ess)
  over - ! leaves us with L G-L
 ]
 until
 ! we have the GCD twice now, drop one
 drop ;

: euclid% ( a b -- gcd )
 [ dup zero? ]
 [
  ! a b -> a b b -> b a b -> b a%b
  dup -rot mod
 ]
 until
 ! the zero is on top, so get rid of it
 drop ;

42 56 euclid% . ! 14
48 180 euclid% . ! 12

42 56 euclid- . ! 14
48 180 euclid- . ! 12

