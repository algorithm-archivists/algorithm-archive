! There's no built-in "is sorted" function, so let's make one:
USING: locals ;
: sorted? ( seq -- ? )
  2 clump ! split it up into overlapping pairs
  ! so now, for example, { 1 2 3 } has turned into { { 1 2 } { 2 3 } }
  ! and now we make sure that for every pair, the latter is >= the former
  [| pair | pair first pair last <= ] all?
;

USING: random ;
: bogosort ( seq -- seq' )
  ! `dup` duplicates the array, because `sorted?` pops its reference to it
  ! randomize works in-place
  ! so we `randomize` `until` it's `sorted?`
  [ dup sorted? ] [ randomize ] until
;

! WARNING: Increasing this number beyond 5 or so will make this take a very long time.
!          That said, if you have an afternoon to kill...
5 <iota> >array randomize ! generate a random array to demo
dup .                     ! show the array
bogosort                  ! bogosort it
.                         ! show it again


