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

