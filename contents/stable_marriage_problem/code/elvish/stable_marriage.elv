use str

# function to shuffle a list
# `shuf` is available in the GNU coreutils (most linux distributions)
fn shuffle [list]{
  put [(echo (str:join "\r\n" $list) | shuf)]
}

# switch the index with the value to produce a map
fn switch-index [list]{
  map = [&]
  i = 0
  n = (count $list)

  while (< $i $n) {
    map[$list[$i]] = $i
    i = (+ $i 1)
  }

  put $map
}

# do nothing
fn return-same [thing]{ put $thing }

# pref format for females: [&name=[&name=number ..] ..] for O(1) lookup
# pref format for males: [&name=[name1 name2 ..] ..] because it's used like a stack
fn gen-pref [names1 names2 &switch-idx=0]{
  map = [&]

  fn = [thing]{ return-same $thing }
  if (== $switch-idx 1) {
    fn = [list]{ switch-index $list }
  }

  each [name]{ map[$name] = ($fn (shuffle $names2)) } $names1

  put $map
}

# use `put` on elements that satisfy the condition
fn take-if [cond list]{
  for elem $list {
    if ($cond $elem) {
      put $elem
    }
  }
}

# find out if list is empty, `count` can be used as well
# but this function should be faster
fn is-empty [list]{
  for elem $list {
    put $false
    return
  }

  put $true
}

# select the best candidate out of the males in $propos
fn select-best [pref propos]{
  if (is-empty $propos) {
    put $nil
    return
  }

  best = $propos[0]

  for elem $propos {
    if (> $pref[$best] $pref[$elem]) {
      best = $elem
    }
  }

  put $best
}

fn gale-shapley [men-pref fem-pref]{
  exit = 0

  fem-names = [(keys $fem-pref)]
  men-names = [(keys $men-pref)]

  # this is defined here because the take-if line would be very long otherwise
  proposal-cond = [elem female]{
    put (and (==s $men-pref[$elem][0] $female ) (!=s (kind-of $men-pref[$elem]) string))
  }

  while (!= $exit 1) {
    exit = 1
    for female $fem-names {
      # choose men who have $female as their first choice
      proposals = [(take-if [elem]{ $proposal-cond $elem $female } $men-names)]

      # there's no reason to continue this loop if $proposals is empty
      # and if we didn't find any proposals, the algorithm should end
      if (is-empty $proposals) {
        continue
      } else {
        exit = 0
      }

      # if $female already has a pair, $fem-pref[$female] is of type string
      if (==s (kind-of $fem-pref[$female]) string) {
        for elem $proposals {
          men-pref[$elem] = $men-pref[$elem][1..]
        }

        continue
      }

      # choose the best choice accordng to the $fem-pref[$female] map
      best = (select-best $fem-pref[$female] $proposals)
      echo 'for $female, chosen $best'
      fem-pref[$female] = $best
      men-pref[$best] = $female

      # eliminate those who failed
      for elem $proposals {
        if (!=s $elem $best) {
          men-pref[$elem] = $men-pref[$elem][1..]
        }
      }
    }
  }

  put $men-pref $fem-pref
}

men-names = [a b c d]
fem-names = [e f g h]

men-pref = (gen-pref $men-names $fem-names)
fem-pref = (gen-pref $fem-names $men-names &switch-idx=1)

echo 'Generated preferences:'
put $men-pref
pprint $fem-pref

gale-shapley $men-pref $fem-pref
