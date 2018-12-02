#!/usr/bin/env perl

use strict;
use warnings;

sub bubble_sort {
    my $n = scalar(@_); # Number of elements.
    our @arr;
    local *arr = \@_; # Alias to @_


    foreach my $i (0..$n-1) {
        foreach my $j (0..$n-$i-2) {
            if ($arr[$j]>$arr[$j+1]) {
                ($arr[$j], $arr[$j+1]) = ($arr[$j+1], $arr[$j]);
            }
        }
    }
}


sub main {
    my @a;
    foreach my $i (1..10) {
        push(@a, int rand(10000));
    }

    print "Before Sorting: ".join(' ,',@a)."\n";
    bubble_sort (@a);
    print "After Sorting: ".join(' ,',@a)."\n";
}

main()

