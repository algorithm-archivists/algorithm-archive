#!/usr/bin/env perl
use strict;
use warnings;

sub euclid_sub {
    my ($a , $b) = @_ ;
    $a = abs( $a );
    $b = abs( $b );

    while ( $a != $b ) {
        if ( $a > $b ) {
            $a = $a - $b;
        } else {
            $b = $b - $a;
        }
    }

    return $a;
}

sub euclid_mod {
    my ($a , $b) = @_;
    $a = abs( $a );
    $b = abs( $b );

    while ( $b != 0 ) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }

    return $a;
}

sub main {
    $check1 = euclid_mod(64*67,64*81);
    $check2 = euclid_sub(128*12,128*77);
    print $check1."\n";
    print $check2."\n";
}

main()
