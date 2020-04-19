#!/usr/bin/env bash
abs() {
    local ret=$1
    if (( ret < 0 )); then
        ((ret *= -1))
    fi
    printf "%s" "$ret"
} 

euclid_mod() {
    local a
    local b
    a=$(abs "$1")
    b=$(abs "$2")
    
    while (( b != 0 )); do
        ((tmp = b))
        ((b = a % b))
        ((a = tmp))
    done
    printf "%s" "$a"
}

euclid_sub() {
    local a
    local b
    a=$(abs "$1")
    b=$(abs "$2")
    
    while (( a != b )); do
        if (( a > b )); then
            ((a -= b))
        else
            ((b -= a))
        fi
    done
    printf "%s" "$a"
}

result=$(euclid_mod $((64 * 67)) $((64 * 81)))
echo "$result"
result=$(euclid_sub $((128 * 12)) $((128 * 77)))
echo "$result"
