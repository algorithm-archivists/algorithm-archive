#!/usr/bin/env bash
is_sorted() {
    local arr
    local len
    local i
    local sorted
    arr=("$@")
    (( len = ${#arr[@]} - 1))
    (( sorted = 1 ))
    
    for (( i = len; i > 0; i-- )); do
        if (( arr[i] < arr[(( i - 1 ))] )); then
            (( sorted = 0 ))
            break
        fi
    done
    printf "%d" $sorted
}

shuffle() {
    local arr
    local len
    local i
    local tmp
    local rand
    arr=("$@")
    (( len = ${#arr[@]} ))
    
    for (( i = 0; i < len; i++ )); do
        (( rand = RANDOM % len ))
        (( tmp = arr[rand] ))
        (( arr[rand] = arr[i] ))
        (( arr[i] = tmp ))
    done
    echo ${arr[*]}
}

bogo_sort() {
    local arr
    arr=("$@")
    while [[ $(is_sorted "${arr[@]}") == 0 ]]; do
        arr=($(shuffle "${arr[@]}"))
    done
    echo ${arr[*]}
}

arr=(1 3654 78 654 -234 -12 4 3 -6 -100)
echo "Unsorted array: ${arr[*]}"
arr=("$(bogo_sort "${arr[@]}")")
echo "Sorted array: ${arr[*]}"
