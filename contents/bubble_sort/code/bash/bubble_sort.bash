#!/usr/bin/env bash
bubble_sort() {
    local i
    local j
    local tmp
    local len
    local arr
    arr=("$@")
    (( len = ${#arr[@]} ))

    for i in $(seq 0 $(( len - 1 ))); do
	for j in $(seq 0 $(( len - 2 ))); do
	    if (( arr[j] > arr[(( j + 1 ))] )); then
		(( tmp = arr[(( j + 1 ))] ))
		(( arr[(( j + 1 ))] = arr[j] ))
		(( arr[j] = tmp ))
	    fi
	done
    done
    echo ${arr[*]}
}

arr=(1 45 756 4569 56 3 8 5 -10 -4)
echo "Unsorted array: ${arr[*]}"
tmp=$(bubble_sort "${arr[@]}")
echo "Sorted array: ${tmp[*]}"

