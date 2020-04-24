#!/usr/bin/env bash
inCircle() {
    local ret
    local mag
    ((ret = 0))
    if (($1 ** 2 + $2 ** 2 < 1073676289)); then # 1073676289 = 32767 ** 2
        ((ret = 1))
    fi
    printf "%d" $ret
}

monteCarlo() {
    local count
    local i
    ((count = 0))
    for ((i = 0; i < $1; i++)); do
        if (($(inCircle RANDOM RANDOM) == 1)); then
            ((count++))
        fi
    done
    echo "scale = 8; 4 * $count / $1" | bc
}

est=$(monteCarlo 10000)
echo "The estimate of pi is $est"
echo "Percentage error: $(echo "scale = 8; 100 * sqrt( ( 1 - $est / (4*a(1)) ) ^ 2 )" | bc -l)"
