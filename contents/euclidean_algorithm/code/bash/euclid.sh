abs() {
	local ret=$1
	if [ $ret -lt 0 ]; then
		let "ret = $ret * -1"
	fi
	echo $ret
} 

euclid_mod() {
	local a=$(abs $1)
	local b=$(abs $2)
	
	while [ $b -ne 0 ]; do
		let "tmp = $b"
		let "b = $a % $b"
		let "a = tmp"
	done
	echo $a
}

euclid_sub() {
	local a=$(abs $1)
	local b=$(abs $2)
	
	while [ $a -ne $b ]; do
		if [ $a -gt $b ]; then
			let "a -= $b"
		else
			let "b -= $a"
		fi
	done
	echo $a
}

result=$(euclid_mod 143 693)
echo $result
result=$(euclid_sub 150 400)
echo $result
