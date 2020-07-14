function s:euclid_mod(a, b)
	let l:a = abs(a:a)
	let l:b = abs(a:b)

	while l:b != 0
		let l:c = l:b
		let l:b = l:a % l:b
		let l:a = l:c
	endwhile

	return l:a
endfunction

function s:euclid_sub(a, b)
	let l:a = abs(a:a)
	let l:b = abs(a:b)

	while l:a != l:b
		if l:a > l:b
			let l:a -= l:b
		else
			let l:b -= l:a
		endif
	endwhile

	return l:a
endfunction

let s:check_1 = s:euclid_mod(64 * 67, 64 * 71)
let s:check_2 = s:euclid_sub(128 * 12, 128 * 77)

echo 'Modulus-based euclidean algorithm result:' s:check_1
echo 'subtraction-based euclidean algorithm result:' s:check_2
