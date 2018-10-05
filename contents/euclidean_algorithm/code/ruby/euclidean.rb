def gcd_mod(a, b)
	a = a.abs
	b = b.abs
	a, b = b, a%b until b.zero?
	a
end
 
def gcd_minus(a, b)
	a = a.abs
	b = b.abs
	until a == b
		if a > b
			a -= b
		else
			b -= a
		end
	end
	a
end
 
p gcd_mod(12 * 6, 12 * 4) #=> 12
p gcd_mod(9 * 667, 9 * 104) #=> 9

p gcd_minus(12 * 6, 12 * 4) #=> 12
p gcd_minus(9 * 667, 9 * 104) #=> 9
