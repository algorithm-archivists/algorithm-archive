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

print "[#]\nModulus-based euclidean algorithm result:\n"
p gcd_mod(64 * 67, 64 * 81)
print "[#]\nSubtraction-based euclidean algorithm result:\n"
p gcd_minus(128 * 12, 128 * 77)
