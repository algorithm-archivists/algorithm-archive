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
 
p gcd_mod(108, 48) #=> 12
p gcd_mod(6003, 936) #=> 9
 
p gcd_minus(108, 48) #=> 12
p gcd_minus(6003, 936) #=> 9
