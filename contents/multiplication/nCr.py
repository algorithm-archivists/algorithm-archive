'''

fermits little theorem
nCr % p = (fac[n]* modinverse(fac[r]) % p *  modinverse(fac[n-r]) % p) % p

modinverse(x) % p = x^p-2

'''


def ncr(n, r, p): 
    numerator = denominator = 1 
    for i in range(r): 
        numerator = (numerator * (n - i)) % p 
        denominator = (denominator * (i + 1)) % p     
    return (numerator * pow(denominator , p - 2 , p)) % p 


n, r, p = 10**5, 20000, 1000000007

print(ncr(n, r, p)) 
