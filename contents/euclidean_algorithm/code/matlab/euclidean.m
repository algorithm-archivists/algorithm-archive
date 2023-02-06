// Submitted by Max Weinstein

function gcd = euclidSub(a,b)
    
    a = abs(a);
    b = abs(b);
    
    while a ~= b
        if a > b
            a = a - b;
        else
            b = b - a;
        end
    end
    
    gcd = a;
end

function gcd = euclidMod(a,b)
    
    a=abs(a);
    b=abs(b);
    
    while b > 0
        temp = b;
        b = mod(a,b);
        a = temp;
    end
    
    gcd = a;
end

function euclid()
    ['[#] Modulus-based euclidean algorithm result: ',num2str(euclidMod(64 * 67, 64 * 81))]

    ['[#] Subtraction-based euclidean algorithm result: ',num2str(euclidSub(128 * 12, 128 * 77))]
end