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
    ['gcd(520,420) via euclidSub: ',num2str(euclidSub(520,420))]
    ['gcd(183,244) via euclidMod: ',num2str(euclidMod(183,244))]
end