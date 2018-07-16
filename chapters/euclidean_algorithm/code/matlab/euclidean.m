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