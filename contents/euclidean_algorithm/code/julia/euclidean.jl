function euclid_mod(a::Int64, b::Int64)
    a = abs(a)
    b = abs(b)

    while(b != 0)
        b,a = a%b,b
    end

    return a
end

function euclid_sub(a::Int64, b::Int64)
    a = abs(a)
    b = abs(b)

    while (a != b)
        if (a > b)
            a -= b
        else
            b -= a
        end
    end

    return a
end

function main()
    check1 = euclid_mod(64 * 67, 64 * 81);
    check2 = euclid_sub(128 * 12, 128 * 77);

    println("[#]\nModulus-based euclidean algorithm result:\n$(check1)")
    println("[#]\nSubtraction-based euclidean algorithm result:\n$(check2)")

end

main()
