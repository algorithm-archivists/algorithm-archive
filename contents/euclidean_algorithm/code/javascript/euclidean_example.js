function euclid_mod(a, b){
    a = Math.abs(a);
    b = Math.abs(b);

    var temp;
    while (b != 0){
        temp = b;
        b = a%b;
        a = temp;
    }

    return a;
}

function euclid_sub(a, b){
    a = Math.abs(a);
    b = Math.abs(b);

    while (a != b){
        if (a > b){
            a = a - b;
        }
        else{
            b = b - a;
        }
    }

    return a;
}

console.log(euclid_mod(64*67, 64*81));
console.log(euclid_sub(128*12, 128*77));
