// contributed by Nicole Mazzuca (ubsan)

fn euclid_sub(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while a != b {
        if a < b {
            b -= a;
        } else {
            a -= b;
        }
    }

    a
}

fn euclid_rem(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let tmp = b;
        b = a % b;
        a = tmp;
    }

    a
}

fn main() {
    let chk1 = euclid_rem(64 * 67, 64 * 81);
    let chk2 = euclid_sub(128 * 12, 128 * 77);
    println!("{}", chk1);
    println!("{}", chk2);
}
