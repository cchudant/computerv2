pub fn gcd(first: i64, second: i64) -> i64 {
    let mut max = first.abs();
    let mut min = second.abs();
    if min > max {
        let val = max;
        max = min;
        min = val;
    }

    loop {
        let res = max % min;
        if res == 0 {
            return min;
        }

        max = min;
        min = res;
    }
}
