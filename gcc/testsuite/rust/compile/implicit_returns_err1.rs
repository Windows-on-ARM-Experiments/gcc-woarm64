fn test(x: i32) -> i32 {
    if x > 1 { // { dg-error "mismatched types, expected .... but got .<integer>." }
        1
    } else {
        2
    }
    3
}

fn main() {
    let a = test(1);
}
