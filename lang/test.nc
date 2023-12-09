use test::foo::Option;
use test::bar::Data;
use std::System;


type MyData = struct {
    a: i32,
}

fn main() -> i32 {
    let a = 32;
    let b = 3;

    let _ = Option::Some(b);

    if a == b {
        return 1;
    }

    let c;
    match a {
        0 => {
            c = 32;
        },
        _ => {
            c = 2;
        },
    };
    return 0;
}
