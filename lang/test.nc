
struct MyData {
    a: i32,
}

fn main() -> i32 {
    let a = 32;
    const b = 3;

    if a == b {
        return 1;
    }

    let c;
    switch a {
        0 => {
            c = 32;
        },
        _ => {
            c = 2;
        },
    };
    return 0;
}
