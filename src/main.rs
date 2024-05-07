use std::io::BufRead;
use pratt_sample::expr;

fn main() {
    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        let s = expr(&line);
        println!("{}", s)
    }
}
