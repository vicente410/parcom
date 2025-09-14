mod parcom;

use crate::parcom::predicate;
use crate::parcom::map;
use crate::parcom::some;

fn main() {
    println!(
        "{:?}",
        map(some(predicate(|c| char::is_ascii_digit(&c))), |n| n
            .iter()
            .collect::<String>()
            .parse::<i32>())("123abc")
    );
}
