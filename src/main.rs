mod parcom;

use crate::parcom::char;
use crate::parcom::string;
use crate::parcom::some;
use crate::parcom::many;
use crate::parcom::pair;

fn main() {
    println!("{:?}", pair(pair(char('H'), string("el")), char('l'))("Hello"));
    println!("{:?}", many(char(' '))("Hello"));
    println!("{:?}", some(char(' '))("Hello"));
}
