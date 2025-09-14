mod parcom;

use crate::parcom::char;
use crate::parcom::string;
use crate::parcom::some;
use crate::parcom::many;
use crate::parcom::pair;
use crate::parcom::take_while;
use crate::parcom::map;

fn main() {
    //println!("{:?}", pair(pair(char('H'), string("el")), char('l'))("Hello"));
    //println!("{:?}", many(char(' '))("Hello"));
    //println!("{:?}", some(char(' '))("Hello"));
    println!("{:?}", many(string("H"))("ello"));
    println!("{:?}", map(take_while(|c| char::is_ascii_digit(&c)), |n| n.parse::<i32>())("123abc"));
}
