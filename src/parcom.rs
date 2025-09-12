type Parser<T> = dyn Fn(&str) -> Option<(&str, T)>;

pub fn char(c: char) -> Box<Parser<char>> {
    Box::new(move |s: &str| {
        if s.starts_with(c) {
            Some((&s[1..], c))
        } else {
            None
        }
    })
}
