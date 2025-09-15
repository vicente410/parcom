pub type Parser<'a, T> = Box<dyn Fn(&str) -> Option<(&str, T)> + 'a>;

pub fn char(c: char) -> Parser<'static, char> {
    Box::new(move |input: &str| {
        if input.starts_with(c) {
            Some((&input[1..], c))
        } else {
            None
        }
    })
}

pub fn string(s: &str) -> Parser<'_, &str> {
    Box::new(move |input: &str| {
        if input.starts_with(s) {
            Some((&input[s.len()..], s))
        } else {
            None
        }
    })
}

pub fn predicate<'a>(f: fn(char) -> bool) -> Parser<'a, char> {
    Box::new(move |input: &str| {
        if input.len() > 0 && f(input.as_bytes()[0] as char) {
            Some((&input[1..], input.as_bytes()[0] as char))
        } else {
            None
        }
    })
}

pub fn map<'a, T: 'a, U: 'a>(p: Parser<'a, T>, f: fn(T) -> U) -> Parser<'a, U> {
    Box::new(move |input: &str| p(input).map(|(rest, output)| (rest, f(output))))
}

pub fn map_option<'a, T: 'a, U: 'a>(p: Parser<'a, T>, f: fn(T) -> Option<U>) -> Parser<'a, U> {
    Box::new(move |input: &str| {
        p(input).and_then(|(rest, output)| f(output).map(|output| (rest, output)))
    })
}

pub fn map_result<'a, T: 'a, U: 'a, V: 'a>(
    p: Parser<'a, T>,
    f: fn(T) -> Result<U, V>,
) -> Parser<'a, U> {
    Box::new(move |input: &str| {
        p(input).and_then(|(rest, output)| f(output).ok().map(|output| (rest, output)))
    })
}

pub fn pair<'a, T: 'a, U: 'a>(p1: Parser<'a, T>, p2: Parser<'a, U>) -> Parser<'a, (T, U)> {
    Box::new(move |input: &str| {
        p1(input)
            .and_then(|(rest, output1)| p2(rest).map(|(rest, output2)| (rest, (output1, output2))))
    })
}

pub fn branch<'a, T: 'a>(p1: Parser<'a, T>, p2: Parser<'a, T>) -> Parser<'a, T> {
    Box::new(move |input: &str| p1(input).or(p2(input)))
}

pub fn delimited<'a, T: 'a, U: 'a, V: 'a>(
    p1: Parser<'a, T>,
    p2: Parser<'a, U>,
    p3: Parser<'a, V>,
) -> Parser<'a, U> {
    Box::new(move |input: &str| {
        p1(input).and_then(|(rest, _)| {
            p2(rest).and_then(|(rest, output)| p3(rest).map(|(rest, _)| (rest, output)))
        })
    })
}

pub fn separated_pair<'a, T: 'a, U: 'a, V: 'a>(
    p1: Parser<'a, T>,
    p2: Parser<'a, U>,
    p3: Parser<'a, V>,
) -> Parser<'a, (T, V)> {
    Box::new(move |input: &str| {
        p1(input).and_then(|(rest, output1)| {
            p2(rest)
                .and_then(|(rest, _)| p3(rest).map(|(rest, output2)| (rest, (output1, output2))))
        })
    })
}

pub fn some<'a, T: 'a>(p: Parser<'a, T>) -> Parser<'a, Vec<T>> {
    Box::new(move |input: &str| {
        let mut outputs = Vec::new();
        let mut final_rest = input;

        while let Some((rest, output)) = p(final_rest) {
            final_rest = rest;
            outputs.push(output);
        }

        if outputs.len() > 0 {
            Some((final_rest, outputs))
        } else {
            None
        }
    })
}

pub fn many<'a, T: 'a>(p: Parser<'a, T>) -> Parser<'a, Vec<T>> {
    Box::new(move |input: &str| {
        let mut outputs = Vec::new();
        let mut final_rest = input;

        while let Some((rest, output)) = p(final_rest) {
            final_rest = rest;
            outputs.push(output)
        }

        Some((final_rest, outputs))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn char_success() {
        assert_eq!(char('a')("abc"), Some(("bc", 'a')));
    }

    #[test]
    fn char_ahead() {
        assert_eq!(char('b')("abc"), None);
    }

    #[test]
    fn char_no_input() {
        assert_eq!(char('b')(""), None);
    }

    #[test]
    fn char_not_present() {
        assert_eq!(char('d')("abc"), None);
    }

    #[test]
    fn string_success() {
        assert_eq!(string("hello")("hello world"), Some((" world", "hello")));
    }

    #[test]
    fn string_ahead() {
        assert_eq!(string("hello")("well, hello world"), None);
    }

    #[test]
    fn string_incomplete() {
        assert_eq!(string("hello")("hell"), None);
    }

    #[test]
    fn predicate_true() {
        assert_eq!(
            predicate(|c| c.is_alphabetic())("abc123"),
            Some(("bc123", 'a'))
        );
    }

    #[test]
    fn predicate_false() {
        assert_eq!(predicate(|c| c.is_numeric())("abc123"), None);
    }

    #[test]
    fn map_some() {
        assert_eq!(map(char('a'), |c| c as u8)("abc"), Some(("bc", 'a' as u8)));
    }

    #[test]
    fn map_none() {
        assert_eq!(map(char('d'), |c| c as u8)("abc"), None);
    }

    #[test]
    fn map_option_some() {
        assert_eq!(map_option(char('a'), |c| Some(c))("abc"), Some(("bc", 'a')));
    }

    #[test]
    fn map_option_none_function() {
        assert_eq!(map_option::<char, ()>(char('a'), |_| None)("abc"), None);
    }

    #[test]
    fn map_option_none_parser() {
        assert_eq!(map_option(char('b'), |c| Some(c))("abc"), None);
    }

    #[test]
    fn map_result_ok() {
        assert_eq!(
            map_result::<char, char, ()>(char('a'), |c| Ok(c))("abc"),
            Some(("bc", 'a'))
        );
    }

    #[test]
    fn map_result_err() {
        assert_eq!(
            map_result::<char, char, ()>(char('a'), |_| Err(()))("abc"),
            None
        );
    }

    #[test]
    fn map_result_none() {
        assert_eq!(
            map_result::<char, char, ()>(char('b'), |c| Ok(c))("abc"),
            None
        );
    }

    #[test]
    fn pair_success() {
        assert_eq!(pair(char('a'), char('b'))("abc"), Some(("c", ('a', 'b'))));
    }

    #[test]
    fn pair_first_none() {
        assert_eq!(pair(char('d'), char('a'))("abc"), None);
    }

    #[test]
    fn pair_second_none() {
        assert_eq!(pair(char('a'), char('c'))("abc"), None);
    }

    #[test]
    fn branch_first() {
        assert_eq!(branch(char('a'), char('b'))("abc"), Some(("bc", 'a')));
    }

    #[test]
    fn branch_second() {
        assert_eq!(branch(char('b'), char('a'))("abc"), Some(("bc", 'a')));
    }

    #[test]
    fn branch_none() {
        assert_eq!(branch(char('b'), char('c'))("abc"), None);
    }

    #[test]
    fn delimited_success() {
        assert_eq!(
            delimited(char('a'), char('b'), char('c'))("abc"),
            Some(("", 'b'))
        );
    }

    #[test]
    fn delimited_first_none() {
        assert_eq!(delimited(char('b'), char('b'), char('c'))("abc"), None);
    }

    #[test]
    fn delimited_second_none() {
        assert_eq!(delimited(char('a'), char('c'), char('c'))("abc"), None);
    }

    #[test]
    fn delimited_third_none() {
        assert_eq!(delimited(char('a'), char('b'), char('d'))("abc"), None);
    }

    #[test]
    fn separated_pair_success() {
        assert_eq!(
            separated_pair(char('a'), char('b'), char('c'))("abc"),
            Some(("", ('a', 'c')))
        );
    }

    #[test]
    fn separated_pair_first_none() {
        assert_eq!(separated_pair(char('b'), char('b'), char('c'))("abc"), None);
    }

    #[test]
    fn separated_pair_second_none() {
        assert_eq!(separated_pair(char('a'), char('c'), char('c'))("abc"), None);
    }

    #[test]
    fn separated_pair_third_none() {
        assert_eq!(separated_pair(char('b'), char('b'), char('d'))("abc"), None);
    }

    #[test]
    fn some_success() {
        assert_eq!(
            some(char(' '))("   abc"),
            Some(("abc", vec![' ', ' ', ' ']))
        );
    }

    #[test]
    fn some_nothing() {
        assert_eq!(some(char(' '))("abc"), None);
    }

    #[test]
    fn many_success() {
        assert_eq!(
            many(char(' '))("   abc"),
            Some(("abc", vec![' ', ' ', ' ']))
        );
    }

    #[test]
    fn many_nothing() {
        assert_eq!(many(char(' '))("abc"), Some(("abc", vec![])));
    }
}
