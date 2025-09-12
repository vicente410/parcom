type Parser<'a, T> = Box<dyn Fn(&str) -> Option<(&str, T)> + 'a>;

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

pub fn pair<'a, T: 'a, U: 'a>(p1: Parser<'a, T>, p2: Parser<'a, U>) -> Parser<'a, (T, U)> {
    Box::new(move |input: &str| {
        if let Some((rest, output1)) = p1(input) {
            if let Some((rest, output2)) = p2(rest) {
                Some((rest, (output1, output2)))
            } else {
                return None;
            }
        } else {
            return None;
        }
    })
}
