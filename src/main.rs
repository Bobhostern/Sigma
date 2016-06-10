extern crate sigma;

fn main() {
	let test_source = include_str!("../test.sgm");
	let lex = sigma::token::Lexer::new(test_source);
	for t in lex {
		if t.is_ok() {
			println!("{:?}", t);
		} else {
			let t = t.unwrap_err();
			panic!("{}", t);
		}
	}
	let lex2 = sigma::token::Lexer::new(test_source);
	let mut parser = sigma::tree::Parser::new(lex2, 10); // Start w/ enough space for 10 consts w/o alloc

	println!("{:?}", parser.file_rule());
	let pcr: &Vec<_> = parser.constants().as_ref();
	println!("{:?}", pcr);
}
