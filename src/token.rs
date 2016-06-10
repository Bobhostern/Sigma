// The Lexer :3
use std::iter::Peekable;
use std::str::Chars;
use std::char;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
	Identifier,
	Integer,
	Float,
	StrConst,
	Symbol, // An atomic id that has 1 value (Ruby symbols / Erlang atom)

	LParen,
	RParen,
	LAngle,
	RAngle,
	LBracket,
	RBracket,
	LBrace,
	RBrace,
	Comma,
	Colon,
	DoubleColon,
	Equal,
	Arrow,
	Dot,
	DoubleDot,
	TripleDot,
	Dollar,
	Exclaimation,
	Plus,
	Minus,

	Struct,
	Is,
	Where,
	End,
	Submodule,
	Trait,
	Access,
	Write,
	Public,
	Def,
	Impl,
	For,
	Func,
	Do,
	Var,
	Import,
	From,
	Return,
	Closure,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenData {
	Basic(String),
	Integer(i32),
	Float(f32),
	Null
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	pub token_type: TokenType,
	pub line_number: usize,
	pub data: TokenData
}

impl Token {
	pub fn new(tt: TokenType, dt: TokenData, ln: usize) -> Token {
		Token {
			token_type: tt,
			data: dt,
			line_number: ln
		}
	}

	pub fn simple(tt: TokenType, ln: usize) -> Token {
		Token {
			token_type: tt,
			data: TokenData::Null,
			line_number: ln
		}
	}
}

quick_error! {
	#[derive(Debug, Clone)]
	pub enum LexerError {
		MalformedNumber(s: String, u: usize) {
			display("Malformed number {} at line {}", s, u)
		}
		MalformedEscape(s: String, u: usize) {
			display("Malformed escape at string starting with {} at line {}", s, u)
		}
		Unknown(c: char, u: usize) {
			display("Unknown char {} at line {}", c, u)
		}
	}
}

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Clone)]
pub struct Lexer<'a> {
	line_number: usize,
	char_stream: Peekable<Chars<'a>>,
	keywords: HashMap<String, TokenType>,
}

impl<'a> Lexer<'a> {
	pub fn new(s: &'a str) -> Lexer<'a> {
		let mut keywords = HashMap::new();
		macro_rules! keyword {
			($i:ident => $c:expr) => (keywords.insert($c.into(), TokenType::$i))
		};
		keyword!(Struct => "struct");
		keyword!(Is => "is");
		keyword!(Where => "where");
		keyword!(End => "end");
		keyword!(Import => "import");
		keyword!(From => "from");
		keyword!(Submodule => "submodule");
		keyword!(Trait => "trait");
		keyword!(Access => "access");
		keyword!(Write => "write");
		keyword!(Public => "public");
		keyword!(Def => "def");
		keyword!(Impl => "impl");
		keyword!(For => "for");
		keyword!(Func => "func");
		keyword!(Do => "do");
		keyword!(Var => "var");
		keyword!(Return => "return");
		keyword!(Closure => "closure");
		Lexer {
			char_stream: s.chars().peekable(),
			line_number: 1,
			keywords: keywords,
		}
	}
}

impl<'a> Iterator for Lexer<'a> {
	type Item = LexerResult<Token>;
	fn next(&mut self) -> Option<Self::Item> {
		while let Some(c) = self.char_stream.next() {
			match c {
				'#' => if let Some(&c) = self.char_stream.peek() {
					match c {
						'{' => {
							let mut depth = 1;
							while let Some(c) = self.char_stream.next() {
								// Multiline, multidepth comments, oh my!
								if depth < 1 { break }
								match c {
									'#' => if let Some(&c) = self.char_stream.peek() {
										match c {
											'{' => depth += 1,
											_ => ()
										}
									},
									'}' => if let Some(&c) = self.char_stream.peek() {
										match c {
											'#' => depth -= 1,
											_ => ()
										}
									},
									'\n' => self.line_number += 1,
									_ => ()
								}
							}
						},
						_ => while let Some(b) = self.char_stream.next() {
							if b == '\n' { self.line_number += 1; break }
						}
					}
				},
				'A'...'Z' | 'a'...'z' | '_' => {
					let mut vec = vec![c];
					while let Some(&c) = self.char_stream.peek() {
						match c {
							'A'...'Z' | 'a'...'z' | '_' | '0'...'9' | '!' | '?' => {
								vec.push(self.char_stream.next().unwrap());
							},
							_ => break
						}
					}
					let out = vec.into_iter().collect::<String>();
					return Some(Ok(match self.keywords.get(&out) {
						Some(kyt) => Token::simple(kyt.clone(), self.line_number),
						None => Token::new(TokenType::Identifier, TokenData::Basic(out), self.line_number)
					}))
				},
				'@' => {
					let mut vec = vec![];
					while let Some(&c) = self.char_stream.peek() {
						match c {
							'A'...'Z' | 'a'...'z' | '_' | '0'...'9' | '!' | '?' => {
								vec.push(self.char_stream.next().unwrap());
							},
							_ => break
						}
					}
					let out = vec.into_iter().collect::<String>();
					return Some(Ok(Token::new(TokenType::Symbol, TokenData::Basic(out), self.line_number)))
				},
				'0'...'9' => {
					let mut vec = vec![c];
					while let Some(&n) = self.char_stream.peek() {
						match n {
							'0'...'9' | '.' => vec.push(self.char_stream.next().unwrap()),
							_ => break
						}
					}
					let float_mode = vec.iter().any(|&x| x == '.');
					let num = vec.iter().cloned().collect::<String>();
					if !float_mode {
						if let Ok(i) = num.parse::<i32>() {
							return Some(Ok(Token::new(TokenType::Integer, TokenData::Integer(i), self.line_number)))
						}
						return Some(Err(LexerError::MalformedNumber(num, self.line_number)))
					} else {
						if let Ok(f) = num.parse::<f32>() {
							return Some(Ok(Token::new(TokenType::Float, TokenData::Float(f), self.line_number)))
						}
						return Some(Err(LexerError::MalformedNumber(num, self.line_number)))
					}
				},
				'"' => {
					let mut string = Vec::new();
					let mut escape = false;
					let sln = self.line_number;

					while let Some(c) = self.char_stream.next() {
						macro_rules! unicode_escape {
							($c:expr, $s:expr, $e:expr, $_self:expr) => {{
								let mut out: u32 = 0;
								for _ in 0..$c {
									match self.char_stream.next() {
										Some(c) => match c.to_digit(16) {
											Some(d) => {
												out *= 16;
												out += d;
											},
											None => return Some(Err(LexerError::MalformedEscape($s.iter().cloned().collect(), $_self.line_number)))
										},
										None => return Some(Err(LexerError::MalformedEscape($s.iter().cloned().collect(), $_self.line_number)))
									}
								}

								match char::from_u32(out) {
									Some(c) => {$s.push(c); $e = false},
									None => return Some(Err(LexerError::MalformedEscape($s.iter().cloned().collect(), $_self.line_number)))
								}
							}};
						};
						match c {
							'"' if !escape => break,
							'\\' if !escape => escape = true,
							'\\' if escape => {string.push('\\'); escape = false;},
							't' if escape => {string.push('\t'); escape = false;},
							'r' if escape => {string.push('\r'); escape = false;},
							'n' if escape => {string.push('\n'); escape = false;},
							's' if escape => {string.push(' '); escape = false;},
							// My first joke
							'h' if escape => {"hello world".chars().inspect(|x| string.push(*x)).collect::<Vec<_>>(); escape = false;},
							'x' if escape => unicode_escape!(2, string, escape, self),
							'u' if escape => unicode_escape!(4, string, escape, self),
							'U' if escape => unicode_escape!(8, string, escape, self),

							'\r' => (), // Skip \r to regulate newlines to \n
							'\t' if escape => escape = false, // A hidden goodie ;)
							'\n' if escape =>  {self.line_number += 1; escape = false;},
							_ if escape => return Some(Err(LexerError::MalformedEscape(string.iter().cloned().collect(), self.line_number))),
							'\n' if !escape =>  { string.push(c); self.line_number += 1;}
							_ => string.push(c)
						}
					}

					let out: String = string.iter().cloned().collect();
					return Some(Ok(Token::new(TokenType::StrConst, TokenData::Basic(out), sln)))
				},
				':' => match self.char_stream.peek() {
					Some(&':') => {
						self.char_stream.next();
						return Some(Ok(Token::simple(TokenType::DoubleColon, self.line_number)))
					},
					_ => return Some(Ok(Token::simple(TokenType::Colon, self.line_number))),
				},
				'=' => return Some(Ok(Token::simple(TokenType::Equal, self.line_number))),
				'-' => match self.char_stream.peek() {
					Some(&'>') => {
						self.char_stream.next();
						return Some(Ok(Token::simple(TokenType::Arrow, self.line_number)))
					},
					_ => return Some(Ok(Token::simple(TokenType::Minus, self.line_number))),
				},
				'.' => match self.char_stream.peek() {
					Some(&'.') => {
						self.char_stream.next();
						match self.char_stream.peek() {
							Some(&'.') => {
								self.char_stream.next();
								return Some(Ok(Token::simple(TokenType::TripleDot, self.line_number)))
							},
							_ => return Some(Ok(Token::simple(TokenType::DoubleDot, self.line_number))),
						}
					},
					_ => return Some(Ok(Token::simple(TokenType::Dot, self.line_number))),
				},
				',' => return Some(Ok(Token::simple(TokenType::Comma, self.line_number))),
				'$' => return Some(Ok(Token::simple(TokenType::Dollar, self.line_number))),
				'<' => return Some(Ok(Token::simple(TokenType::LAngle, self.line_number))),
				'>' => return Some(Ok(Token::simple(TokenType::RAngle, self.line_number))),
				'(' => return Some(Ok(Token::simple(TokenType::LParen, self.line_number))),
				')' => return Some(Ok(Token::simple(TokenType::RParen, self.line_number))),
				'[' => return Some(Ok(Token::simple(TokenType::LBracket, self.line_number))),
				']' => return Some(Ok(Token::simple(TokenType::RBracket, self.line_number))),
				'{' => return Some(Ok(Token::simple(TokenType::LBrace, self.line_number))),
				'}' => return Some(Ok(Token::simple(TokenType::RBrace, self.line_number))),
				'+' => return Some(Ok(Token::simple(TokenType::Plus, self.line_number))),
				'\n' => self.line_number += 1,
				' ' | '\t' | '\r' => (),
				_ => return Some(Err(LexerError::Unknown(c, self.line_number)))
			}
		}
		None
	}
}
