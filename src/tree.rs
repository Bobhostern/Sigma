use super::token::{Lexer, Token, TokenType, TokenData, LexerError};
use super::util::{ConstantTable, ConstantName, Constant};
use std::error::Error;
use url::{Url, ParseError as URLParseError};

use super::parslets::{LiteralParslet};

quick_error! {
	#[derive(Debug, Clone)]
	pub enum ParserError {
		UnexpectedEndOfInput {
			description("Unexpected end of input")
		}
		UnexpectedInput(t: Token) {
			description("Unexpected input")
			display(x) -> ("{} {:?}", x.description(), t)
		}
		MismatchTokenType(e: TokenType, t: Token) {
			description("Unexpected input type")
			display(x) -> ("{} where {:?} was expected as {:?}", x.description(), t, e)
		}
		ImportURL(e: URLParseError) {
			from()
		}
	}
}

// Things that "have" a value.
#[derive(Debug, Clone)]
pub enum Expression {
	Integer(ConstantName),
	Float(ConstantName),
	StrConst(ConstantName),
	Symbol(ConstantName),
}

// Things that DO NOT "have" a value, but can be used in lower-level blocks
#[derive(Debug, Clone)]
pub enum Statement {

}

// Things that DO NOT "have" a value, and cannot be used in lower-level blocks
#[derive(Debug, Clone)]
pub enum Structure {
	Import(Import),
	Struct(Struct),
	Submodule(Submodule),
}

#[derive(Debug, Clone)]
pub struct Import {
	pub names: Vec<(String, Option<String>)>,
	pub source: Url,
}

#[derive(Debug, Clone)]
pub struct TypeName {
	pub scopes: Vec<String>,
	pub name: String,
	pub type_args: Vec<TypeName>
}

#[derive(Debug, Clone)]
pub struct Struct {
	pub name: String,
	pub type_params: Vec<(String, Vec<TypeName>)>,
	pub members: Vec<(String, TypeName)>,
}

#[derive(Debug, Clone)]
pub struct Submodule {
	pub name: String,
	pub members: Vec<Structure>,
}

#[derive(Debug, Clone)]
pub struct Function {
	pub name: String,
	pub type_params: Vec<(String, Vec<TypeName>)>,
	pub arguments: Vec<(String, TypeName)>,
	pub body: Expression
}

#[derive(Debug, Clone)]
pub struct Trait {
	
}

pub type ParserResult<T> = Result<T, ParserError>;

pub trait PrefixParslet {
	fn parse(&self, _: &mut Parser, _: Token) -> ParserResult<Expression>;
}

pub trait InfixParslet {
	fn parse(&self, _: &mut Parser, _: Token, _: Expression) -> ParserResult<Expression>;
	fn precedence(&self) -> i32;
}

fn get_prefix_parslet(t: &Token) -> Option<Box<PrefixParslet>> {
	match t.token_type {
		TokenType::Integer | TokenType::Float | TokenType::StrConst | TokenType::Symbol => Some(Box::new(LiteralParslet)),
		_ => None
	}
}

fn get_infix_parslet(t: &Token) -> Option<Box<InfixParslet>> {
	match t.token_type {
		_ => None
	}
}

pub struct Parser<'a> {
	lexer: Lexer<'a>,
	buffer: Vec<Token>,
	table: ConstantTable,
	pub lex_errors: Option<Vec<LexerError>>
}

impl<'a> Parser<'a> {
	pub fn new(l: Lexer<'a>, sc: usize) -> Parser<'a> {
		Parser {
			lexer: l,
			buffer: vec![],
			table: ConstantTable::new(sc),
			lex_errors: None
		}
	}

	pub fn parse_structure(&mut self) -> ParserResult<Structure> {
		match self.consume(0) {
			Some(tok) => match tok.token_type {
				TokenType::Import => Ok(Structure::Import(try!(self.import_rule()))),
				TokenType::Struct => Ok(Structure::Struct(try!(self.struct_rule()))),
				TokenType::Submodule => Ok(Structure::Submodule(try!(self.submodule_rule()))),
				_ => Err(ParserError::UnexpectedInput(tok))
			},
			None => Err(ParserError::UnexpectedEndOfInput)
		}
	}

	pub fn file_rule(&mut self) -> ParserResult<Vec<Structure>> {
		let mut v = vec![];
		while let Some(_) = self.lookahead(0) {
			v.push(try!(self.parse_structure()));
		}

		Ok(v)
	}

	pub fn type_name_rule(&mut self) -> ParserResult<TypeName> {
		let mut name = match try!(self.expect(TokenType::Identifier)).data {
			TokenData::Basic(d) => d,
			_ => unreachable!()
		};
		let mut ta = vec![];
		let mut scopes = vec![];
		while self.is_present(TokenType::DoubleColon) {
			scopes.push(name);
			name = match try!(self.expect(TokenType::Identifier)).data {
				TokenData::Basic(d) => d,
				_ => unreachable!()
			};
		}
		if self.is_present(TokenType::LBracket) {
			while !self.is_present(TokenType::RBracket) {
				let tname = try!(self.type_name_rule());
				self.is_present(TokenType::Comma);
				ta.push(tname);
			}
		}
		Ok(TypeName {
			scopes: scopes,
			name: name,
			type_args: ta
		})
	}

	pub fn import_rule(&mut self) -> ParserResult<Import> {
		try!(self.expect(TokenType::LParen));
		let mut imports = vec![];
		while !self.is_present(TokenType::RParen) {
			let id = match try!(self.expect(TokenType::Identifier)).data {
				TokenData::Basic(s) => s,
				_ => unreachable!()
			};

			let mut specif = None;
			if self.is_present(TokenType::LBracket) {
				specif = match try!(self.expect(TokenType::Symbol)).data {
					TokenData::Basic(s) => Some(s),
					_ => unreachable!()
				};
				try!(self.expect(TokenType::RBracket));
			}

			imports.push((id, specif));
		}

		try!(self.expect(TokenType::From));
		let url = match try!(self.expect(TokenType::StrConst)).data {
			TokenData::Basic(s) => match Url::parse(&s) {
				Ok(url) => url,
				Err(e) => return Err(e.into())
			},
			_ => unreachable!()
		};

		Ok(Import {
			names: imports,
			source: url
		})
	}

	pub fn struct_rule(&mut self) -> ParserResult<Struct> {
		let name = match try!(self.expect(TokenType::Identifier)).data {
			TokenData::Basic(b) => b,
			_ => unreachable!()
		};
		let mut tp = vec![];
		if self.is_present(TokenType::LBracket) {
			while !self.is_present(TokenType::RBracket) {
				let name = match try!(self.expect(TokenType::Identifier)).data {
					TokenData::Basic(b) => b,
					_ => unreachable!()
				};
				let mut must_impl = vec![];
				if self.is_present(TokenType::Colon) {
					let first = try!(self.type_name_rule());
					must_impl.push(first);
					while self.is_present(TokenType::Plus) {
						let next = try!(self.type_name_rule());
						must_impl.push(next)
					}
				}
				self.is_present(TokenType::Comma);
				tp.push((name, must_impl));
			}
		}
		try!(self.expect(TokenType::Is));
		let mut members = vec![];
		while !self.is_present(TokenType::End) {
			let memname = match try!(self.expect(TokenType::Identifier)).data {
				TokenData::Basic(d) => d,
				_ => unreachable!()
			};
			try!(self.expect(TokenType::Colon));
			let type_name = try!(self.type_name_rule());
			members.push((memname, type_name));
		}
		Ok(Struct {
			name: name,
			type_params: tp,
			members: members
		})
	}

	/// A submodule is a separate scope within a file.
	pub fn submodule_rule(&mut self) -> ParserResult<Submodule> {
		let name = match try!(self.expect(TokenType::Identifier)).data {
			TokenData::Basic(b) => b,
			_ => unreachable!()
		};
		try!(self.expect(TokenType::Is));
		let mut members = vec![];
		while !self.is_present(TokenType::End) {
			members.push(try!(self.parse_structure()));
		}

		Ok(Submodule {
			name: name,
			members: members
		})
	}

	pub fn parse_expression(&mut self, prec: i32) -> ParserResult<Expression> {
		let tok = match self.consume(0) {
			Some(tok) => tok,
			None => return Err(ParserError::UnexpectedEndOfInput)
		};

		let mut left = match get_prefix_parslet(&tok) {
			Some(parslet) => match parslet.parse(self, tok) {
				Ok(expr) => expr,
				Err(e) => return Err(e)
			},
			None => return Err(ParserError::UnexpectedInput(tok))
		};

		loop {
			match self.lookahead(0) {
				Some(tok) => match get_infix_parslet(tok) {
					Some(pars) => if pars.precedence() <= prec { break },
					None => break
				},
				None => break
			};

			if let Some(tok) = self.consume(0) {
				match get_infix_parslet(&tok) {
					Some(parslet) => left = match parslet.parse(self, tok, left) {
						Ok(expr) => expr,
						Err(e) => return Err(e)
					},
					None => return Err(ParserError::UnexpectedInput(tok))
				}
			}
		}

		Ok(left)
	}

	pub fn constants(&self) -> &ConstantTable {
		&self.table
	}

	pub fn mut_constants(&mut self) -> &mut ConstantTable {
		&mut self.table
	}

	pub fn is_present(&mut self, t: TokenType) -> bool {
		let res = match self.lookahead(0) {
			Some(ref tok) => if t == tok.token_type {
				true
			} else {
				false
			},
			None => false
		};
		if res { self.consume(0); }
		res
	}

	pub fn expect(&mut self, t: TokenType) -> ParserResult<Token> {
		match self.consume(0) {
			Some(tok) => if t == tok.token_type {
				Ok(tok)
			} else {
				Err(ParserError::MismatchTokenType(t, tok))
			},
			None => Err(ParserError::UnexpectedEndOfInput)
		}
	}

	pub fn consume(&mut self, i: usize) -> Option<Token> {
		self.lookahead(i);
		self.buffer.drain(0..i+1).nth(i)
	}

	pub fn lookahead(&mut self, i: usize) -> Option<&Token> {
		while self.buffer.len() <= i {
			// Expand the buffer
			match self.lexer.next() {
				Some(Ok(tok)) => self.buffer.push(tok),
				Some(Err(e)) => {
					match self.lex_errors {
						Some(ref mut v) => v.push(e),
						None => self.lex_errors = Some(vec![e])
					};
					return None
				},
				None => return None,
			}
		}
		Some(self.buffer.iter().nth(i).unwrap())
	}
}
