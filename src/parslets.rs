use super::tree::{Parser, InfixParslet, PrefixParslet, ParserError, Expression, ParserResult};
use super::token::{Token, TokenType, TokenData};
use super::util::{ConstantTable, ConstantName, Constant};

pub struct LiteralParslet;
impl PrefixParslet for LiteralParslet {
	fn parse(&self, parser: &mut Parser, tok: Token) -> ParserResult<Expression> {
		match tok.token_type {
			TokenType::Integer => match tok.data {
				TokenData::Integer(i) => Ok(Expression::Integer(parser.mut_constants().add(Constant::Integer(i)))),
				_ => unreachable!()
			},
			TokenType::Float => match tok.data {
				TokenData::Float(f) => Ok(Expression::Float(parser.mut_constants().add(Constant::Float(f)))),
				_ => unreachable!()
			},
			TokenType::StrConst => match tok.data {
				TokenData::Basic(s) => Ok(Expression::StrConst(parser.mut_constants().add(Constant::StrConst(s)))),
				_ => unreachable!()
			},
			TokenType::Symbol => match tok.data {
				TokenData::Basic(s) => Ok(Expression::Symbol(parser.mut_constants().add(Constant::StrConst(s)))),
				_ => unreachable!()
			},
			_ => Err(ParserError::UnexpectedInput(tok))
		}
	}
}
