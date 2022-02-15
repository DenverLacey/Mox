use crate::ast::{Node, NodeKind, AST};
use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug)]
pub struct CodeLocation {
	pub line: usize,
	pub col: usize,
	pub file: String,
}

impl CodeLocation {
	pub fn new(line: usize, col: usize, file: String) -> Self {
		Self { line, col, file }
	}
}

impl Display for CodeLocation {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}:{}:{}", self.line + 1, self.col + 1, self.file)
	}
}

#[derive(Debug)]
pub struct Token {
	pub data: TokenData,
	pub location: CodeLocation,
}

impl Token {
	fn new(data: TokenData, location: CodeLocation) -> Self {
		Self { data, location }
	}

	const fn precedence(&self) -> TokenPrecedence {
		use TokenData::*;
		match self.data {
			// Literals
			Bool(_) => TokenPrecedence::None,
			Int(_) => TokenPrecedence::None,
			Num(_) => TokenPrecedence::None,
			Str(_) => TokenPrecedence::None,

			// Delimeters
			Newline => TokenPrecedence::None,
			Semicolon => TokenPrecedence::None,

			// Operators
			Plus => TokenPrecedence::Term,
			Dash => TokenPrecedence::Term,
			Star => TokenPrecedence::Factor,
			Slash => TokenPrecedence::Factor,
		}
	}
}

#[derive(Debug)]
pub enum TokenData {
	// Literals
	Bool(bool),
	Int(i64),
	Num(f64),
	Str(String),

	// Delimeters
	Newline,
	Semicolon,

	// Operators
	Plus,
	Dash,
	Star,
	Slash,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
enum TokenPrecedence {
	None,
	Assignment, // = += -= *= /= &= etc.
	Colon,      // :
	Cast,       // as
	Range,      // .. ...
	Or,         // ||
	And,        // &&
	BitOr,      // |
	Xor,        // ^
	BitAnd,     // &
	Equality,   // == !=
	Comparison, // < > <= >=
	Shift,      // << >>
	Term,       // + -
	Factor,     // * / %
	Unary,      // ! ~
	Call,       // . () []
	Primary,
}

impl TokenPrecedence {
	fn next(self) -> Self {
		const PRIMARY: u8 = TokenPrecedence::Primary as u8;
		let p = self as u8;
		let p1 = p + 1;
		let p = if p1 > PRIMARY { PRIMARY } else { p1 };
		unsafe { std::mem::transmute(p) }
	}
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
	source: Peekable<Chars<'a>>,
	peeked_tokens: VecDeque<Token>,

	line: usize,
	coloumn: usize,
	token_location: CodeLocation,
}

impl<'a> Tokenizer<'a> {
	pub fn new(source: Peekable<Chars<'a>>) -> Self {
		Self {
			source,
			peeked_tokens: VecDeque::new(),
			line: 0,
			coloumn: 0,
			token_location: CodeLocation::new(0, 0, "<TODO>".to_string()),
		}
	}

	fn record_token_location(&mut self) {
		self.token_location.line = self.line;
		self.token_location.col = self.coloumn;
	}

	fn current_location(&self) -> CodeLocation {
		CodeLocation::new(self.line, self.coloumn, "<TODO>".to_string())
	}
}

impl<'a> Tokenizer<'a> {
	fn peek_char(&mut self) -> Option<&char> {
		self.source.peek()
	}

	fn next_char(&mut self) -> Option<char> {
		let maybe_next_char = self.source.next();
		if maybe_next_char.is_some() {
			self.coloumn += 1;
		}
		maybe_next_char
	}

	fn next_char_if<F>(&mut self, f: F) -> Option<char>
	where
		F: FnOnce(&char) -> bool,
	{
		let maybe_next_char = self.source.next_if(f);
		if maybe_next_char.is_some() {
			self.coloumn += 1;
		}
		maybe_next_char
	}

	fn next_char_if_eq(&mut self, c: char) -> Option<char> {
		let maybe_next_char = self.source.next_if_eq(&c);
		if maybe_next_char.is_some() {
			self.coloumn += 1;
		}
		maybe_next_char
	}

	pub fn peek(&mut self) -> Result<Option<&Token>, String> {
		if self.peeked_tokens.is_empty() {
			let t = self.next()?;
			if let Some(t) = t {
				self.peeked_tokens.push_back(t);
			} else {
				return Ok(None);
			}
		}
		Ok(Some(
			self
				.peeked_tokens
				.front()
				.expect("We checked for empty vec."),
		))
	}

	pub fn peek_n(&mut self, n: usize) -> Result<Option<&Token>, String> {
		while self.peeked_tokens.len() <= n {
			let t = self.next_no_peeking()?;
			if let Some(t) = t {
				self.peeked_tokens.push_back(t);
			} else {
				return Ok(None);
			}
		}
		Ok(Some(&self.peeked_tokens[n]))
	}

	pub fn next(&mut self) -> Result<Option<Token>, String> {
		if !self.peeked_tokens.is_empty() {
			let t = self
				.peeked_tokens
				.pop_front()
				.expect("Already checked for empty vec.");
			return Ok(Some(t));
		}
		self.next_no_peeking()
	}

	fn next_no_peeking(&mut self) -> Result<Option<Token>, String> {
		self.skip_to_begining_of_next_token();
		self.record_token_location();

		if let Some(&c) = self.peek_char() {
			if c == '\0' {
				Ok(None)
			} else if c == '\n' {
				self.next_char().expect("We've already peeked this");
				Ok(Some(Token::new(
					TokenData::Newline,
					self.token_location.clone(),
				)))
			} else if c.is_ascii_digit() {
				Ok(Some(self.tokenize_number()?))
			} else if c.is_alphabetic() {
				Ok(Some(self.tokenize_identifier_or_keyword()))
			} else {
				Ok(Some(self.tokenize_punctuation()?))
			}
		} else {
			Ok(None)
		}
	}

	fn skip_to_begining_of_next_token(&mut self) {
		loop {
			match self.peek_char() {
				None => break,
				Some(c) => match c {
					'#' => {
						loop {
							if matches!(self.next_char(), Some('\n') | None) {
								break;
							}
						}
						self.line += 1;
						self.coloumn = 0;
					}
					'\n' => {
						self.source.next().expect("We already checked for `None`.");
						self.line += 1;
						self.coloumn = 0;
					}
					_ if !c.is_whitespace() => break,
					_ => {
						self.next_char().expect("We already checked for `None`.");
					}
				},
			}
		}
	}

	fn tokenize_number(&mut self) -> Result<Token, String> {
		let mut word = String::new();

		while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
			word.push(c);
		}

		let source = self.source.clone();
		if let Some(true) = self.next_char_if_eq('.').and_then(|_| {
			if let Some(c) = self.peek_char() {
				Some(c.is_ascii_digit())
			} else {
				Some(false)
			}
		}) {
			word.push('.');

			while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
				word.push(c);
			}

			Ok(Token::new(
				TokenData::Num(word.parse().or_else(|err| Err(format!("{}", err)))?),
				self.token_location.clone(),
			))
		} else {
			self.source = source;
			Ok(Token::new(
				TokenData::Int(word.parse().or_else(|err| Err(format!("{}", err)))?),
				self.token_location.clone(),
			))
		}
	}

	fn tokenize_identifier_or_keyword(&mut self) -> Token {
		todo!()
	}

	fn tokenize_punctuation(&mut self) -> Result<Token, String> {
		let token_location = self.token_location.clone();
		match self.next_char().ok_or("self.source.next() failed!")? {
			';' => Ok(Token::new(TokenData::Semicolon, token_location)),
			'+' => Ok(Token::new(TokenData::Plus, token_location)),
			'-' => Ok(Token::new(TokenData::Dash, token_location)),
			'*' => Ok(Token::new(TokenData::Star, token_location)),
			'/' => Ok(Token::new(TokenData::Slash, token_location)),
			c => Err(format!(
				"{}: Unknown operator `{}`.",
				self.current_location(),
				c
			)),
		}
	}
}

pub struct Parser<'a> {
	ast: AST,
	tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
	pub fn new(source: Peekable<Chars<'a>>) -> Self {
		Self {
			ast: AST::new(),
			tokenizer: Tokenizer::new(source),
		}
	}

	fn expect_statement_terminator(&mut self, err: impl Into<String>) -> Result<Token, String> {
		let token = self.tokenizer.next()?;

		use TokenData::*;
		match token {
			None
			| Some(Token {
				data: Newline,
				location: _,
			})
			| Some(Token {
				data: Semicolon,
				location: _,
			}) => Ok(token.unwrap()),
			_ => Err(err.into()),
		}
	}
}

impl<'a> Parser<'a> {
	fn parse_declaration(&mut self) -> Result<usize, String> {
		if false {
			todo!("THis is where declaration parsing functions will go.");
		} else {
			self.parse_statement()
		}
	}

	fn parse_statement(&mut self) -> Result<usize, String> {
		if false {
			todo!("This is where statement parsing functions will go.");
		} else {
			let idx = self.parse_expression_or_assignment()?;
			self.expect_statement_terminator("Expected end of statement!")?;
			Ok(idx)
		}
	}

	fn parse_expression_or_assignment(&mut self) -> Result<usize, String> {
		self.parse_precedence(TokenPrecedence::Assignment)
	}

	fn parse_expression(&mut self) -> Result<usize, String> {
		let idx = self.parse_expression_or_assignment()?;

		todo!("Check not assignment");

		// idx
	}

	fn parse_precedence(&mut self, precedence: TokenPrecedence) -> Result<usize, String> {
		let token = self.tokenizer.next()?.ok_or("Unexpected end of file!")?;
		let mut previous = self.parse_prefix(token)?;
		while precedence
			<= self
				.tokenizer
				.peek()?
				.ok_or("Unexpected end of file!")?
				.precedence()
		{
			let token = self.tokenizer.next()?.ok_or("Unexpected end of file!")?;
			previous = self.parse_infix(token, previous)?;
		}

		Ok(previous)
	}

	fn parse_prefix(&mut self, token: Token) -> Result<usize, String> {
		use TokenData::*;
		match token.data {
			Bool(value) => Ok(self.ast.add_node(Node::new_bool(value), token.location)),
			Int(value) => Ok(self.ast.add_node(Node::new_int(value), token.location)),
			Num(value) => Ok(self.ast.add_node(Node::new_num(value), token.location)),
			Str(_value) => todo!(),
			_ => Err(format!("`{:?}` is not a prefix operation!", token)),
		}
	}

	fn parse_infix(&mut self, token: Token, previous: usize) -> Result<usize, String> {
		use TokenData::*;
		match token.data {
			Plus => self.parse_binary(token.precedence(), NodeKind::Add, previous, token.location),
			Dash => self.parse_binary(
				token.precedence(),
				NodeKind::Subtract,
				previous,
				token.location,
			),
			Star => self.parse_binary(
				token.precedence(),
				NodeKind::Multiply,
				previous,
				token.location,
			),
			Slash => self.parse_binary(
				token.precedence(),
				NodeKind::Divide,
				previous,
				token.location,
			),
			_ => Err(format!("`{:?}` is not an infix operation!", token)),
		}
	}

	fn parse_binary(
		&mut self,
		precedence: TokenPrecedence,
		kind: NodeKind,
		lhs: usize,
		location: CodeLocation,
	) -> Result<usize, String> {
		let precedence = precedence.next();
		let rhs = self.parse_precedence(precedence)?;
		Ok(self.ast.add_node(Node::new(kind, lhs, rhs), location))
	}
}

pub fn parse<'a>(source: Peekable<Chars<'a>>, filename: Option<String>) -> Result<AST, String> {
	let mut p = Parser::new(source);
	while p.tokenizer.peek()?.is_some() {
		let root = p.parse_declaration()?;
		p.ast.add_root(root);
	}
	Ok(p.ast)
}
