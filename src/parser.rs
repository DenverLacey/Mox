use crate::ast::{self, Node, NodeKind, AST};
use crate::error::*;
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
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
			Ident(_) => TokenPrecedence::None,

			// Delimeters
			Newline => TokenPrecedence::None,
			Semicolon => TokenPrecedence::None,
			LeftParen => TokenPrecedence::Call,
			RightParen => TokenPrecedence::None,
			LeftCurly => TokenPrecedence::None,
			RightCurly => TokenPrecedence::None,

			// Operators
			Plus => TokenPrecedence::Term,
			Dash => TokenPrecedence::Term,
			Star => TokenPrecedence::Factor,
			Slash => TokenPrecedence::Factor,

			// Keywords
			If => TokenPrecedence::None,
			Else => TokenPrecedence::None,
			While => TokenPrecedence::None,
		}
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.data)
	}
}

#[derive(Debug)]
pub enum TokenData {
	// Literals
	Bool(bool),
	Int(i64),
	Num(f64),
	Str(String),
	Ident(String),

	// Delimeters
	Newline,
	Semicolon,
	LeftParen,
	RightParen,
	LeftCurly,
	RightCurly,

	// Operators
	Plus,
	Dash,
	Star,
	Slash,

	// Keywords
	If,
	Else,
	While,
}

impl TokenData {
	const fn tag(&self) -> u8 {
		use TokenData::*;
		match self {
			Bool(_) => 0,
			Int(_) => 1,
			Num(_) => 2,
			Str(_) => 3,
			Ident(_) => 4,
			Newline => 5,
			Semicolon => 6,
			LeftParen => 7,
			RightParen => 8,
			LeftCurly => 9,
			RightCurly => 10,
			Plus => 11,
			Dash => 12,
			Star => 13,
			Slash => 14,
			If => 15,
			Else => 16,
			While => 17,
		}
	}

	fn eq_kind(&self, other: &Self) -> bool {
		self.tag() == other.tag()
	}
}

impl Display for TokenData {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use TokenData::*;
		match self {
			// Literals
			Bool(value) => write!(f, "{}", value),
			Int(value) => write!(f, "{}", value),
			Num(value) => write!(f, "{}", value),
			Str(value) => write!(f, "{}", value),
			Ident(ident) => write!(f, "{}", ident),

			// Delimeters
			Newline => write!(f, "newline"),
			Semicolon => write!(f, ";"),
			LeftParen => write!(f, "("),
			RightParen => write!(f, ")"),
			LeftCurly => write!(f, "{{"),
			RightCurly => write!(f, "}}"),

			// Operators
			Plus => write!(f, "+"),
			Dash => write!(f, "-"),
			Star => write!(f, "*"),
			Slash => write!(f, "/"),

			// Keywords
			If => write!(f, "if"),
			Else => write!(f, "else"),
			While => write!(f, "while"),
		}
	}
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
	filename: String,
	peeked_tokens: VecDeque<Token>,
	previous_was_newline: bool,

	line: usize,
	coloumn: usize,
	token_location: CodeLocation,
}

impl<'a> Tokenizer<'a> {
	pub fn new(source: Peekable<Chars<'a>>, filename: String) -> Self {
		let token_location_filename = filename.clone();
		Self {
			source,
			filename,
			peeked_tokens: VecDeque::new(),
			previous_was_newline: true, // to skip leading newlines in source file
			line: 0,
			coloumn: 0,
			token_location: CodeLocation::new(0, 0, token_location_filename),
		}
	}

	fn record_token_location(&mut self) {
		self.token_location.line = self.line;
		self.token_location.col = self.coloumn;
	}

	fn current_location(&self) -> CodeLocation {
		CodeLocation::new(self.line, self.coloumn, self.filename.clone())
	}
}

impl<'a> Tokenizer<'a> {
	fn is_ident_begin(c: &char) -> bool {
		c.is_alphabetic() || *c == '_'
	}

	fn is_ident_character(c: &char) -> bool {
		Self::is_ident_begin(c) || c.is_ascii_digit()
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

	pub fn peek(&mut self) -> Result<Option<&Token>> {
		if self.peeked_tokens.is_empty() {
			let t = self.next()?;
			if let Some(t) = t {
				self.peeked_tokens.push_back(t);
			} else {
				return Ok(None);
			}
		}
		Ok(self.peeked_tokens.front())
	}

	pub fn peek_n(&mut self, n: usize) -> Result<Option<&Token>> {
		while self.peeked_tokens.len() <= n {
			if let Some(t) = self.next_no_peeking()? {
				self.peeked_tokens.push_back(t);
			} else {
				return Ok(None);
			}
		}
		Ok(Some(&self.peeked_tokens[n]))
	}

	pub fn next(&mut self) -> Result<Option<Token>> {
		if !self.peeked_tokens.is_empty() {
			return Ok(self.peeked_tokens.pop_front());
		}
		self.next_no_peeking()
	}

	fn next_no_peeking(&mut self) -> Result<Option<Token>> {
		self.skip_to_begining_of_next_token();
		self.record_token_location();
		self.previous_was_newline = false;

		if let Some(&c) = self.peek_char() {
			if c == '\0' {
				Ok(None)
			} else if c == '\n' {
				self.previous_was_newline = true;
				self.next_char().expect("We've already peeked this");
				Ok(Some(Token::new(
					TokenData::Newline,
					self.token_location.clone(),
				)))
			} else if c.is_ascii_digit() {
				Ok(Some(self.tokenize_number()?))
			} else if Self::is_ident_begin(&c) {
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
						if !self.previous_was_newline {
							break;
						}

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

	fn tokenize_number(&mut self) -> Result<Token> {
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
				TokenData::Num(
					word
						.parse()
						.or_else(|err| ErrAt(self.token_location.clone(), format!("{}", err)))?,
				),
				self.token_location.clone(),
			))
		} else {
			self.source = source;
			Ok(Token::new(
				TokenData::Int(
					word
						.parse()
						.or_else(|err| ErrAt(self.token_location.clone(), format!("{}", err)))?,
				),
				self.token_location.clone(),
			))
		}
	}

	fn tokenize_identifier_or_keyword(&mut self) -> Token {
		let mut word = String::new();
		while let Some(c) = self.next_char_if(Self::is_ident_character) {
			word.push(c);
		}

		match word.as_str() {
			"if" => Token::new(TokenData::If, self.token_location.clone()),
			"else" => Token::new(TokenData::Else, self.token_location.clone()),
			"while" => Token::new(TokenData::While, self.token_location.clone()),
			_ => Token::new(TokenData::Ident(word), self.token_location.clone()),
		}
	}

	fn tokenize_punctuation(&mut self) -> Result<Token> {
		let token_location = self.token_location.clone();
		match self.next_char().ok_or(Error::SimpleErrAt(
			token_location.clone(),
			"self.source.next() failed!",
		))? {
			';' => Ok(Token::new(TokenData::Semicolon, token_location)),
			'(' => Ok(Token::new(TokenData::LeftParen, token_location)),
			')' => Ok(Token::new(TokenData::RightParen, token_location)),
			'{' => Ok(Token::new(TokenData::LeftCurly, token_location)),
			'}' => Ok(Token::new(TokenData::RightCurly, token_location)),
			'+' => Ok(Token::new(TokenData::Plus, token_location)),
			'-' => Ok(Token::new(TokenData::Dash, token_location)),
			'*' => Ok(Token::new(TokenData::Star, token_location)),
			'/' => Ok(Token::new(TokenData::Slash, token_location)),
			c => ErrAt(token_location, format!("Unknown operator `{}`.", c)),
		}
	}
}

pub struct Parser<'a> {
	ast: AST,
	tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
	pub fn new(source: Peekable<Chars<'a>>, filename: String) -> Self {
		Self {
			ast: AST::new(),
			tokenizer: Tokenizer::new(source, filename),
		}
	}
}

impl<'a> Parser<'a> {
	fn check(&mut self, data_kind: &TokenData) -> Result<bool> {
		let next = self.tokenizer.peek()?;
		if let Some(next) = next {
			Ok(next.data.tag() == data_kind.tag())
		} else {
			Ok(false)
		}
	}

	fn skip_check(&mut self, data_kind: &TokenData) -> Result<bool> {
		self.skip_newlines()?;

		let next = self.tokenizer.peek()?;
		if let Some(next) = next {
			Ok(next.data.tag() == data_kind.tag())
		} else {
			Ok(false)
		}
	}

	fn peek_check(&mut self, data_kind: &TokenData) -> Result<bool> {
		let i = self.peek_newlines()?;
		if let Some(t) = self.tokenizer.peek_n(i)? {
			Ok(data_kind.eq_kind(&t.data))
		} else {
			Ok(false)
		}
	}

	fn check_eof(&mut self) -> Result<bool> {
		Ok(self.tokenizer.peek()?.is_none())
	}

	fn skip_check_eof(&mut self) -> Result<bool> {
		self.skip_newlines()?;
		Ok(self.tokenizer.peek()?.is_none())
	}

	fn peek_check_eof(&mut self) -> Result<bool> {
		let i = self.peek_newlines()?;
		Ok(self.tokenizer.peek_n(i)?.is_none())
	}

	fn check_and_consume(&mut self, data_kind: &TokenData) -> Result<bool> {
		if self.check(data_kind)? {
			self.tokenizer.next().expect("We peek in check");
			Ok(true)
		} else {
			Ok(false)
		}
	}

	fn skip_check_and_consume(&mut self, data_kind: &TokenData) -> Result<bool> {
		self.skip_newlines()?;

		if self.check(data_kind)? {
			self.tokenizer.next().expect("We peek in check");
			Ok(true)
		} else {
			Ok(false)
		}
	}

	fn peek_check_and_consume(&mut self, data_kind: &TokenData) -> Result<bool> {
		let i = self.peek_newlines()?;
		if let Some(t) = self.tokenizer.peek_n(i)? {
			if data_kind.eq_kind(&t.data) {
				self.flush_peeked_newlines();
				self
					.tokenizer
					.next()
					.expect("We already peeked")
					.expect("We already peeked");
				return Ok(true);
			}
		}
		return Ok(false);
	}

	fn expect(&mut self, data_kind: &TokenData, err: impl Into<String>) -> Result<Token> {
		let next = self
			.tokenizer
			.next()?
			.ok_or(Error::SimpleErr("Unexpected end of file!"))?;
		println!("\texpect: next = {:?}", next);
		if !next.data.eq_kind(data_kind) {
			ErrAt(next.location, err.into())
		} else {
			Ok(next)
		}
	}

	fn skip_expect(&mut self, data_kind: &TokenData, err: impl Into<String>) -> Result<Token> {
		self.skip_newlines()?;
		self.expect(data_kind, err)
	}

	fn peek_expect(&mut self, data_kind: &TokenData, err: impl Into<String>) -> Result<Token> {
		let i = self.peek_newlines()?;
		if let Some(t) = self.tokenizer.peek_n(i)? {
			if t.data.eq_kind(data_kind) {
				ErrAt(t.location.clone(), err.into())
			} else {
				self.flush_peeked_newlines();
				Ok(
					self
						.tokenizer
						.next()
						.expect("We already peeked")
						.expect("We already peeked"),
				)
			}
		} else {
			Err(err.into())
		}
	}

	fn expect_statement_terminator(&mut self, err: impl Into<String>) -> Result<Token> {
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
			Some(t) => ErrAt(t.location, err.into()),
		}
	}

	fn skip_newlines(&mut self) -> Result<()> {
		while let Some(&Token {
			data: TokenData::Newline,
			location: _,
		}) = self.tokenizer.peek()?
		{
			self.tokenizer.next().expect("We already peeked!");
		}

		Ok(())
	}

	fn peek_newlines(&mut self) -> Result<usize> {
		let mut i = 0;
		while let Some(Token {
			data: TokenData::Newline,
			location: _,
		}) = self.tokenizer.peek_n(i)?
		{
			i += 1;
		}

		Ok(i)
	}

	fn flush_peeked_newlines(&mut self) {
		println!("Before Flush:\n\t{:?}", self.tokenizer.peeked_tokens);

		while let Some(Token {
			data: TokenData::Newline,
			location: _,
		}) = self.tokenizer.peeked_tokens.front()
		{
			self
				.tokenizer
				.peeked_tokens
				.pop_front()
				.expect("We checked the front for `None`");
		}

		println!("After Flush:\n\t{:?}", self.tokenizer.peeked_tokens);
	}
}

impl<'a> Parser<'a> {
	fn parse_declaration(&mut self) -> Result<usize> {
		if false {
			todo!("THis is where declaration parsing functions will go.");
		} else {
			self.parse_statement()
		}
	}

	fn parse_statement(&mut self) -> Result<usize> {
		if false {
			todo!("This is where statement parsing functions will go.");
		} else {
			let idx = self.parse_expression_or_assignment()?;
			self.expect_statement_terminator("Expected end of statement!")?;
			Ok(idx)
		}
	}

	fn parse_expression_or_assignment(&mut self) -> Result<usize> {
		self.parse_precedence(TokenPrecedence::Assignment)
	}

	fn parse_expression(&mut self) -> Result<usize> {
		let idx = self.parse_expression_or_assignment()?;

		if self.ast.get(idx).kind == NodeKind::Assign {
			SimpleErr("Cannot assign in expression context!")
		} else {
			Ok(idx)
		}
	}

	fn parse_precedence(&mut self, precedence: TokenPrecedence) -> Result<usize> {
		let token = self
			.tokenizer
			.next()?
			.ok_or(Error::SimpleErr("Unexpected end of file!"))?;
		let mut previous = self.parse_prefix(token)?;
		while precedence
			<= self
				.tokenizer
				.peek()?
				.ok_or(Error::SimpleErr("Unexpected end of file!"))?
				.precedence()
		{
			let token = self
				.tokenizer
				.next()?
				.expect("We already checked in the condition of while");
			previous = self.parse_infix(token, previous)?;
		}

		Ok(previous)
	}

	fn parse_prefix(&mut self, token: Token) -> Result<usize> {
		use TokenData::*;
		match token.data {
			// Literals
			Bool(value) => Ok(self.ast.add_node(Node::new_bool(value), token.location)),
			Int(value) => Ok(self.ast.add_node(Node::new_int(value), token.location)),
			Num(value) => Ok(self.ast.add_node(Node::new_num(value), token.location)),
			Str(value) => {
				let index = self.ast.add_string(value);
				Ok(self.ast.add_node(Node::new_str(index), token.location))
			}
			Ident(ident) => {
				let index = self.ast.add_string(ident);
				Ok(self.ast.add_node(Node::new_ident(index), token.location))
			}

			// Delimeters
			LeftParen => {
				let idx = self.parse_expression()?;
				self.expect(
					&TokenData::RightParen,
					format!(
						"Expected `{}` to terminate parenthesised expression.",
						TokenData::RightParen
					),
				)?;
				Ok(idx)
			}

			// Operators
			Dash => {
				let next = self
					.tokenizer
					.peek()?
					.ok_or(Error::SimpleErr("Unexpected end of input!"))?;
				if let TokenData::Int(value) = next.data {
					self.tokenizer.next().expect("We just peeked this");
					Ok(self.ast.add_node(Node::new_int(-value), token.location))
				} else if let TokenData::Num(value) = next.data {
					self.tokenizer.next().expect("We just peeked this");
					Ok(self.ast.add_node(Node::new_num(-value), token.location))
				} else {
					self.parse_unary(NodeKind::Negate, token.location)
				}
			}

			// Keywords
			If => self.parse_if(token),
			While => self.parse_while(token),

			_ => ErrAt(
				token.location.clone(),
				format!("`{}` is not a prefix operation!", token),
			),
		}
	}

	fn parse_infix(&mut self, token: Token, previous: usize) -> Result<usize> {
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
			_ => ErrAt(
				token.location.clone(),
				format!("`{}` is not an infix operation!", token),
			),
		}
	}

	fn parse_unary(&mut self, kind: NodeKind, location: CodeLocation) -> Result<usize> {
		let sub = self.parse_precedence(TokenPrecedence::Unary)?;
		Ok(self.ast.add_node(Node::new(kind, sub, 0), location))
	}

	fn parse_binary(
		&mut self,
		precedence: TokenPrecedence,
		kind: NodeKind,
		lhs: usize,
		location: CodeLocation,
	) -> Result<usize> {
		let precedence = precedence.next();
		let rhs = self.parse_precedence(precedence)?;
		Ok(self.ast.add_node(Node::new(kind, lhs, rhs), location))
	}

	fn parse_block(&mut self, kind: NodeKind) -> Result<usize> {
		let mut roots = Vec::new();

		println!("parse_block");
		let block_location = self
			.skip_expect(
				&TokenData::LeftCurly,
				format!("Expected `{}` to begin block.", TokenData::LeftCurly),
			)?
			.location;

		loop {
			self.skip_newlines()?;
			if self.check(&TokenData::RightCurly)? || self.check_eof()? {
				break;
			}
			roots.push(self.parse_declaration()?);
		}

		self.expect(
			&TokenData::RightCurly,
			format!("Expected `{}` to terminate block.", TokenData::RightCurly),
		)?;

		let block = self.ast.add_block(ast::NodeBlock::new(kind, roots));
		Ok(self.ast.add_node(Node::new(kind, block, 0), block_location))
	}

	fn parse_if(&mut self, token: Token) -> Result<usize> {
		println!("ENTER `parse_if`");
		let if_location = token.location;

		self.skip_newlines()?;
		let condition = self.parse_expression()?;

		let then_block = self.parse_block(NodeKind::Block)?;
		let else_block = if self.peek_check_and_consume(&TokenData::Else)? {
			if self.skip_check(&TokenData::If)? {
				let if_token = self
					.tokenizer
					.next()
					.expect("Peeked by `skip_check`")
					.expect("Peeked by `skip_check`");
				self.parse_if(if_token)?
			} else {
				self.parse_block(NodeKind::Block)?
			}
		} else {
			0
		};

		let if_data = self.ast.add_extra_data(ast::ExtraDataIf {
			condition,
			then_block,
			else_block,
		});

		println!("EXIT `parse_if`");

		Ok(
			self
				.ast
				.add_node(Node::new(NodeKind::If, if_data, 0), if_location),
		)
	}

	fn parse_while(&mut self, token: Token) -> Result<usize> {
		println!("ENTER `parse_while`");

		let while_location = token.location;

		self.skip_newlines()?;
		let condition = self.parse_expression()?;
		let body = self.parse_block(NodeKind::Block)?;

		println!("EXIT `parse_while`");

		Ok(
			self
				.ast
				.add_node(Node::new(NodeKind::While, condition, body), while_location),
		)
	}
}

pub fn parse<'a>(source: Peekable<Chars<'a>>, filename: String) -> Result<AST> {
	let mut p = Parser::new(source, filename);
	while p.tokenizer.peek()?.is_some() {
		let root = p.parse_declaration()?;
		p.ast.add_root(root);
	}
	Ok(p.ast)
}
