use crate::ast::*;
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
		write!(f, "{}:{}:{}", self.file, self.line + 1, self.col + 1)
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
			Comma => TokenPrecedence::None,
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
			Def => TokenPrecedence::None,
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
	Comma,
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
	Def,
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
			Comma => 6,
			Semicolon => 7,
			LeftParen => 8,
			RightParen => 9,
			LeftCurly => 10,
			RightCurly => 11,
			Plus => 12,
			Dash => 13,
			Star => 14,
			Slash => 15,
			If => 16,
			Else => 17,
			While => 18,
			Def => 19,
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
			Comma => write!(f, ","),
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
			Def => write!(f, "def"),
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

	fn is_string_begin(c: char) -> bool {
		c == '"' || c == '\''
	}
}

impl<'a> Tokenizer<'a> {
	fn peek_char(&mut self) -> Option<&char> {
		self.source.peek()
	}

	fn next_char(&mut self) -> Option<char> {
		let maybe_next_char = self.source.next();
		if let Some(c) = maybe_next_char {
			if c == '\n' {
				self.line += 1;
				self.coloumn = 0;
			} else {
				self.coloumn += 1;
			}
		}
		maybe_next_char
	}

	fn next_char_if<F>(&mut self, f: F) -> Option<char>
	where
		F: FnOnce(&char) -> bool,
	{
		let maybe_next_char = self.source.next_if(f);
		if let Some(c) = maybe_next_char {
			if c == '\n' {
				self.line += 1;
				self.coloumn = 0;
			} else {
				self.coloumn += 1;
			}
		}
		maybe_next_char
	}

	fn next_char_if_eq(&mut self, c: char) -> Option<char> {
		let maybe_next_char = self.source.next_if_eq(&c);
		if let Some(c) = maybe_next_char {
			if c == '\n' {
				self.line += 1;
				self.coloumn = 0;
			} else {
				self.coloumn += 1;
			}
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
			} else if Self::is_string_begin(c) {
				Ok(Some(self.tokenize_string()?))
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
					'#' => loop {
						if matches!(self.next_char(), Some('\n') | None) {
							break;
						}
					},
					'\n' => {
						if !self.previous_was_newline {
							break;
						}

						self.next_char().expect("We already checked for `None`.");
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

	fn tokenize_string(&mut self) -> Result<Token> {
		let terminator = self
			.next_char()
			.expect("We should have peeked in the outer loop");

		let mut word = String::new();

		loop {
			match self.next_char() {
				None => return SimpleErrAt(self.token_location.clone(), "Unended string literal."),
				Some('\\') => match self.next_char() {
					None => return SimpleErrAt(self.token_location.clone(), "Unended string literal."),
					Some('0') => word.push('\0'),
					Some('e') => todo!(),
					Some('n') => word.push('\n'),
					Some('t') => word.push('\t'),
					Some('u') => todo!(),
					Some('U') => todo!(),
					Some('x') => todo!(),
					Some('X') => todo!(),
					Some('\\') => word.push('\\'),
					Some('"') => word.push('"'),
					Some('\'') => word.push('\''),
					Some(c) => {
						return ErrAt(
							self.current_location(),
							format!("`\\{}` is not a valid escape sequence.", c),
						)
					}
				},
				Some(c) if c == terminator => break,
				Some(c) => word.push(c),
			}
		}

		Ok(Token::new(
			TokenData::Str(word),
			self.token_location.clone(),
		))
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
			"def" => Token::new(TokenData::Def, self.token_location.clone()),
			_ => Token::new(TokenData::Ident(word), self.token_location.clone()),
		}
	}

	fn tokenize_punctuation(&mut self) -> Result<Token> {
		let token_location = self.token_location.clone();
		match self.next_char().ok_or(Error::SimpleErrAt(
			token_location.clone(),
			"self.source.next() failed.",
		))? {
			',' => Ok(Token::new(TokenData::Comma, token_location)),
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

	fn check_and_consume(&mut self, data_kind: &TokenData) -> Result<Option<Token>> {
		if self.check(data_kind)? {
			Ok(self.tokenizer.next().expect("We peek in check"))
		} else {
			Ok(None)
		}
	}

	fn skip_check_and_consume(&mut self, data_kind: &TokenData) -> Result<Option<Token>> {
		self.skip_newlines()?;

		if self.check(data_kind)? {
			Ok(self.tokenizer.next().expect("We peek in check"))
		} else {
			Ok(None)
		}
	}

	fn peek_check_and_consume(&mut self, data_kind: &TokenData) -> Result<Option<Token>> {
		let i = self.peek_newlines()?;
		if let Some(t) = self.tokenizer.peek_n(i)? {
			if data_kind.eq_kind(&t.data) {
				self.flush_peeked_newlines();
				return Ok(self.tokenizer.next().expect("We already peeked"));
			}
		}
		return Ok(None);
	}

	fn expect(&mut self, data_kind: &TokenData, err: impl Into<String>) -> Result<Token> {
		let next = self
			.tokenizer
			.next()?
			.ok_or(Error::SimpleErr("Unexpected end of file."))?;
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
	}
}

impl<'a> Parser<'a> {
	fn parse_declaration(&mut self) -> Result<usize> {
		if self.check(&TokenData::Def)? {
			self.parse_def()
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
			SimpleErr("Cannot assign in expression context.")
		} else {
			Ok(idx)
		}
	}

	fn parse_precedence(&mut self, precedence: TokenPrecedence) -> Result<usize> {
		let token = self
			.tokenizer
			.next()?
			.ok_or(Error::SimpleErr("Unexpected end of file."))?;
		let mut previous = self.parse_prefix(token)?;
		while precedence
			<= self
				.tokenizer
				.peek()?
				.ok_or(Error::SimpleErr("Unexpected end of file."))?
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
					.ok_or(Error::SimpleErr("Unexpected end of input."))?;
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
				format!("`{}` is not a prefix operation.", token),
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
				format!("`{}` is not an infix operation.", token),
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

	fn parse_block(&mut self, kind: BlockKind) -> Result<usize> {
		let mut roots = Vec::new();

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

		let block = self.ast.add_block(NodeBlock::new(kind, roots));
		Ok(
			self
				.ast
				.add_node(Node::new(NodeKind::Block, block, 0), block_location),
		)
	}

	fn parse_if(&mut self, token: Token) -> Result<usize> {
		let if_location = token.location;

		self.skip_newlines()?;
		let condition = self.parse_expression()?;

		let then_block = self.parse_block(BlockKind::Block)?;
		let else_block = if self.peek_check_and_consume(&TokenData::Else)?.is_some() {
			if self.skip_check(&TokenData::If)? {
				let if_token = self
					.tokenizer
					.next()
					.expect("Peeked by `skip_check`")
					.expect("Peeked by `skip_check`");
				self.parse_if(if_token)?
			} else {
				self.parse_block(BlockKind::Block)?
			}
		} else {
			0
		};

		let if_data = self.ast.add_extra_data(ExtraDataIf {
			condition,
			then_block,
			else_block,
		});

		Ok(
			self
				.ast
				.add_node(Node::new(NodeKind::If, if_data, 0), if_location),
		)
	}

	fn parse_while(&mut self, token: Token) -> Result<usize> {
		let while_location = token.location;

		self.skip_newlines()?;
		let condition = self.parse_expression()?;
		let body = self.parse_block(BlockKind::Block)?;

		Ok(
			self
				.ast
				.add_node(Node::new(NodeKind::While, condition, body), while_location),
		)
	}

	fn parse_def(&mut self) -> Result<usize> {
		let def_location = self
			.tokenizer
			.next()
			.expect("We already peeked in `parse_declaration`")
			.expect("We already peeked in `parse_declaration`")
			.location;

		let ident = self.expect(
			&TokenData::Ident(String::new()),
			format!("Expected an identifer after keyword `{}`.", TokenData::Def),
		)?;

		let ident = if let Token {
			data: TokenData::Ident(ident),
			location: _,
		} = ident
		{
			ident
		} else {
			unreachable!("We already know its an Ident");
		};

		let ident = self.ast.add_string(ident);

		self.expect(
			&TokenData::LeftParen,
			format!(
				"Expected `{}` to begin parameter list.",
				TokenData::LeftParen
			),
		)?;

		let mut param_idents = Vec::new();
		let mut param_location = None;
		loop {
			if self.check(&TokenData::RightParen)? || self.check_eof()? {
				break;
			}

			let token = self.expect(&TokenData::Ident(String::new()), "Expected parameter name.")?;

			if param_location.is_none() {
				param_location = Some(token.location);
			}

			if let TokenData::Ident(ident) = token.data {
				let ident = self.ast.add_string(ident);
				param_idents.push(ident);
			} else {
				unreachable!();
			}

			if !self.check_and_consume(&TokenData::Comma)?.is_some() || self.check_eof()? {
				break;
			}
		}

		let params = if param_idents.is_empty() {
			0
		} else {
			let param_block = self
				.ast
				.add_block(NodeBlock::new(BlockKind::Comma, param_idents));
			self.ast.add_node(
				Node::new(NodeKind::Block, param_block, 0),
				param_location.expect("Should be set if there are params"),
			)
		};

		self.expect(
			&TokenData::RightParen,
			format!(
				"Expected `{}` to terminate parameter list.",
				TokenData::RightParen
			),
		)?;

		let body = self.parse_block(BlockKind::Block)?;

		let def_data = self.ast.add_extra_data(ExtraDataDef {
			ident,
			params,
			body,
		});

		Ok(
			self
				.ast
				.add_node(Node::new(NodeKind::Def, def_data, 0), def_location),
		)
	}
}

pub fn parse<'a>(source: Peekable<Chars<'a>>, filename: String) -> Result<AST> {
	let mut p = Parser::new(source, filename);
	loop {
		p.skip_newlines()?;
		if p.tokenizer.peek()?.is_none() {
			break;
		}

		let root = p.parse_declaration()?;
		p.ast.add_root(root);
	}
	Ok(p.ast)
}
