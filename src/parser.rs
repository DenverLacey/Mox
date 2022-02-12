use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
use std::iter::Peekable;
use std::str::Chars;

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

impl Debug for CodeLocation {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}:{}:{}", self.line, self.col, self.file)
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

	// Operators
	Plus,
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
	source: Peekable<Chars<'a>>,
	peeked_tokens: VecDeque<Token>,

	line: usize,
	coloumn: usize,
}

impl<'a> Tokenizer<'a> {
	pub fn new(source: Peekable<Chars<'a>>) -> Self {
		Self {
			source,
			peeked_tokens: VecDeque::new(),
			line: 0,
			coloumn: 0,
		}
	}
}

impl<'a> Tokenizer<'a> {
	pub fn peek(&mut self) -> Option<&Token> {
		if self.peeked_tokens.is_empty() {
			let t = self.next()?;
			self.peeked_tokens.push_back(t);
		}
		Some(
			self
				.peeked_tokens
				.front()
				.expect("We checked for empty vec."),
		)
	}

	pub fn peek_n(&mut self, n: usize) -> Option<&Token> {
		while self.peeked_tokens.len() <= n {
			let t = self.next_no_peeking()?;
			self.peeked_tokens.push_back(t);
		}
		Some(&self.peeked_tokens[n])
	}

	pub fn next(&mut self) -> Option<Token> {
		if !self.peeked_tokens.is_empty() {
			let t = self
				.peeked_tokens
				.pop_front()
				.expect("Already checked for empty vec.");
			return Some(t);
		}
		self.next_no_peeking()
	}

	fn next_no_peeking(&mut self) -> Option<Token> {
		self.skip_to_begining_of_next_token();

		if let Some(&c) = self.source.peek() {
			if c == '\0' {
				None
			} else if c == '\n' {
				self.source.next().expect("We've already peeked this");
				Some(Token::new(
					TokenData::Newline,
					CodeLocation::new(0, 0, String::new()),
				))
			} else if c.is_ascii_digit() {
				self.tokenize_number()
			} else if c.is_alphabetic() {
				Some(self.tokenize_identifier_or_keyword())
			} else {
				self.tokenize_punctuation()
			}
		} else {
			None
		}
	}

	fn skip_to_begining_of_next_token(&mut self) {
		loop {
			match self.source.peek() {
				None => break,
				Some(c) => match c {
					'#' => {
						loop {
							if matches!(self.source.next(), Some('\n') | None) {
								break;
							}
						}
						self.line += 1;
						self.coloumn = 0;
					}
					_ if !c.is_whitespace() => break,
					_ => {
						self.source.next().expect("We already checked for `None`.");
						self.coloumn += 1;
					}
				},
			}
		}
	}

	fn tokenize_number(&mut self) -> Option<Token> {
		let mut word = String::new();

		while let Some(c) = self.source.next_if(|c| c.is_ascii_digit()) {
			word.push(c);
		}

		let source = self.source.clone();
		if let Some(true) = self.source.next_if_eq(&'.').and_then(|_| {
			if let Some(c) = self.source.peek() {
				Some(c.is_ascii_digit())
			} else {
				Some(false)
			}
		}) {
			word.push('.');
			while let Some(c) = self.source.next_if(|c| c.is_ascii_digit()) {
				word.push(c);
			}

			Some(Token::new(
				TokenData::Num(word.parse().ok()?),
				CodeLocation::new(0, 0, String::new()),
			))
		} else {
			self.source = source;
			Some(Token::new(
				TokenData::Int(word.parse().ok()?),
				CodeLocation::new(0, 0, String::new()),
			))
		}
	}

	fn tokenize_identifier_or_keyword(&mut self) -> Token {
		todo!()
	}

	fn tokenize_punctuation(&mut self) -> Option<Token> {
		match self.source.next().expect("self.source.next() failed!") {
			'+' => Some(Token::new(
				TokenData::Plus,
				CodeLocation::new(0, 0, String::new()),
			)),
			_ => None,
		}
	}
}
