use std::iter::Peekable;
use std::str::Chars;
use std::collections::VecDeque;
use std::fmt::{Formatter, Debug};

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
	pub location: CodeLocation,
	pub data: TokenData,
}

#[derive(Debug)]
pub enum TokenData {
	Bool(bool),
	Int(i64),
	Num(f64),
	Str(String),
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
	source: Peekable<Chars<'a>>,
	peeked_tokens: VecDeque<Token>,
}

impl<'a> Tokenizer<'a> {
	pub fn new(source: Peekable<Chars<'a>>) -> Self {
		Self { source, peeked_tokens: VecDeque::new() }
	}
}

impl<'a> Tokenizer<'a> {
	fn peek(&mut self) -> &Token {
		if self.peeked_tokens.is_empty() {
			let t = self.next();
			self.peeked_tokens.push_back(t);
		}
		self.peeked_tokens.front().expect("We checked for empty vec.")
	}

	fn peek_n(&mut self, n: usize) -> &Token {
		while self.peeked_tokens.len() <= n {
			let t = self.next();
			self.peeked_tokens.push_back(t);
		}
		&self.peeked_tokens[n]
	}

	fn next(&mut self) -> Token {
		if !self.peeked_tokens.is_empty() {
			let t = self.peeked_tokens.pop_front().expect("Already checked for empty vec.");
			return t;
		}

		todo!()
	}
}
