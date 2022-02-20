use crate::parser::CodeLocation;
use std::fmt::{Debug, Display, Formatter};

pub enum Error {
	SimpleErr(&'static str),
	Err(String),
	SimpleErrAt(CodeLocation, &'static str),
	ErrAt(CodeLocation, String),
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn SimpleErr<T>(err: &'static str) -> Result<T> {
	std::result::Result::Err(Error::SimpleErr(err))
}

pub fn Err<T>(err: String) -> Result<T> {
	std::result::Result::Err(Error::Err(err))
}

pub fn SimpleErrAt<T>(location: CodeLocation, err: &'static str) -> Result<T> {
	std::result::Result::Err(Error::SimpleErrAt(location, err))
}

pub fn ErrAt<T>(location: CodeLocation, err: String) -> Result<T> {
	std::result::Result::Err(Error::ErrAt(location, err))
}

impl Debug for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self)
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Error::*;
		match self {
			SimpleErr(err) => write!(f, "Error: {}", err),
			Err(err) => write!(f, "Error: {}", err),
			SimpleErrAt(location, err) => write!(f, "{}: Error: {}", location, err),
			ErrAt(location, err) => write!(f, "{}: Error: {}", location, err),
		}
	}
}
