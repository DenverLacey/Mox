use crate::parser::CodeLocation;
use crate::value::ObjID;

#[derive(Debug)]
pub struct AST {
	nodes: Vec<Node>,
	locations: Vec<CodeLocation>,
}

#[derive(Debug)]
pub struct Node {
	pub kind: NodeKind,
	pub lhs: usize,
	pub rhs: usize,
}

impl Node {
	pub fn new(kind: NodeKind, lhs: usize, rhs: usize) -> Self {
		Self { kind, lhs, rhs }
	}

	pub fn new_bool(value: bool) -> Self {
		Self::new(NodeKind::Bool, value as usize, 0)
	}

	pub fn new_int(value: i64) -> Self {
		Self::new(NodeKind::Int, value as usize, 0)
	}

	pub fn new_num(value: f64) -> Self {
		Self::new(NodeKind::Num, unsafe { std::mem::transmute(value) }, 0)
	}

	pub fn new_str(value: ObjID) -> Self {
		Self::new(NodeKind::Str, value, 0)
	}

	pub fn new_list(value: ObjID) -> Self {
		Self::new(NodeKind::List, value, 0)
	}
}

#[derive(Debug)]
pub enum NodeKind {
	// Literals
	Bool,
	Int,
	Num,
	Str,
	List,

	// Binary
	Add,
	Subtract,
	Multiply,
	Divide,
}
