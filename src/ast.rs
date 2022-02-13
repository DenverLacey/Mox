use crate::parser::CodeLocation;
use crate::value::ObjID;

#[derive(Debug)]
pub struct AST {
	nodes: Vec<Node>,
	locations: Vec<CodeLocation>,
}

impl AST {
	pub fn new() -> Self {
		Self {
			nodes: Vec::new(),
			locations: Vec::new(),
		}
	}

	pub fn add_node(&mut self, node: Node, location: CodeLocation) -> usize {
		self.nodes.push(node);
		self.locations.push(location);
		self.nodes.len() - 1
	}

	pub fn get(&self, index: usize) -> &Node {
		&self.nodes[index]
	}

	pub fn get_mut(&mut self, index: usize) -> &mut Node {
		&mut self.nodes[index]
	}
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
