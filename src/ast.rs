use crate::parser::CodeLocation;
use crate::value::ObjID;

// @NOTE:
// How zig does AST stuff with indexes instead of pointers / convential tree structure
// https://mitchellh.com/zig/parser
//

#[derive(Debug)]
pub struct AST {
	roots: Vec<usize>,
	nodes: Vec<Node>,
	locations: Vec<CodeLocation>,
}

impl AST {
	pub fn new() -> Self {
		Self {
			roots: Vec::new(),
			nodes: Vec::new(),
			locations: Vec::new(),
		}
	}

	pub fn add_node(&mut self, node: Node, location: CodeLocation) -> usize {
		self.nodes.push(node);
		self.locations.push(location);
		self.nodes.len() - 1
	}

	pub fn add_root(&mut self, root_index: usize) {
		self.roots.push(root_index);
	}

	pub fn get(&self, index: usize) -> &Node {
		&self.nodes[index]
	}

	pub fn get_mut(&mut self, index: usize) -> &mut Node {
		&mut self.nodes[index]
	}
}

impl std::fmt::Display for AST {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		const INDENT_SIZE: usize = 2;

		fn fmt_at_indent(
			me: &AST,
			node_index: usize,
			indent: usize,
			f: &mut std::fmt::Formatter<'_>,
		) -> std::fmt::Result {
			let node = me.nodes[node_index];

			use NodeKind::*;
			match node.kind {
				// Literals
				Bool => write!(f, "Bool({})\n", node.lhs != 0),
				Int => write!(f, "Int({})\n", node.lhs as i64),
				Num => write!(f, "Num({})\n", unsafe {
					std::mem::transmute::<usize, f64>(node.lhs)
				}),
				Str => todo!(),
				List => todo!(),

				// Unary
				Negate => fmt_unary_at_indent("-", me, node_index, indent, f),

				// Binary
				Add => fmt_binary_at_indent("+", me, node_index, indent, f),
				Subtract => fmt_binary_at_indent("-", me, node_index, indent, f),
				Multiply => fmt_binary_at_indent("*", me, node_index, indent, f),
				Divide => fmt_binary_at_indent("/", me, node_index, indent, f),
				Assign => fmt_binary_at_indent("=", me, node_index, indent, f),
			}
		}

		fn fmt_unary_at_indent(
			op: &str,
			me: &AST,
			node_index: usize,
			indent: usize,
			f: &mut std::fmt::Formatter<'_>,
		) -> std::fmt::Result {
			write!(f, "`{}` {{\n", op)?;
			write!(
				f,
				"{: >indent$}{}",
				"",
				"sub: ",
				indent = (indent + 1) * INDENT_SIZE
			)?;
			write!(f, "{: >indent$}}}\n", "", indent = indent * INDENT_SIZE)
		}

		fn fmt_binary_at_indent(
			op: &str,
			me: &AST,
			node_index: usize,
			indent: usize,
			f: &mut std::fmt::Formatter<'_>,
		) -> std::fmt::Result {
			let node = me.nodes[node_index];
			write!(f, "`{}` {{\n", op)?;
			write!(
				f,
				"{: >indent$}{}",
				"",
				"lhs: ",
				indent = (indent + 1) * INDENT_SIZE
			)?;
			fmt_at_indent(me, node.lhs, indent + 1, f)?;
			write!(
				f,
				"{: >indent$}{}",
				"",
				"rhs: ",
				indent = (indent + 1) * INDENT_SIZE
			)?;
			fmt_at_indent(me, node.rhs, indent + 1, f)?;
			write!(f, "{: >indent$}}}\n", "", indent = indent * INDENT_SIZE)
		}

		write!(f, "AST {{\n")?;
		for (i, &root) in self.roots.iter().enumerate() {
			write!(f, "{: >indent$}{}. ", "", i, indent = INDENT_SIZE)?;
			fmt_at_indent(self, root, 1, f)?;
		}
		write!(f, "}}")
	}
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NodeKind {
	// Literals
	Bool,
	Int,
	Num,
	Str,
	List,

	// Unary
	Negate,

	// Binary
	Add,
	Subtract,
	Multiply,
	Divide,
	Assign,
}
