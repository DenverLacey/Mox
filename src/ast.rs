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
	extra_data: Vec<usize>,
	strings: Vec<String>,
	locations: Vec<CodeLocation>,
}

impl AST {
	pub fn new() -> Self {
		Self {
			roots: Vec::new(),
			nodes: Vec::new(),
			extra_data: Vec::new(),
			strings: Vec::new(),
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

	pub fn add_extra_data<E>(&mut self, extra_data: E) -> usize
	where
		E: ExtraData,
	{
		let extra_data = extra_data.as_slice();
		let extra_data_index = self.extra_data.len();
		self.extra_data.extend_from_slice(extra_data);
		extra_data_index
	}

	pub fn add_string(&mut self, string: String) -> usize {
		if let Some(index) = self.strings.iter().position(|s| *s == string) {
			index
		} else {
			self.strings.push(string);
			self.strings.len() - 1
		}
	}

	pub fn get(&self, index: usize) -> &Node {
		&self.nodes[index]
	}

	pub fn get_mut(&mut self, index: usize) -> &mut Node {
		&mut self.nodes[index]
	}

	pub fn get_extra_data<E>(&self, index: usize) -> E
	where
		E: ExtraData,
	{
		let slice = self
			.extra_data
			.get(index..index + E::len())
			.expect("Failed to retrieve extra data!");

		E::from(slice)
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
				Ident => write!(f, "Ident({})\n", me.strings[node.lhs]),
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
			let node = me.nodes[node_index];
			write!(f, "`{}` {{\n", op)?;
			write!(
				f,
				"{: >indent$}{}",
				"",
				"sub: ",
				indent = (indent + 1) * INDENT_SIZE
			)?;
			fmt_at_indent(me, node.lhs, indent + 1, f)?;
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

	pub fn new_str(index: usize) -> Self {
		Self::new(NodeKind::Str, index, 0)
	}

	pub fn new_ident(index: usize) -> Self {
		Self::new(NodeKind::Ident, index, 0)
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
	Ident,
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

pub trait ExtraData {
	fn len() -> usize;
	fn from(slice: &[usize]) -> Self;

	fn as_slice(&self) -> &[usize] {
		unsafe { std::slice::from_raw_parts(self as *const Self as *const usize, Self::len()) }
	}
}

pub struct IfExtraData {
	pub condition: usize,
	pub then_block: usize,
	pub else_block: usize, // 0 means no else_block
}

impl ExtraData for IfExtraData {
	fn len() -> usize {
		3
	}

	fn from(slice: &[usize]) -> Self {
		Self {
			condition: slice[0],
			then_block: slice[1],
			else_block: slice[2],
		}
	}
}
