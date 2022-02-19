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
	blocks: Vec<NodeBlock>,
	locations: Vec<CodeLocation>,
}

impl AST {
	pub fn new() -> Self {
		Self {
			roots: Vec::new(),
			nodes: Vec::new(),
			extra_data: Vec::new(),
			strings: Vec::new(),
			blocks: Vec::new(),
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

	pub fn add_block(&mut self, block: NodeBlock) -> usize {
		self.blocks.push(block);
		self.blocks.len() - 1
	}

	pub fn get(&self, index: usize) -> &Node {
		&self.nodes[index]
	}

	pub fn get_mut(&mut self, index: usize) -> &mut Node {
		&mut self.nodes[index]
	}

	pub fn get_extra_data<E>(&self, index: usize) -> E
	where
		E: ExtraData + Default + Sized,
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

				// Blocks
				Block => fmt_block_at_indent("{}", me, node_index, indent, f),

				// Unique
				If => {
					let data: ExtraDataIf = me.get_extra_data(node.lhs);

					write!(f, "`if` {{\n")?;

					write!(
						f,
						"{: >indent$}condition: ",
						"",
						indent = (indent + 1) * INDENT_SIZE
					)?;
					fmt_at_indent(me, data.condition, indent + 1, f)?;

					write!(
						f,
						"{: >indent$}then: ",
						"",
						indent = (indent + 1) * INDENT_SIZE
					)?;
					fmt_at_indent(me, data.then_block, indent + 1, f)?;

					if data.else_block != 0 {
						write!(
							f,
							"{: >indent$}else: ",
							"",
							indent = (indent + 1) * INDENT_SIZE
						)?;
						fmt_at_indent(me, data.else_block, indent + 1, f)?;
					}

					write!(f, "{: >indent$}}}\n", "", indent = indent * INDENT_SIZE)
				}
				While => {
					write!(f, "`while` {{\n")?;

					write!(
						f,
						"{: >indent$}condition: ",
						"",
						indent = (indent + 1) * INDENT_SIZE
					)?;
					fmt_at_indent(me, node.lhs, indent + 1, f)?;

					write!(
						f,
						"{: >indent$}body: ",
						"",
						indent = (indent + 1) * INDENT_SIZE
					)?;
					fmt_at_indent(me, node.rhs, indent + 1, f)?;

					write!(f, "{: >indent$}}}\n", "", indent = indent * INDENT_SIZE)
				}
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

		fn fmt_block_at_indent(
			op: &str,
			me: &AST,
			node_index: usize,
			indent: usize,
			f: &mut std::fmt::Formatter<'_>,
		) -> std::fmt::Result {
			let node = me.nodes[node_index];
			let block = &me.blocks[node.lhs];
			write!(f, "`{}` {{\n", op)?;
			for (i, &root) in block.roots.iter().enumerate() {
				write!(
					f,
					"{: >indent$}{}. ",
					"",
					i,
					indent = (indent + 1) * INDENT_SIZE
				)?;
				fmt_at_indent(me, root, indent + 1, f)?;
			}
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

	// Blocks
	Block,

	// Unique
	If,
	While,
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

#[derive(Debug)]
pub struct NodeBlock {
	pub kind: NodeKind,
	pub roots: Vec<usize>,
}

impl NodeBlock {
	pub fn new(kind: NodeKind, roots: Vec<usize>) -> Self {
		Self { kind, roots }
	}
}

pub trait ExtraData
where
	Self: Sized + Default,
{
	fn len() -> usize {
		std::mem::size_of::<Self>() / std::mem::size_of::<usize>()
	}

	fn from(slice: &[usize]) -> Self {
		assert_eq!(slice.len(), Self::len());
		let mut s = Self::default();
		s.as_slice_mut().copy_from_slice(slice);
		s
	}

	fn as_slice(&self) -> &[usize] {
		unsafe { std::slice::from_raw_parts(self as *const Self as *const usize, Self::len()) }
	}

	fn as_slice_mut(&mut self) -> &mut [usize] {
		unsafe { std::slice::from_raw_parts_mut(self as *mut Self as *mut usize, Self::len()) }
	}
}

impl ExtraData for ExtraDataIf {}

#[repr(C)] // We need repr(C) to guarentee fields are in the expencted places
#[derive(Default)]
pub struct ExtraDataIf {
	pub condition: usize,
	pub then_block: usize,
	pub else_block: usize, // 0 means no else_block
}
