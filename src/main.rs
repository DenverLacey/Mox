mod ast;
mod parser;
mod value;

use parser::*;

fn main() {
	let source = r#"
	1 + 2
	"#;
	let mut t = Tokenizer::new(source.chars().peekable());

	println!("Peek:");
	println!("{:?}", t.peek());
	println!("{:?}", t.peek());

	println!("PeekN:");
	println!("{:?}", t.peek_n(0));
	println!("{:?}", t.peek_n(1));
	println!("{:?}", t.peek_n(2));

	println!("Next:");
	while let Some(token) = t.next() {
		println!("{:?}", token);
	}

	println!("sizeof(AST) = {}", std::mem::size_of::<ast::AST>());
	println!("sizeof(Node) = {}", std::mem::size_of::<ast::Node>());
	println!("sizeof(Value) = {}", std::mem::size_of::<value::Value>());
}
