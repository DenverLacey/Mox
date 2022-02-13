mod ast;
mod parser;
mod value;

use parser::*;

fn main() {
	let source = r#"
	1 + 2.4
	"#;
	let mut t = Tokenizer::new(source.chars().peekable());

	println!("Peek:");
	println!("\t{:?}", t.peek());
	println!("\t{:?}", t.peek());

	println!("PeekN:");
	println!("\t{:?}", t.peek_n(0));
	println!("\t{:?}", t.peek_n(1));
	println!("\t{:?}", t.peek_n(2));
	println!("\t{:?}", t.peek_n(3));

	println!("Next:");
	while let Some(token) = t.next().unwrap() {
		println!("\t{:?}", token);
	}

	println!("sizeof(AST) = {}", std::mem::size_of::<ast::AST>());
	println!("sizeof(Node) = {}", std::mem::size_of::<ast::Node>());
	println!("sizeof(Value) = {}", std::mem::size_of::<value::Value>());
}
