mod parser;
mod value;
mod ast;

use parser::*;

fn main() {
let source = r#"
1 + 2
"#;
	let t = Tokenizer::new(source.chars().peekable());
	println!("{:#?}", t);

	println!("sizeof(AST) = {}", std::mem::size_of::<ast::AST>());
	println!("sizeof(Node) = {}", std::mem::size_of::<ast::Node>());
	println!("sizeof(Value) = {}", std::mem::size_of::<value::Value>());
}
