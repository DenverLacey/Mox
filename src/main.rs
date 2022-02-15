mod ast;
mod parser;
mod value;

use parser::parse;

fn main() -> Result<(), String> {
	let source = r#"
	12 + 13;
"#;

	let ast = parse(source.chars().peekable(), Some("<TODO>".to_string()))?;
	println!("{}", ast);

	println!("sizeof(AST) = {}", std::mem::size_of::<ast::AST>());
	println!("sizeof(Node) = {}", std::mem::size_of::<ast::Node>());
	println!("sizeof(Value) = {}", std::mem::size_of::<value::Value>());

	Ok(())
}
