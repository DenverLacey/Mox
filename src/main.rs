mod ast;
mod parser;
mod value;

use parser::parse;

fn main() {
	println!("sizeof(AST) = {}", std::mem::size_of::<ast::AST>());
	println!("sizeof(Node) = {}", std::mem::size_of::<ast::Node>());
	println!("sizeof(Value) = {}", std::mem::size_of::<value::Value>());

	if let Err(err) = execute() {
		eprintln!("Error: {}", err);
	}
}

fn execute() -> Result<(), String> {
	let source = r#"
	# this is a comment
	
	15 - -3 / 44

	# this is another comment

	"#;

	let ast = parse(source.chars().peekable(), Some("<TODO>".to_string()))?;
	println!("{}", ast);

	Ok(())
}
