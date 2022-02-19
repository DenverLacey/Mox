mod ast;
mod error;
mod parser;
mod value;

use parser::parse;

fn main() {
	println!("sizeof(AST) = {}", std::mem::size_of::<ast::AST>());
	println!("sizeof(Node) = {}", std::mem::size_of::<ast::Node>());
	println!("sizeof(Value) = {}", std::mem::size_of::<value::Value>());

	if let Err(err) = execute() {
		eprintln!("{}", err);
	}
}

fn execute() -> error::Result<()> {
	let source = r#"
	# this is a comment
	
	a1 + -_1b
	3 * (4 + 1)

	# this is another comment

	"#;

	let ast = parse(source.chars().peekable(), "<SOURCE>".to_string())?;
	println!("{}", ast);

	Ok(())
}
