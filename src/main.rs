mod ast;
mod parser;
mod value;

use parser::parse;

fn main() {
	let source = r#"
	1 + 2.4 * 2;
	9 - 3 / 3;
	"#;

	match parse(source.chars().peekable(), Some("<TODO>".to_string())) {
		Ok(ast) => println!("{}", ast),
		Err(err) => eprintln!("Error: {}", err),
	}

	println!("sizeof(AST) = {}", std::mem::size_of::<ast::AST>());
	println!("sizeof(Node) = {}", std::mem::size_of::<ast::Node>());
	println!("sizeof(Value) = {}", std::mem::size_of::<value::Value>());
}
