mod ast;
mod error;
mod parser;
mod value;

use parser::parse;

fn main() {
	if let Err(err) = execute() {
		eprintln!("{}", err);
	}
}

fn execute() -> error::Result<()> {
	let source = r#"
		# this is a comment
		a1 + -_1b
		3 * (4 + 1)

		if my_bool {
			9999
		} else if 1 {
			99.9
		} else {
			666
		}

		while my_other_bool {
			69240
		}

		# this is another comment

		"#;

	let ast = parse(source.chars().peekable(), "<SOURCE>".to_string())?;
	println!("{}", ast);

	Ok(())
}
