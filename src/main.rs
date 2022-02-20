mod ast;
mod error;
mod parser;
mod value;

use parser::parse;

fn main() {
	let (source, file) = match get_source() {
		Ok((source, file)) => (source, file),
		Err(err) => {
			eprintln!("Error: {}", err);
			return;
		}
	};

	if let Err(err) = execute(source, file) {
		eprintln!("{}", err);
	}
}

fn get_source() -> Result<(String, String), &'static str> {
	let file = std::env::args()
		.skip(1)
		.next()
		.ok_or("No file name given.")?;

	let source = std::fs::read_to_string(file.clone()).or_else(|_| Err("Failed to read file."))?;

	Ok((source, file))
}

fn execute(source: String, file: String) -> error::Result<()> {
	let ast = parse(source.chars().peekable(), file)?;
	println!("{}", ast);

	Ok(())
}
