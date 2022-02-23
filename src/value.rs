use enum_tags::*;

pub type ObjID = usize;

#[derive(Debug, Tag)]
pub enum Value {
	Bool(bool),
	Int(i64),
	Num(f64),
	Str(ObjID),
	List(ObjID),
}
