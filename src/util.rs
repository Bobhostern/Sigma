#[derive(Debug, Clone)]
pub enum Constant {
	Integer(i32),
	Float(f32),
	StrConst(String)
}

#[derive(Debug, Clone)]
pub struct ConstantName(usize);

#[derive(Debug, Clone)]
pub struct ConstantTable {
	table: Vec<Constant>,
}

impl ConstantTable {
	pub fn new(sc: usize) -> ConstantTable {
		ConstantTable {
			table: Vec::with_capacity(sc)
		}
	}

	pub fn add(&mut self, s: Constant) -> ConstantName {
		match self.table.iter().position(|ref x| match (x, s.clone()) {
			(&&Constant::Integer(ref i), Constant::Integer(ref b)) =>  i == b,
			(&&Constant::Float(ref i), Constant::Float(ref b)) =>  i == b,
			(&&Constant::StrConst(ref i), Constant::StrConst(ref b)) =>  i == b,
			_ => false
		}) {
			Some(p) => ConstantName(p),
			None => {
				self.table.push(s);
				ConstantName(self.table.len() - 1)
			}
		}
	}
}

impl AsRef<[Constant]> for ConstantTable {
	fn as_ref(&self) -> &[Constant] {
		&self.table
	}
}

impl AsRef<Vec<Constant>> for ConstantTable {
	fn as_ref(&self) -> &Vec<Constant> {
		&self.table
	}
}

impl From<Vec<Constant>> for ConstantTable {
	fn from(o: Vec<Constant>) -> ConstantTable {
		ConstantTable {
			table: o
		}
	}
}

impl Into<Vec<Constant>> for ConstantTable {
	fn into(self) -> Vec<Constant> {
		self.table
	}
}
