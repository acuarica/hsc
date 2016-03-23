
enum Expr<'a> {
	Const(i32),
	Plus(&'a Expr<'a>, &'a Expr<'a>),
}

fn eval(expr: &Expr) -> i32 {
	match expr {
		&Expr::Const(x) => x,
		&Expr::Plus(lexp, rexp) => eval(lexp) + eval(rexp),
	}
}

fn main() {
	let x = 4;
	println!("Holaaa {}", x);

	let e0 = Expr::Const(2);
	let e1 = Expr::Const(5);
	let e2 = Expr::Plus(&e0, &e1);

	println!("eval: {}", eval(&e2));
}
