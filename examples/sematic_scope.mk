let a = "global";

let inner = fn () {
	let show = fn () {
		print a;	
	}

	show();

	let a = "inner";

	show();
}

inner();
