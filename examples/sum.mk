let sum = fn (n) {
	if (n == 1) {
		n;
	} else {
		n + sum(n - 1);	
	}
}

puts(sum(3));
