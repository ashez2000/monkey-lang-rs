let sum = fn (n) {
	if (n == 1) {
		n;
	} else {
		n + sum(n - 1);	
	}
}

print sum(3);
