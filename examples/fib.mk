let fib = fn (n) {
	if (n < 2) { return n; };
	return fib(n - 2) + fib(n - 1);
}

print fib(7);
