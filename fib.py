def fibonacci(number=1):
	previous = 0
	current = 1
	history = []

	for i in range (0, number):
		aux = current + previous
		previous = current
		current = aux
		history.append(current)

	for i in range(number):
		print history[i]
fibonacci(10)
fibonacci()







def fibonacci_history(number):
	previous = 0
	current = 1
	history = []
	for i in range (0, number):
		aux = current + previous
		previous = current
		current = aux
		history.append(current)
	return history

def print_history(history, number):
	for i in range(number):
		print history[i]

def fibonacci2(number=1):
	history = fibonacci_history(number)
	print_history(history, number)

fibonacci2(10)
fibonacci2()