import math


def is_prime(n):
    for i in range(2, min(n, math.ceil(math.sqrt(n)) + 1)):
        if n % i == 0:
            return False
    return True


b = 106700
step = 17
c = b + (step * 1000)
h = sum(1 for i in range(b, c + 1, step) if not is_prime(i))
print(h)
