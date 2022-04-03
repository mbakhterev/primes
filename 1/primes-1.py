import sys

def is_prime(n, P):
    for p in P:
        if p > n//2: return True
        if not (n%p): return False
    return True

limit = int(sys.argv[1])

primes = [2]
for n in range(3, limit + 1, 2):
    if is_prime(n, primes): primes.append(n)

print(len(primes))
