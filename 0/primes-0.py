def is_prime(n):
    for p in range(2, n//2 + 1):
        if (not (n%p)):
            return 0
    return 1

n_primes = 0

for i in range(2, 250001):
    n_primes += is_prime(i)

print(str(n_primes))
