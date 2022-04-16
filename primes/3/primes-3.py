import sys

def isqrt ( x ):
    q = 1
    while q <= x : 
        q *= 4
    z,r = x,0
    while q > 1 :
        q  /= 4
        t,r = z-r-q,r/2
        if t >= 0 :
            z,r = t,r+q
    return r

def is_prime(n, P):
    l = isqrt(n)
    for p in P:
        if p > l: return True
        if not (n % p): return False
    return True

limit = int(sys.argv[1])
primes = [2]

for n in range(3, limit + 1, 2):
    if is_prime(n, primes): primes.append(n)

print(len(primes))
