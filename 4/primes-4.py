import sys

def isqrt(x):
    q = 1
    while q <= x: 
        q *= 4
    z, r = x, 0
    while q > 1:
        q  /= 4
        t, r = z-r-q, r/2
        if t >= 0:
            z, r = t, r+q
    return r

def init_markvector(limit):
    assert(limit > 1)
    M = bytearray([1] * (limit + 1))
    M[0] = 0
    M[1] = 1
    return M

def vector_limit(A, L): return min(L, len(A) - 1)

def sieve(M, limit, p, cursor):
    l = vector_limit(M, limit)
    c = cursor
    while c <= l:
        M[c] = 0
        c += p
    return c - l - 1

def next_prime_offset(M, limit, start):
    l = vector_limit(M, limit)
    i = start + 1 + (start & 1)
    while i <= l and M[i] == 0: i += 2
    if i <= l: return i
    return limit + 1

def optimus_primes():
    N = int(sys.argv[1])
    L = isqrt(N)
    P = []
    C = []
    if L < 2:
        M = bytearray([0] * L)
    else:
        M = init_markvector(L)
    p = 2
    while p <= L:
        P.append(p)
        C.append(sieve(M, L, p, p + p))
        p = next_prime_offset(M, L, p)
    return N, M, P, C

def reset(M):
    for i in range(0, len(M)):
        M[i] = 1

def limited_sum(A, L):
    s = 0
    for i in range(0, vector_limit(A, L) + 1):
        s += A[i]
    return s

def sieve_recursor_count(N, M, P, C):
    reset(M)
    for i in range(0, len(P)):
        p = P[i]
        c = C[i]
        C[i] = sieve(M, N, p, c)
    return limited_sum(M, N)

N, M, P, C = optimus_primes()
if len(P) == 0:
    print(0)

L = len(M)
n = len(P)
left = N - L
while left > 0:
    n += sieve_recursor_count(left, M, P, C)
    left -= L
print(n)
