import sys

def isqrt(x):
    q = 1
    while q <= x: 
        q <<= 2
    z, r = x, 0
    while q > 1:
        q  >>= 2
        t, r = z-r-q, r >> 1
        if t >= 0:
            z, r = t, r+q
    return r

def init_marks(N):
    assert(N > 1)
    M = bytearray([1] * N)
    M[0] = 0
    M[1] = 1
    return M

def sieve(M, N, p, c):
    while c < N:
        M[c] = 0
        c += p
    return c - N

def next_prime_offset(M, N, start):
    i = start + 1 + (start & 1)
    while i < N and M[i] == 0: i += 2
    return i

def optimus_primes(N):
    P = []
    C = []
    M = init_marks(N) if N >= 2 else bytearray([0] * L)
    p = 2
    while p < L:
        P.append(p)
        C.append(sieve(M, N, p, p + p))
        p = next_prime_offset(M, N, p)
    return M, P, C

def reset(M):
    for i in range(0, len(M)): M[i] = 1

def sieve_recursor_count(N, M, P, C):
    reset(M)
    for i in range(0, len(P)):
        p = P[i]
        c = C[i]
        C[i] = sieve(M, N, p, c)
    return sum(M[:N])

N = int(sys.argv[1])
L = isqrt(N) + 1
M, P, C = optimus_primes(L)
if len(P) == 0:
    print(0)
else:
    n = len(P)
    l = (N + 1) - L
    while l > 0:
        n += sieve_recursor_count(min(l, L), M, P, C)
        l -= L
    print(n)
