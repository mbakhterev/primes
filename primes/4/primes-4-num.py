import sys
import numpy as np

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
    M = np.ones(N, np.dtype('int8'))
    M[:2] = 0
    return M

def sieve(M, N, step, c):
    M[c:N:step] = 0
    r = (N - c) % step
    return (step - r) if r != 0 else 0

def next_prime_offset(M, N, start):
    i = start + 1 + (start & 1)
    while i < N and M[i] == 0: i += 2
    return i

def optimus_primes(N):
    P = []
    C = []
    M = init_marks(N) if N >= 2 else np.zero(N)
    p = 2
    while p < N:
        s = p << (p & 1)
        P.append(s)
        C.append(sieve(M, N, s, p * p))
        p = next_prime_offset(M, N, p)
    return M, P, C

def sieve_recursor_count(N, M, P, C):
    M[:] = 1
    for i in range(0, len(P)):
        p = P[i]
        c = C[i]
        C[i] = sieve(M, N, p, c)
    return M[:N].sum()

def go(N):
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

go(int(sys.argv[1]))
