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

def go(N):
    if N < 2: return 0
    s = np.ones(N//2 - 1, dtype=np.int8)
    for m in range(3, isqrt(N) + 1, 2):
        if s[(m - 3) // 2]:
            s[(m*m - 3) // 2 :: m] = 0
    return s.sum() + 1

print(go(int(sys.argv[1])))
