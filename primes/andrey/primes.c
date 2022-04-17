#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static long isqrt(long x) {
    long q = 1;
    while (q <= x)
        q <<= 2;
    long r = 0;
    while (q > 1) {
        q >>= 2;
        const long t = x - r - q;
        r >>= 1;
        if (t >= 0) {
            x = t;
            r += q;
        }
    }
    return r;
}

static long sum(const int8_t *const restrict A, const long N) {
  long s = 0;
  for (long i = 0; i < N; i++) s += A[i];
  return s;
}

static long idx(const long n) { return (n - 3) / 2; }

static long go(const long N) {
  if (N < 2) return 0;

  const long M = N / 2 - 1;
  int8_t *const restrict S = malloc(M);
  memset(S, 1, M);

  for (long m = 3; m <= isqrt(N); m += 2)
    if (S[idx(m)])
      for (long i = idx(m * m); i < M; i += m) S[i] = 0;

  const long s = sum(S, M) + 1;
  free(S);

  return s;
}

int main(int argc, char *argv[]) {                                                                                     
  assert(argc >= 2);
  printf("%ld\n", go(atol(argv[1])));
  return 0;
}
