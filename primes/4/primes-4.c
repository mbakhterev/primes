#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

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

static long min(const long a, const long b) {
  const long d = a - b;
  const long s = d >> 63;
  return b + (d & s);
}

typedef struct {
  int8_t *restrict buffer;
  long length;
} Marks;

static Marks init_marks(const long N) {
  assert(N > 1);
  int8_t *restrict B = malloc(N);
  assert(B);
  memset(B, 1, N);
  B[0] = 0;
  B[1] = 0;
  return (Marks) { .buffer = B, .length = N };
}

static void reset(const Marks *const M) {
  memset(M->buffer, 1, M->length);
}

typedef struct {
  long *restrict buffer;
  long capacity;
  long cursor;
} Array;

static Array empty_array(void) {
  const long ps = sysconf(_SC_PAGESIZE);
  return (Array) {
    .buffer = malloc(ps),
    .cursor = 0,
    .capacity = ps / sizeof(long) };
}

static void push(const long v, Array *const restrict A) {
  if (A->cursor >= A->capacity) {
    A->capacity += sysconf(_SC_PAGESIZE) / sizeof(long);
    A->buffer = realloc(A->buffer, A->capacity * sizeof(long));
  }
  A->buffer[A->cursor++] = v;
}

static long sieve(const Marks *const M, const long N, const long step, long c) {
  while (c < N) {
    M->buffer[c] = 0;
    c += step;
  }
  return c - N;
}

static long next_prime_offset(const Marks *const M, const long N, const long start) {
  long i = start + 1 + (start & 1);
  while (i < N && M->buffer[i] == 0) i += 2;
  return i;
}

static long sum(const Marks *const M, const long N) {
  long s = 0;
  for(long i = 0; i < N; i += 1) s += M->buffer[i];
  return s;
}

static Marks optimus_primes(const long N, Array *const P, Array *const C) {
  *P = empty_array();
  *C = empty_array();

  Marks M  = N >= 2 ? init_marks(N) : (Marks){ .buffer = NULL, .length = 0 };

  long p = 2;
  while (p < N) {
    const long s = p << (p & 1);
    push(s, P);
    push(sieve(&M, N, s, p * p), C);
    p = next_prime_offset(&M, N, p);
  }

  return M;
}

static long sieve_recursor_count(const long N, const Marks *const M, const Array *const P, const Array *const C) {
  reset(M);
  for(long i = 0; i < P->cursor; i += 1) {
    const long p = P->buffer[i];
    const long c = C->buffer[i];
    C->buffer[i] = sieve(M, N, p, c);
  }
  return sum(M, N);
}

int main(int argc, char *argv[]) {                                                                                     
  if (argc < 2) {                                                                                                      
    fprintf(stderr, "Specify limit\n");                                                                                
    return 1;                                                                                                          
  }                                                                                                                    
  const long N = atol(argv[1]);                                                                                        
  const long L = isqrt(N) + 1;

  Array P, C;
  Marks M = optimus_primes(L, &P, &C);

  long n = P.cursor;
  if (n == 0) {
    printf("0\n");
    return 0;
  }

  long l = (N + 1) - L;
  while (l > 0) {
    n += sieve_recursor_count(min(l, L), &M, &P, &C);
    l -= L;
  }
  free(M.buffer);
  free(C.buffer);
  free(P.buffer);

  printf("%ld\n", n);
  return 0;
}
