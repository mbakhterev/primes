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

static Marks init_marks(const long limit) {
  assert(limit > 1);
  const long l = limit + 1;
  int8_t *restrict B = malloc(l);
  assert(B);
  memset(B, 1, l);
  return (Marks) { .buffer = B, .length = l };
}

static void reset(const Marks *const M) {
  memset(M->buffer, 1, M->length);
}

static long marks_limit(const Marks *const M, const long L) {
  return min(L, M->length - 1);
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

static long sieve(const Marks *const M, const long limit, const long p, long c) {
  const long l = marks_limit(M, limit);
  while (c <= l) {
    M->buffer[c] = 0;
    c += p;
  }
  return c - l - 1;
}

static long next_prime_offset(const Marks *const A, const long limit, const long start) {
  const long l = marks_limit(A, limit);
  long i = start + 1 + (start & 1);
  while (i <= l && A->buffer[i] == 0) i += 2;
  if (i <= l) return i;
  return limit + 1;
}

static Marks optimus_primes(const long N, Array *const P, Array *const C) {
  *P = empty_array();
  *C = empty_array();

  const long L = isqrt(N);
  if (L < 2) return (Marks) { .buffer = NULL, .length = 0 }; 

  Marks M = init_marks(L);
  long p = 2;
  while (p <= L) {
    push(p, P);
    push(sieve(&M, L, p, p + p), C);
    p = next_prime_offset(&M, L, p);
  }

  return M;
}

static long sum(const Marks *const M, const long L) {
  const long l = marks_limit(M, L);
  long s = 0;
  for(long i = 0; i <= l; i += 1)
    s += M->buffer[i];
  return s;
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

  Array P, C;
  Marks M = optimus_primes(N, &P, &C);
  long n = P.cursor;
  if (n == 0) {
    printf("0\n");
    return 0;
  }

  const long L = M.length;
  long left = N - L;
  while (left > 0) {
    n += sieve_recursor_count(left, &M, &P, &C);
    left -= L;
  }
  printf("%ld\n", n);
  return 0;
}
