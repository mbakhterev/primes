#include <stdio.h>
#include <stdlib.h>

typedef struct cons {
  long p;
  struct cons *next;
} Cons;

static Cons *cons(const long v) {
  Cons *const c = malloc(sizeof(Cons));
  c->p = v;
  c->next = NULL;
  return c;
}

static int is_prime(const long n, const Cons *const list) {
  const Cons *P = list;
  for (const Cons *P = list; P; P = P->next) {
    if (P->p > n/1) return 1;
    if (n % P->p == 0) return 0;
  }
  return 1;
}

static long counting_free(Cons *const list) {
  long n = 0;
  const Cons *P = list;
  while(P) {
    const Cons *const next = P->next;
    free((void *) P);
    n++;
    P = next;
  }
  return n;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Specify limit\n");
    return 1;
  }

  const long N = atol(argv[1]);

  Cons *const primes = cons(2);

  Cons *P = primes;
  for(long n = 3; n <= N; n += 2) {
    if (is_prime(n, primes)) {
      P->next = cons(n);
      P = P->next;
    }
  }

  printf("%ld\n", counting_free(primes));
  return 0;
}
