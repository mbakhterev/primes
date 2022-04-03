#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define PAGE_CAPACITY 510

typedef struct page {
  struct page *restrict next;
  long cursor;
  long numbers[PAGE_CAPACITY];
} Page;

Page *make_page(const long n) {
  Page *const P = malloc(sizeof(Page));
  P->next = NULL;
  P->cursor = 1;
  P->numbers[0] = n;
  return P;
}

#define likely(X) __builtin_expect((X), 1)

Page *push(Page *restrict const P, const long n) {
  if (likely((P->cursor < PAGE_CAPACITY))) {
    P->numbers[P->cursor++] = n;
    return P;
  }
  return P->next = make_page(n);
}

int is_prime(const Page *restrict const pages, const long n) {
  const long l = n / 2;
  for (const Page *restrict P = pages; P; P = P->next) {
    for (int i = 0; i < P->cursor; i++) {
      const long p = P->numbers[i];
      if (p > l) return 1;
      if (n % p == 0) return 0;
    }
  }
  return 1;
}

long counting_free(const Page *restrict const pages) {
  const Page *restrict P = pages;
  long cnt = 0;
  while (P) {
    const Page *const next = P->next;
    cnt += P->cursor;
    free((void *)P);
    P = next;
  }
  return cnt;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Specify limit\n");
    return 1;
  }

  const long N = atol(argv[1]);

  Page *restrict const primes = make_page(2);
  Page *restrict P = primes;
  for (long n = 3; n <= N; n += 2) {
    if (is_prime(primes, n)) P = push(P, n);
  }
  printf("%ld\n", counting_free(primes));

  return 0;
}
