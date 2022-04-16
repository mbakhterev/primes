#include <stdio.h>

int is_prime(int n) {
  for(int p = 2; p <= n/2; p++)
    if (!(n % p))
      return 0;
  return 1;
}

int main(void) {
  int n_primes = 0;

  for(int n = 2; n < 250001; n++)
    n_primes += is_prime(n);

  printf("%d\n", n_primes);
  return 0;
}
