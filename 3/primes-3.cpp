#include <iostream>
#include <vector>

using std::vector;

template <typename integer>
integer isqrt(integer x) {
    integer q = 1;
    while (q <= x)
        q <<= 2;
    integer r = 0;
    while (q > 1) {
        q >>= 2;
        integer t = x - r - q;
        r >>= 1;
        if (t >= 0) {
            x = t;
            r += q;
        }
    }
    return r;
}

static bool is_prime(const long n, const vector<long> &P) {
  long l = isqrt<long>(n);
  for (auto p : P) {
    if (p > l) return true;
    if (!(n % p)) return false;
  }
  return true;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Specify limit" << std::endl;
    return 1;
  }

  const unsigned long limit = std::stol(argv[1]);

  vector<long> primes = {2};

  for(long n = 3; n <= limit; n += 2) {
    if (is_prime(n, primes)) {
      primes.push_back(n);
    }
  }

  std::cout << primes.size() << std::endl;

  return 0;
}
