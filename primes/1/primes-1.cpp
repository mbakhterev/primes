#include <iostream>
#include <vector>

using std::vector;

static bool is_prime(const long n, const vector<long> &P) {
  for (auto p : P) {
    if (p > n / 2) return true;
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
