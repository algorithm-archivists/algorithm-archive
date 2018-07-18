#include <algorithm>
#include <cstddef>
#include <iostream>
#include <iterator>
#include <random>

using std::begin;
using std::end;

template <class Rng>
std::vector<float> generate_input(std::size_t size, Rng& rng) {
  auto dist = std::uniform_real_distribution<>(0.0, 1.0);

  auto ret = std::vector<float>();
  std::generate_n(std::back_inserter(ret), size,
    [&rng, &dist] { return dist(rng); });

  return ret;
}

template <class Iter>
void print_range(std::ostream& os, Iter const first, Iter const last) {
  os << '{';

  if (first != last) {
    os << *first;
    std::for_each(first + 1, last, [&os] (double d) { os << ", " << d; });
  }

  os << "}\n";
}

template <class Iter>
void bubble_sort(Iter const first, Iter const last) {
  if (first == last) {
    // the empty range is vacuously sorted
    return;
  }

  for (;;) {
    bool is_sorted = true;

    for (auto it = first; it + 1 != last; ++it) {
      // these are unsorted! gotta swap 'em
      if (*(it + 1) < *it) {
        using std::swap;
        swap(*it, *(it + 1));
        is_sorted = false;
      }
    }

    if (is_sorted) {
      break;
    }
  }
}

int main() {
  std::random_device random_device;
  auto rng = std::mt19937(random_device());

  auto input = generate_input(10, rng);

  std::cout << "before sorting:\n";
  print_range(std::cout, begin(input), end(input));

  bubble_sort(begin(input), end(input));

  std::cout << "\nafter sorting:\n";
  print_range(std::cout, begin(input), end(input));
}
