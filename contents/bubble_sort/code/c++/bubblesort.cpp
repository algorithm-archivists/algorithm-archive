#include <algorithm>
#include <cstddef>
#include <iostream>
#include <iterator>

template <class Iter>
void print_range(Iter first, Iter last) {
  for (auto it = first; it != last; ++it)
    std::cout << *it << " ";
  std::cout << std::endl;
}

template <class Iter>
void bubble_sort(Iter first, Iter last) {
  for (auto it1 = first; it1 != last; ++it1) {
    for (auto it2 = first; it2 + 1 != last; ++it2) {
      // these are unsorted! gotta swap 'em
      if (*(it2 + 1) < *it2) {
        std::iter_swap(it2, it2 + 1);
      }
    }
  }
}

int main() {
  int input[] = {1, 45, 756, 4569, 56, 3, 8, 5, -10, -4};

  std::cout << "Unsorted array:\n";
  print_range(std::begin(input), std::end(input));

  bubble_sort(std::begin(input), std::end(input));

  std::cout << "\nSorted array:\n";
  print_range(std::begin(input), std::end(input));
}
