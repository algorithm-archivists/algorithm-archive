#include <stdlib.h> // rand
#include <vector>   // vectors
#include <iostream> // cout
#include <algorithm>// swap

std::vector<int> bubble_sort(std::vector<int> unsorted_list) {
  while (true) {
    bool is_sorted = true;
    // Sweep through the array
    for (unsigned int i = 0; i < unsorted_list.size() - 1; i++) {
      // If next if smaller, swap, and keep sorting
      if (unsorted_list[i + 1] < unsorted_list[i]) {
        std::swap(unsorted_list[i], unsorted_list[i + 1]);
        is_sorted = false;
      }
    }
    // If we made it through without swapping anything, the list is sorted
    if (is_sorted) { return unsorted_list; }
  }
}

int main()
{
  std::vector<int> to_be_sorted = {};
  // Initialize and print a vector with 50 random integers of domain [0,1000]
  for (int i = 0; i < 50; i++) {
    int value = rand() % 1000;
    std::cout << value << " ";
    to_be_sorted.push_back(value);
  }
  std::cout << std::endl;

  std::vector<int> sorted = bubble_sort(to_be_sorted);

  for (int i : sorted) { std::cout << i << " "; }

  return 0;
}

