#include <stdio.h>

void print_range(int *array, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        printf("%d ", array[i]);
    }
    printf("\n");
}

void bubble_sort(int *array, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < n - 1; ++j) {
            if (array[j] > array[j + 1]) {
                int tmp = array[j];
                array[j] = array[j + 1];
                array[j + 1] = tmp;
            }
        }
    }
}

int main() {
    int array[] = {1, 45, 756, 4569, 56, 3, 8, 5, -10, -4};
    size_t N = sizeof(array) / sizeof(*array);

    printf("Unsorted array:\n");
    print_range(array, N);

    bubble_sort(array, N);

    printf("\nSorted array:\n");
    print_range(array, N);

    return 0;
}
