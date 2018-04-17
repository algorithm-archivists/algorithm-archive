#include <stdio.h>
#include <stddef.h>

void bubble_sort(int *array, size_t n) {
    int swapped = 0;
    for (size_t i = 0; i < n - 1; ++i) {
        swapped = 0;
        for (size_t j = 0; j < n - i - 1; ++j) {
            if (array[j] > array[j + 1]) {
                int tmp = array[j];
                array[j] = array[j + 1];
                array[j + 1] = tmp;

                swapped = 1;
            }
        }

        if (!swapped) {
            break;
        }
    }
}

int main() {
    int array[10] = {1, 45, 756, 4569, 56, 3, 8, 5, -10, -4};

    printf("Unsorted array:\n");
    for (size_t i = 0; i < 10; ++i) {
        printf("%d ", array[i]);
    }
    printf("\n\n");

    bubble_sort(array, 10);

    printf("Sorted array:\n");
    for (size_t i = 0; i < 10; ++i) {
        printf("%d ", array[i]);
    }
    printf("\n");

    return 0;
}
