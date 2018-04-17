#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>

bool is_sorted(int *array, size_t n) {
    while (--n >= 1) {
        if (array[n] < array[n - 1]) {
            return false;
        }
    }

    return true;
}

void shuffle(int *array, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        int t = array[i];
        int r = rand() % n;
        array[i] = array[r];
        array[r] = t;
    }
}

void bogo_sort(int *array, size_t n) {
    while (!is_sorted(array, n)) {
        shuffle(array, n);
    }
}

int main() {
    int array[10] = {1, 3654, 78, 654, -234, -12, 4, 3, -6, -100};

    printf("Unsorted array:\n");
    for (int i = 0; i < 10; ++i) {
        printf("%d ", array[i]);
    }
    printf("\n\n");

    bogo_sort(array, 10);

    printf("Sorted array:\n");
    for (int i = 0; i < 10; ++i) {
        printf("%d ", array[i]);
    }
    printf("\n");
}
