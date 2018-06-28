#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

struct person {
    int id;
    struct person *partner;
    size_t *prefers;
    size_t index;
};

void shuffle(size_t *array, size_t size) {
    for (size_t i = size - 1; i > 0; --i) {
        size_t j = rand() % (i + 1);
        size_t tmp = array[i];
        array[i] = array[j];
        array[j] = tmp;
    }
}

void create_group(struct person *group, size_t size, bool are_men) {
    for (size_t i = 0; i < size; ++i) {
        group[i].id = i;
        group[i].partner = NULL;
        group[i].prefers = (size_t*)malloc(sizeof(size_t) * size);
        group[i].index = 0;

        for (size_t j = 0; j < size; ++j) {
            group[i].prefers[j] = j;
        }

        shuffle(group[i].prefers, size);
    }
}

bool prefers_partner(size_t *prefers, size_t partner, size_t id, size_t size) {
    for (size_t i = 0; i < size; ++i) {
        if (prefers[i] == partner) {
            return true;
        } else if(prefers[i] == id) {
            return false;
        }
    }
}

void stable_marriage(struct person *men, struct person *women, size_t size) {
    struct person *bachelors[size];
    size_t bachelors_size = size;

    for (size_t i = 0; i < size; ++i) {
        bachelors[i] = &men[i];
    }

    while (bachelors_size > 0) {
        struct person *man = bachelors[bachelors_size - 1];
        struct person *woman = &women[man->prefers[man->index]];

        if (!woman->partner) {
            woman->partner = man;
            man->partner = woman;
            bachelors[--bachelors_size] = NULL;
        } else if (!prefers_partner(woman->prefers, woman->partner->id, man->id,
                                   size)) {

            woman->partner->index++;
            bachelors[bachelors_size - 1] = woman->partner;
            woman->partner = man;
            man->partner = woman;
        } else {
            man->index++;
        }
    }
}

void free_group(struct person *group, size_t size) {
    for (size_t i = 0; i < size; ++i) {
        free(group[i].prefers);
    }
}

int main() {
    srand(time(NULL));

    struct person men[5], women[5];

    create_group(men, 5, true);
    create_group(women, 5, false);

    for (size_t i = 0; i < 5; ++i) {
        printf("preferences of man %zu: ", i);
        for (size_t j = 0; j < 5; ++j) {
            printf("%zu ", men[i].prefers[j]);
        }

        printf("\n");
    }

    printf("\n");

    for (size_t i = 0; i < 5; ++i) {
        printf("preferences of woman %zu: ", i);
        for (size_t j = 0; j < 5; ++j) {
            printf("%zu ", women[i].prefers[j]);
        }

        printf("\n");
    }

    stable_marriage(men, women, 5);

    printf("\n");

    for (size_t i = 0; i < 5; ++i) {
        printf("the partner of man %zu is woman %d\n", i, men[i].partner->id);
    }

    free_group(men, 5);
    free_group(women, 5);
}
