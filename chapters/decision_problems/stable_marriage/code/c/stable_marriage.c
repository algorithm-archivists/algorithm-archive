#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct person {
    size_t id;
    size_t prtnr;
    size_t *prefs;
    size_t pref_ind;
} person;

void shuffle(size_t *x, size_t n) {
    if (n > 1) {
        for (size_t i = 0; i < n - 1; ++i) {
            size_t j = i + rand() / (RAND_MAX / (n - i) + 1);
            size_t t = x[j];
            x[j] = x[i];
            x[i] = t;
        }
    }
}

bool prefers(size_t *prefs, size_t prtnr_id, size_t prop_id, size_t pref_size) {
    for (size_t i = 0; i < pref_size; ++i) {
        if (prefs[i] == prtnr_id) {
            return false;
        } else if(prefs[i] == prop_id) {
            return true;
        }
    }
}

void create_ppl(person *grp, size_t grp_size) {
    for (size_t i = 0; i < grp_size; ++i) {
        person prn;
        prn.id = i;
        prn.prtnr = grp_size + 1;
        prn.pref_ind = 0;
        prn.prefs = (size_t *) malloc(sizeof(size_t) * grp_size);

        for (size_t j = 0; j < grp_size; ++j) {
            prn.prefs[j] = j;
        }

        shuffle(prn.prefs, grp_size);
        grp[i] = prn;
    }
}

void stable_matching(person *men, person *women, size_t grp_size) {
    bool cont = true;
    while (cont) {
        for (size_t i = 0; i < grp_size; ++i) {
            if (men[i].prtnr == (grp_size + 1)) {
                size_t wmn_id = men[i].prefs[men[i].pref_ind];

                if (women[wmn_id].prtnr == (grp_size + 1)) {
                    men[i].prtnr = wmn_id;
                    women[wmn_id].prtnr = i;
                } else if(prefers(women[wmn_id].prefs, women[wmn_id].prtnr, i,
                                    grp_size)) {
                    men[women[wmn_id].prtnr].prtnr = grp_size + 1;
                    women[wmn_id].prtnr = i;
                    men[i].prtnr = wmn_id;
                }

                men[i].pref_ind++;
            }
        }

        cont = false;
        for (size_t i = 0; i < grp_size; ++i) {
            if (men[i].prtnr == (grp_size + 1)) {
                cont = true;
                break;
            }
        }
    }
}

void kill(person *grp, size_t grp_size) {
    for (size_t i = 0; i < grp_size; ++i) {
        free(grp[i].prefs);
    }
}

int main() {
    int grp_size = 5;
    person men[grp_size], women[grp_size];

    create_ppl(men, grp_size);
    create_ppl(women, grp_size);

    stable_matching(men, women, grp_size);

    for (size_t i = 0; i < grp_size; ++i) {
        printf("preferences of man %zu \n", i);
        for (size_t j = 0; j < grp_size; ++j) {
            printf("%zu \n", men[i].prefs[j]);
        }
    }

    for (size_t i = 0; i < grp_size; ++i) {
        printf("preferences of woman %zu \n", i);
        for (size_t j = 0; j < grp_size; ++j) {
            printf("%zu \n", women[i].prefs[j]);
        }
    }

    for (size_t i = 0; i < grp_size; ++i) {
        printf("partners of man %zu is woman %zu\n", i, men[i].prtnr);
    }

    kill(men, grp_size);
    kill(women, grp_size);

    return 0;
}
