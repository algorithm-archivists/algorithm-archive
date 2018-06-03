// Made by Guston and edited by Gathros
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

struct tree {
    struct tree* left;
    struct tree* right;

    int count;
    char value;
};

struct bitstring_builder {
    char str[257];
    int next_index;
};

struct codebook {
    char* codes[256];
};

struct heap {
    struct tree** data;
    int length;
    int capacity;
};

bool is_leaf(const struct tree* t) {
    return !t->left && !t->right;
}

void swap(struct tree** lhs, struct tree** rhs) {
    struct tree* tmp = *lhs;
    *lhs = *rhs;
    *rhs = tmp;
}

/* The two concat functions are horribly inefficient */
void concat(char** dst, const char* src) {
    int dst_len = strlen(*dst);
    int src_len = strlen(src);
    *dst = realloc(*dst, src_len + dst_len + 1);
    strcat(*dst, src);
}

void concat_char(char** dst, char c) {
    int len = strlen(*dst);
    *dst = realloc(*dst, len + 2);
    (*dst)[len] = c;
    (*dst)[len + 1] = '\0';
}

char* duplicate(const char* src) {
    int length = strlen(src);
    char* dst = malloc(length + 1);
    memcpy(dst, src, length + 1);
    return dst;
}

void heap_push(struct heap* heap, struct tree* value) {
    if (heap->capacity == heap->length) {
        heap->capacity = heap->capacity == 0 ? 4 : heap->capacity * 2;
        heap->data = realloc(heap->data, heap->capacity * sizeof(struct tree*));
    }
    heap->data[heap->length++] = value;

    int index = heap->length - 1;
    while (index) {
        int parent_index = (index - 1) / 2;
        if (heap->data[parent_index]->count <= heap->data[index]->count) {
            break;
        }

        swap(&heap->data[parent_index], &heap->data[index]);
        index = parent_index;
    }
}

struct tree* heap_pop(struct heap* heap) {
    if (!heap->length) {
        return NULL;
    }

    struct tree* result = heap->data[0];
    swap(&heap->data[0], &heap->data[--heap->length]);

    int index = 0;
    for (;;) {
        int target = index;
        int left = 2 * index + 1;
        int right = left + 1;

        if (left < heap->length &&
                heap->data[left]->count < heap->data[target]->count) {
            target = left;
        }

        if (right < heap->length &&
                heap->data[right]->count < heap->data[target]->count) {
            target = right;
        }

        if (target == index) {
            break;
        }

        swap(&heap->data[index], &heap->data[target]);
        index = target;
    }

    return result;
}

void heap_free(struct heap* heap) {
    free(heap->data);
}

struct tree* generate_tree(const char* str) {
    int counts[256] = { 0 };

    for (; *str != '\0'; ++str) {
        counts[(unsigned char)*str] += 1;
    }

    struct heap heap = { 0 };
    for (int i = 0; i < sizeof(counts) / sizeof(int); ++i) {
        if (counts[i]) {
            struct tree* tree = calloc(1, sizeof(struct tree));
            tree->value = (char)i;
            tree->count = counts[i];
            heap_push(&heap, tree);
        }
    }

    while (heap.length > 1) {
        struct tree* left = heap_pop(&heap);
        struct tree* right = heap_pop(&heap);
        struct tree* branch = calloc(1, sizeof(struct tree));
        branch->count = left->count + right->count;
        branch->left = left;
        branch->right = right;
        heap_push(&heap, branch);
    }

    struct tree* root = heap_pop(&heap);
    heap_free(&heap);
    return root;
}

void tree_free(struct tree* tree) {
    if (!tree) return;
    tree_free(tree->left);
    tree_free(tree->right);
    free(tree);
}

void codebook_recurse(const struct tree* tree,
                        struct bitstring_builder* builder,
                        struct codebook* codebook) {
    if (!tree) {
        return;
    }

    if (is_leaf(tree)) {
        builder->str[builder->next_index] = '\0';
        codebook->codes[(unsigned char)tree->value] = duplicate(builder->str);
        return;
    }

    builder->str[builder->next_index++] = '0';
    codebook_recurse(tree->left, builder, codebook);
    builder->next_index -= 1;

    builder->str[builder->next_index++] = '1';
    codebook_recurse(tree->right, builder, codebook);
    builder->next_index -= 1;
}

struct codebook generate_codebook(const struct tree* tree) {
    struct codebook codebook = { .codes = { 0 } };
    struct bitstring_builder builder = { .str = { 0 }, .next_index = 0 };
    codebook_recurse(tree, &builder, &codebook);
    return codebook;
}

void codebook_free(struct codebook* codebook) {
    int size = sizeof(codebook->codes) / sizeof(codebook->codes[0]);
    for (int i = 0; i < size; ++i) {
        free(codebook->codes[i]);
    }
}

const char* get_code(const struct codebook* codebook, char c) {
    return codebook->codes[(unsigned char)c];
}

char* encode(const char* input, struct tree** huffman_tree,
                struct codebook* codebook) {
    *huffman_tree = generate_tree(input);
    *codebook = generate_codebook(*huffman_tree);

    char* result = duplicate(get_code(codebook, *input));
    int result_length = strlen(result);
    int result_capacity = result_length;

    input += 1;

    for (; *input; ++input) {
        concat(&result, get_code(codebook, *input));
    }

    return result;
}

const char* decode_recurse(const char* input, const struct tree* tree,
                            char** result) {
    if (!tree) {
        return input;
    }

    if (is_leaf(tree)) {
        concat_char(result, tree->value);
        return input;
    }

    if (*input == '0') {
        return decode_recurse(input + 1, tree->left, result);
    } else {
        return decode_recurse(input + 1, tree->right, result);
    }
}

char* decode(const char* input, const struct tree* tree) {
    char* result = calloc(1, 1);
    do {
        input = decode_recurse(input, tree, &result);
    } while (*input);
    return result;
}

int main() {
    struct tree* tree;
    struct codebook codebook;

    char* encoded = encode("bibbity bobbity", &tree, &codebook);
    char* decoded = decode(encoded, tree);

    printf("Codebook:\n");
    for (int i = 0; i < 256; ++i) {
        if (codebook.codes[i]) {
            printf("%c %s\n", (char)i, codebook.codes[i]);
        }
    }

    printf("%s\n", encoded);
    printf("%s\n", decoded);

    tree_free(tree);
    codebook_free(&codebook);
    free(encoded);
    free(decoded);
    return 0;
}
