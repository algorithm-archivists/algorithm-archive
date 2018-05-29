#include "utility.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

struct node {
    struct node *children;
    size_t children_size;
    int id;
};

struct node create_tree(int rows, size_t num_children) {
    struct node n = {NULL, 0, rows};

    if (rows > 0) {
        n.children = (struct node*)malloc(num_children * sizeof(struct node));
        n.children_size = num_children;
        for (size_t i = 0; i < num_children; ++i) {
            n.children[i] = create_tree(rows - 1, num_children);
        }
    }

    return n;
}

void destroy_tree(struct node n) {
    if (n.id > 0) {
        for (size_t i = 0; i < n.children_size; ++i) {
            destroy_tree(n.children[i]);
        }

        free(n.children);
    }
}

void dfs_recursive(struct node n) {
    printf("%d\n", n.id);

    if (n.children) {
        for (size_t i = 0; i < n.children_size; ++i) {
            dfs_recursive(n.children[i]);
        }
    }
}

void dfs_recursive_postorder(struct node n) {
    for (size_t i = 0; i < n.children_size; ++i) {
        dfs_recursive_postorder(n.children[i]);
    }

    printf("%d\n", n.id);
}

void dfs_recursive_inorder_btree(struct node n) {
    switch (n.children_size) {
    case 2:
        dfs_recursive_inorder_btree(n.children[0]);
        printf("%d\n", n.id);
        dfs_recursive_inorder_btree(n.children[1]);
        break;
    case 1:
        dfs_recursive_inorder_btree(n.children[0]);
        printf("%d\n", n.id);
        break;
    case 0:
        printf("%d\n", n.id);
        break;
    default:
        printf("This is not a binary tree.\n");
        break;
    }
}

void dfs_stack(struct node n) {
    struct stack stk = get_stack(sizeof(struct node*));
    stack_push(&stk, &n);
    struct node *tmp;

    while (!stack_empty(&stk)) {
        tmp = (struct node*)stack_pop(&stk);
        if (!tmp) {
            break;
        }

        printf("%d\n", tmp->id);
        for (size_t i = 0; i < tmp->children_size; ++i) {
            stack_push(&stk, &tmp->children[i]);
        }
    }

    free_stack(stk);
}

void bfs_queue(struct node n) {
    struct queue q = get_queue(sizeof(struct node*));
    enqueue(&q, &n);
    struct node *tmp;

    while (!queue_empty(&q)) {
        tmp = (struct node*)dequeue(&q);
        if (!tmp) {
            break;
        }

        printf("%d\n", tmp->id);
        for (size_t i = 0; i < tmp->children_size; ++i) {
            enqueue(&q, &tmp->children[i]);
        }
    }

    free_queue(q);
}

int main() {
    struct node root = create_tree(3, 3);
    bfs_queue(root);
    destroy_tree(root);

    return 0;
}
