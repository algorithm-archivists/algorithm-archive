#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node {
    struct node *children;
    int children_num;
    int ID;
} node;

typedef struct node_list {
    node n;
    struct node_list *last_list, *next_list;
} node_list;

typedef struct node_points {
    node_list *start_point, *end_point;
} node_points;

void push(node_points *np, node n) {
    node_list *temp = (node_list*)malloc(sizeof(node_list));
    temp->n = n;
    temp->last_list = temp->next_list = NULL;

    if (!np->end_point) {
        np->start_point = temp;
    } else {
        np->end_point->next_list = temp;
        temp->last_list = np->end_point;
    }

    np->end_point = temp;
}

void stack_pop(node_points *np) {
    node_list *temp;
    temp = np->end_point;

    if (temp) {
        np->end_point = temp->last_list;
        if (!np->end_point) {
            np->start_point = NULL;
        }

        free(temp);
    }
}

void queue_pop(node_points *np) {
    node_list *temp;
    temp = np->start_point;
    if (temp) {
        np->start_point = temp->next_list;
        if (!np->start_point) {
            np->end_point = NULL;
        }

        free(temp);
    }
}

void create_tree(node *n, int num_row, int num_child) {
    n->ID = num_row;
    if (num_row == 0) {
        return;
    }

    n->children = (node *)malloc(num_child * sizeof(*n->children));
    n->children_num = num_child;
    for (int i = 0; i < num_child; ++i) {
        node child;
        create_tree(&child, num_row - 1, num_child);
        *(n->children + i) = child;
    }
}

void DFS_recursive(node n) {
    printf("%d\n", n.ID);
    if (!n.children) {
        return;
    }

    for (int i = 0; i < n.children_num; ++i) {
        DFS_recursive(n.children[i]);
    }
}

void DFS_stack(node n) {
    node_points stack;
    memset(&stack, 0, sizeof(node_points));
    push(&stack, n);
    node temp;

    while (stack.start_point != NULL) {
        temp = stack.end_point->n;
        printf("%d\n", temp.ID);
        stack_pop(&stack);
        for (int i = 0; i < temp.children_num; ++i) {
            if (!temp.children) {
                break;
            }

            push(&stack, temp.children[i]);
        }
    }
}

void BFS_queue(node n) {
    node_points queue;
    memset(&queue, 0, sizeof(node_points));
    push(&queue, n);
    node temp;

    while (queue.start_point != NULL) {
        temp = queue.start_point->n;
        printf("%d\n", temp.ID);
        queue_pop(&queue);
        for (int i = 0; i < temp.children_num; ++i) {
            if (!temp.children) {
                break;
            }

            push(&queue, temp.children[i]);
        }
    }
}

void destroy_tree(node *n) {
    if (n->ID == 0) {
        return;
    }

    for (int i = 0; i < n->children_num; ++i) {
        destroy_tree(n->children + i);
    }

    free(n->children);
}

int main() {
    node root;
    create_tree(&root, 3, 3);
    DFS_recursive(root);
    //DFS_stack(root);
    //BFS_queue(root);
    destroy_tree(&root);

    return 0;
}

