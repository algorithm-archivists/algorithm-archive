// Submitted by Gathros
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node{
    struct node *children; // I chose a pointer to create a dynamic array
    int children_num;
    int ID;
} node;

// There is no stack or queue in c so I created my own
typedef struct node_list{
    node n;
    struct node_list *last_list, *next_list;
} node_list;

typedef struct node_points{
    node_list *start_point, *end_point;
} node_points;

void push(node_points *np, node *n){
    // Adding node into a queue or a stack
    node_list *temp = (node_list*)malloc(sizeof(node_list));
    temp->n = *n;
    temp->last_list = temp->next_list = NULL;
    
    if(!np->end_point){
        np->start_point = temp;
    }else{
        np->end_point->next_list = temp;
        temp->last_list = np->end_point;
    }
    np->end_point = temp;
}

void stack_pop(node_points *np){
    // Removing the last node_list of the stack
    node_list *temp;
    temp = np->end_point;
    if(temp){
        np->end_point = temp->last_list;
        if(!np->end_point){
            np->start_point = NULL;
        }
        free(temp);
    }
}

void queue_pop(node_points *np){
    // Removing the first node_list of the queue
    node_list *temp;
    temp = np->start_point;
    if(temp){
        np->start_point = temp->next_list;
        if(!np->start_point){
            np->end_point = NULL;
        }
        free(temp);
    }
}

void create_tree(node *n, int num_row, int num_child) {
    n->ID = num_row;
    if(num_row == 0){
        return;
    }

    // Creating children
    // Using malloc to make an array of nodes with size num_child
    n->children = (node *)malloc(num_child*sizeof(*n->children));
    // When an array is made into a pointer you can't get it's size later
    n->children_num = num_child;
    for(int i = 0; i < num_child; ++i){
        node child;
        create_tree(&child, num_row - 1, num_child);
        *(n->children + i) = child;
    }
}

void DFS_recursive(node *n){
    printf("%d\n", n->ID);
    
    // Checking if the node's children exist
    if(!n->children){
        return;
    }
    
    for(int i = 0; i < n->children_num; ++i){
        DFS_recursive((n->children + i));
    }
}

void DFS_stack(node *n){
    // Creating a stack and then setting its value to 0
    node_points stack;
    memset(&stack, 0, sizeof(node_points));
    push(&stack, n);
    node temp;

    while(stack.start_point != NULL){
        temp = stack.end_point->n;
        printf("%d\n", temp.ID);
        stack_pop(&stack);
        for(int i=0; i < temp.children_num; ++i){
            // Checking if the node has any children
            if(!temp.children){
                break;
            }
            push(&stack, temp.children + i);
        }
    }
}

void BFS_queue(node *n){
    // Creating a queue and then setting its value to 0
    node_points queue;
    memset(&queue, 0, sizeof(node_points));
    push(&queue, n);
    node temp;

    while(queue.start_point != NULL){
        temp = queue.start_point->n;
        printf("%d\n", temp.ID);
        queue_pop(&queue);
        for(int i = 0; i < temp.children_num; ++i){
            // Checking if the node has any children
            if(!temp.children){
                break;
            }
            push(&queue, temp.children + i);
        }
    }
}

void destroy_tree(node *n){
    // This function is for cleaning up all the nodes
    if(n->ID == 0){
        return;
    }
    
    for(int i = 0; i < n->children_num; ++i){
        destroy_tree(n->children + i);
    }
    free(n->children);
}

int main() {
    node root;
    create_tree(&root, 3, 3);
    //DFS_recursive(&root);
    //DFS_stack(&root);
    BFS_queue(&root);
    destroy_tree(&root);
    
    return 0;
}

