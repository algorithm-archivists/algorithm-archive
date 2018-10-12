#ifndef UTILITY_H
#define UTILITY_H

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

struct stack {
    void **data;
    size_t top, capacity, size;
};

struct queue {
    void **data;
    size_t front, back, capacity;
};

struct stack get_stack(size_t size) {
    struct stack stk;

    stk.data = malloc(4 * size);
    stk.capacity = 4;
    stk.top = 0;

    return stk;
}

bool stack_empty(struct stack *stk) {
    return stk->top == 0;
}

void stack_push(struct stack *stk, void *element) {
    if (stk->top == stk->capacity) {
        stk->capacity *= 2;
        stk->data = realloc(stk->data, stk->capacity * sizeof(stk->data[0]));
    }

    stk->data[stk->top++] = element;
}

void *stack_pop(struct stack *stk) {
    if (stack_empty(stk)) {
        return NULL;
    }

    return stk->data[--stk->top];
}

void free_stack(struct stack stk) {
    free(stk.data);
}

struct queue get_queue(size_t size) {
    struct queue q;

    q.data = calloc(4, size);
    q.front = 0;
    q.back = 0;
    q.capacity = 4;

    return q;
}

bool queue_empty(struct queue *q) {
    return q->front == q->back;
}

void queue_resize(struct queue *q) {
    size_t size = sizeof(q->data[0]);
    void **tmp = calloc((q->capacity * 2), size);
    memcpy(tmp, q->data + q->front, (q->capacity - q->front) * size);
    memcpy(tmp + q->capacity - q->front, q->data, (q->front - 1) * size);

    free(q->data);

    q->data = tmp;
    q->back = q->capacity - 1;
    q->front = 0;
    q->capacity *= 2;
}

void enqueue(struct queue *q, void *element) {
    if (q->front == (q->back + 1) % q->capacity) {
        queue_resize(q);
    }

    q->data[q->back] = element;
    q->back = (q->back + 1) % q->capacity;
}

void *dequeue(struct queue *q) {
    if (queue_empty(q)) {
        return NULL;
    }

    void *ret = q->data[q->front];
    q->front = (q->front + 1) % q->capacity;

    return ret;
}

void free_queue(struct queue q) {
    free(q.data);
}

#endif //UTILITY_H
