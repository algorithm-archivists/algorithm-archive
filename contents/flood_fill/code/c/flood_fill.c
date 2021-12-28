#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct canvas {
    int max_x, max_y;
    int *data;
};

struct point {
    int x, y;
};

struct stack {
    size_t top, capacity;
    struct point *data;
};

struct queue {
    size_t front, back, capacity;
    struct point *data;
};

int inbounds(struct point p, struct canvas c) {
    return (p.x < 0 || p.y < 0 || p.y >= c.max_y || p.x >= c.max_x) ? 0 : 1;
}

int find_neighbors(struct canvas c, struct point p, int old_val, 
        struct point *neighbors) {
    int cnt = 0;
    struct point points[4] = {
        {p.x, p.y + 1},
        {p.x + 1, p.y},
        {p.x, p.y - 1},
        {p.x - 1, p.y}
    };

    for (int i = 0; i < 4; ++i) {
        if (inbounds(points[i], c) &&
                c.data[points[i].x + c.max_x * points[i].y] == old_val) {
            neighbors[cnt++] = points[i];
        }
    }

    return cnt;
}

struct stack get_stack() {
    struct stack stk;

    stk.data = malloc(4 * sizeof(struct point));
    stk.capacity = 4;
    stk.top = 0;

    return stk;
}

int stack_empty(struct stack stk) {
    return stk.top == 0;
}

void stack_push(struct stack *stk, struct point element) {
    if (stk->top == stk->capacity) {
        stk->capacity *= 2;
        stk->data = realloc(stk->data, stk->capacity * sizeof(stk->data[0]));
    }

    stk->data[stk->top++] = element;
}

struct point stack_pop(struct stack *stk) {
    return stk->data[--stk->top];
}

void free_stack(struct stack stk) {
    free(stk.data);
}

void stack_fill(struct canvas c, struct point p, int old_val, int new_val) {
    if (old_val == new_val) {
        return;
    }

    struct stack stk = get_stack();
    stack_push(&stk, p);

    while (!stack_empty(stk)) {
        struct point cur_loc = stack_pop(&stk);
        if (c.data[cur_loc.x + c.max_x * cur_loc.y] == old_val) {
            c.data[cur_loc.x + c.max_x * cur_loc.y] = new_val;

            struct point neighbors[4];
            int cnt = find_neighbors(c, cur_loc, old_val, neighbors);

            for (int i = 0; i < cnt; ++i) {
                stack_push(&stk, neighbors[i]);
            }
        }
    }

    free_stack(stk);
}

struct queue get_queue() {
    struct queue q;

    q.data = calloc(4, sizeof(struct point));
    q.front = 0;
    q.back = 0;
    q.capacity = 4;

    return q;
}

int queue_empty(struct queue q) {
    return q.front == q.back;
}

void enqueue(struct queue *q, struct point element) {
    if (q->front == (q->back + 1) % q->capacity) {
        size_t size = sizeof(q->data[0]);
        struct point *tmp = calloc((q->capacity * 2), size);
        memcpy(tmp, q->data + q->front, (q->capacity - q->front) * size);
        memcpy(tmp + q->capacity - q->front, q->data, (q->front - 1) * size);

        free(q->data);

        q->data = tmp;
        q->back = q->capacity - 1;
        q->front = 0;
        q->capacity *= 2;
    }

    q->data[q->back] = element;
    q->back = (q->back + 1) % q->capacity;
}

struct point dequeue(struct queue *q) {
    struct point ret = q->data[q->front];
    q->front = (q->front + 1) % q->capacity;

    return ret;
}

void free_queue(struct queue q) {
    free(q.data);
}

void queue_fill(struct canvas c, struct point p, int old_val, int new_val) {
    if (old_val == new_val) {
        return;
    }

    struct queue q = get_queue(sizeof(struct point *));
    enqueue(&q, p);

    while (!queue_empty(q)) {
        struct point cur_loc = dequeue(&q);
        if (c.data[cur_loc.x + c.max_x * cur_loc.y] == old_val) {
            c.data[cur_loc.x + c.max_x * cur_loc.y] = new_val;

            struct point neighbors[4];
            int cnt = find_neighbors(c, cur_loc, old_val, neighbors);

            for (int i = 0; i < cnt; ++i) {
                enqueue(&q, neighbors[i]);
            }
        }
    }

    free_queue(q);
}

void recursive_fill(struct canvas c, struct point p, int old_val,
        int new_val) {

    if (old_val == new_val) {
        return;
    }

    c.data[p.x + c.max_x * p.y] = new_val;

    struct point neighbors[4];
    int cnt = find_neighbors(c, p, old_val, neighbors);

    for (int i = 0; i < cnt; ++i) {
        recursive_fill(c, neighbors[i], old_val, new_val);
    }
}

int grid_cmp(int *a, int *b, int size) {
    for (int i = 0; i < size; ++i) {
        if (a[i] != b[i]) {
            return 0;
        }
    }

    return 1;
}

int main() {
    int grid[25] = {
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        1, 1, 1, 1, 1,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0
    };
    int grid1[25] = {
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        1, 1, 1, 1, 1,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0
    };
    int grid2[25] = {
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        1, 1, 1, 1, 1,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0
    };
    int answer_grid[25] = {
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0
    };

    struct canvas c = {5, 5, grid};
    struct canvas c1 = {5, 5, grid1};
    struct canvas c2 = {5, 5, grid2};

    struct point start_loc = {0, 0};

    int pass_cnt = 0;

    recursive_fill(c, start_loc, 0, 1);
    pass_cnt += grid_cmp(grid, answer_grid, 25);

    stack_fill(c1, start_loc, 0, 1);
    pass_cnt += grid_cmp(grid1, answer_grid, 25);

    queue_fill(c2, start_loc, 0, 1);
    pass_cnt += grid_cmp(grid2, answer_grid, 25);

    printf("Test Summary: | Pass\tTotal\n");
    printf("Fill Methods  |\t%d\t3\n", pass_cnt);

    return 0;
}

