#include<stdio.h>

int queue[256];
int front = 0;
int item_number = 0;

int push(int item){
    queue[front + item_number] = item;
    item_number++;
}

int dequeue(){
    int item;
    item = queue[front];
    front++;
    return item;
}

int front_of_queue(){
    return queue[front];
}

int main(){
    push(5);
    push(7);
    push(3);
    dequeue();
    printf("%d\n",front_of_queue());
}
