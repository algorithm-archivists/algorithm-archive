#include<stdio.h>
#define size 256

int queue[size];
int front = 0;
int item_number = 0;

int check_full(){
    if(item_number == size){
        printf("queue is full\n");
        return 1;
    }
    else
        return 0;
}

int push(int item){
    check_full();
    queue[front + item_number] = item;
    item_number++;
}

int dequeue(){
    check_full();
    int item;
    item = queue[front];
    front++;
    return item;
}

int front_of_queue(){
    check_full();
    return queue[front];
}

int main(){
    push(5);
    push(7);
    push(3);
    push(6);
    dequeue();
    printf("%d\n",front_of_queue());
}
