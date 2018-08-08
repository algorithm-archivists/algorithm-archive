#include<stdio.h>

int stack[256];
int top = -1;

int push(int item){
    top++;
    stack[top] = item;
}

int pop(){
    int item;
    item = stack[top];
    top--;
    return item;
}

int peek(){
    return stack[top];
}

int main(){
    push(3);
    push(5);
    push(9);
    pop();
    printf("%d\n",peek());
}
