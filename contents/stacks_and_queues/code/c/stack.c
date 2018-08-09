#include<stdio.h>
#define size 256

int stack[size];
int top = 0;

int check_full(){    
    if(top == size){
        printf("stack is full\n");
        return 1;
    }
    else
        return 0;

}

int push(int item){
    check_full();
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
    check_full();
    return stack[top];
}

int main(){
    push(3);
    push(5);
    push(9);
    printf("%d\n",peek());
}
