import numpy as np

class Node:
    def __init__(self, val):
        self.value = val
        self.next = None

class Stack():
    def __init__(self):
        self.stack_list = None
    
    def push(self, val):
        if self.stack_list == None:
            self.stack_list=  Node(val)
        else :
            temp = Node(val)
            temp.next = self.stack_list
            self.stack_list = temp
            
    def pop(self):
        if self.stack_list == None:
            print ("Stack is Empty")
            return None
        else:
            pop_val = self.stack_list.value
            self.stack_list = self.stack_list.next
            print("you just pop ", pop_val)
            return pop_val
    
    def get_top(self):
        return self.stack_list.value
    
    def print(self):
        print ("Current stack list : ")
        temp = self.stack_list
        while(temp.next != None):
            print(temp.value)
            temp = temp.next
        print(temp.value)
        print("========================")

class Queue():
    def __init__(self):
        self.queue_list=None
        
    def enqueue(self,val):
        temp = Node(val)
        if self.queue_list == None:
            self.queue_list = temp
        else:
            curr = self.queue_list
            while(curr.next != None):
                curr = curr.next
            curr.next = temp
            
    def dequeue(self):
        if self.queue_list == None:
            return None
        else:
            temp = self.queue_list
            dequeue_val = self.queue_list.value
            self.queue_list = temp.next         
            return dequeue_val
        
    def get_front(self):
        curr = self.queue_list
        while(curr.next != None):
            curr = curr.next
        return curr.value
    
    def print(self):
        print("Current queue list = ")
        temp = self.queue_list
        while(temp.next != None):
            print(temp.value)
            temp = temp.next
        print (temp.value)
        print("========================")
    

example_stack = Stack()
example_stack.push(1)
example_stack.push(2)
example_stack.push(3)
example_stack.print()
example_stack.pop()
example_stack.push(4)
example_stack.print()
print ("current top => ",example_stack.get_top())

example_queue = Queue()
example_queue.enqueue(1)
example_queue.enqueue(2)
example_queue.enqueue(3)
example_queue.print()
example_queue.dequeue()
example_queue.enqueue(4)
example_queue.print()
print ("current front => ", example_queue.get_front())