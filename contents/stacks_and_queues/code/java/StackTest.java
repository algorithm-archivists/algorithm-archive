import java.util.List;
import java.util.ArrayList;


public class StackTest {
    
    public static void main(String[] args) {
	IStack<Integer> intStack = new Stack<>();

	intStack.push(4);
	intStack.push(5);
	intStack.push(9);

	System.out.println(intStack.pop());
	System.out.println(intStack.size());
	System.out.println(intStack.top());
    }

}


interface IStack<T> {
   /*
    * 'pop' removed the last element from the stack and returns it
    */
    T pop();

   /*
    * 'push' adds an element to at the end of the stack and returns the new size
    */
    int push(T element);

   /*
    * 'size' returns the length of the stack
    */
    int size();

   /*
    * 'top' returns the first element of the stack
    */
    T top();
}


class Stack<T> implements IStack<T> {
    
    private List<T> list;

    public Stack() {
        this.list = new ArrayList<>();
    }

    public T pop() {
        return this.list.remove(this.size() - 1);
    }

    public int push(T element) {
        this.list.add(element);
        return this.size();
    }

    public int size() {
        return this.list.size();
    }

    public T top() {
        return this.list.get(this.size() - 1);
    }

}


