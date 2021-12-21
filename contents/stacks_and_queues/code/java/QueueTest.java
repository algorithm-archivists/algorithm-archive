import java.util.List;
import java.util.ArrayList;

public class QueueTest {
    
    public static void main(String[] args) {
	IQueue<Integer> intQueue = new Queue<>();

	intQueue.enqueue(4);
	intQueue.enqueue(5);
	intQueue.enqueue(9);
	
	System.out.println(intQueue.dequeue());
	System.out.println(intQueue.size());
	System.out.println(intQueue.front());
    }

}


interface IQueue<T> {

   /*
    * 'dequeue' removes the first element from the queue and returns it
    */
    T dequeue();

   /*
    * 'enqueue' adds an element at the end of the queue and returns the new size
    */
    int enqueue(T element);
    

   /*
    * 'size' returns the size of the queue
    */
    int size();

   /*
    * 'front' returns the first element of the queue without removing it
    */
    T front();
}


class Queue<T> implements  IQueue<T> {

    private List<T> list;

    public Queue() {
        this.list = new ArrayList<>();
    }

    public T dequeue() {
        return this.list.remove(0);
    }

    public int enqueue(T element) {
        this.list.add(element);
        return this.size();
    }

    public int size() {
        return this.list.size();
    }

    public T front() {
        return this.list.get(0);
    }

}
