#include<iostream>
using namespace std;


namespace my {
    /**
     *implementation using parameterised linked list
     * [value][next] -> [value][next] -> ... -> [value][next] -> [value][next]
     *  (front Node)   (intermediat elements)    (rear Node)      (dummy Node)
     */
    template<typename T>
    struct Node {
        /**
        * next: will store next Node(right) address
        */
        T value;
        Node<T>* next;
        Node(const T& V) : value(V), next(nullptr) { }
    };

    template<typename T>
    class queue {
    private:
        /**
         * variabel: _front points to left most node , right most  Node is dummy Node
         * count: will keep track of current number of elements present in stack excluding dummy Node
         */
        Node<T>* _front;
        Node<T>* rear;
        size_t count;
    public:
        queue() : _front(nullptr), rear(nullptr), count(0ULL) {
            _front = rear = new Node<T>(0); // creating a dummy Node
        }

        void push(const T& element) {
            Node<T>* new_node = new Node<T>(element);  // create New Node
            if (count > 0) {
                new_node->next = rear->next; // make buffer point to dummy Node
                rear->next = new_node;       // make rear Node point to buffer
                rear = new_node;             // make rear pointer to point to buffer
            } else {
                new_node->next = rear;
                rear = _front = new_node;
            }
            count = count + 1;
        }

        void dequeue() {
            if (count > 0) {
                Node<T>* buffer = _front;
                _front = _front->next;
                count = count - 1;
                delete buffer;
            }
            rear = (count != 0) ? rear : _front;
        }

        T& front() const { return _front->value; }
        // returning reference can very usefull if someone wants to modify _front element

        size_t size() const { return count; }

        bool empty() const { return count == 0; }

        ~queue() {
            for (Node<T>* pointer = _front; pointer != nullptr;) {
                Node<T>* buffer = pointer;
                pointer = pointer->next;
                delete buffer;
            }
        }
    };
}

int main() {
    my::queue<int> Q;

    Q.push(0);
    Q.push(1);
    Q.push(2);
    Q.push(3);

    cout << "count: " << Q.size() << endl;

    Q.front() = 10;

    while (Q.empty() != true) {
        cout << "element: " << Q.front() << endl;
        Q.dequeue();
    }

    Q.push(3);
    Q.push(6);
    cout << "count: " << Q.size() << endl;
    while (Q.empty() != true) {
        cout << "element: " << Q.front() << endl;
        Q.dequeue();
    }
    return 0;
}

