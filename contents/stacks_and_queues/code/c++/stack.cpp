
#include<iostream>
using namespace std;

namespace my {
    /**
     *implementation using parameterised linked list
     * [value][next] -> [value][next] -> ... -> [value][next]
     * (top Node)      (intermediat Nodes)      (dummy Node)
     * left most Node represents_top element of stack
     * right most Node is a dummy Node
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
    class stack {
    private:
        /**
         * variabel:_top points to left most node , right most  Node is dummy Node
         * count: will keep track of current number of elements present in stack excluding dummy Node
         */
        Node<T>* _top;
        size_t count;
    public:
        stack() : _top(nullptr), count(0ULL) {
            _top = new Node<T>(0); // creating a dummy node
        }

        void push(const T& element) {
            Node<T>* buffer = new Node<T>(element);
            buffer->next = _top;
            _top = buffer;
            count = count + 1;
        }

        void pop() {
            if (count > 0) {
                Node<T>* buffer = _top;
                _top = _top->next;
                count = count - 1;
                delete buffer;
            }
        }

        T& top() const { return _top->value; }
        // returning reference can very usefull if someone wants to modify top element

        size_t size() const { return count; }

        bool empty() const { return count == 0; }

        ~stack() {
            for (Node<T>* pointer = _top; pointer != nullptr;) {
                Node<T>* buffer = pointer;
                pointer = pointer->next;
                delete buffer;
            }
        }
    };
}

int main() {
    my::stack<int> S;
    S.push(0);
    S.push(1);
    S.push(2);
    S.push(3);
    cout << "size: " << S.size() << endl;
    S.top() = 10;
    while (S.empty() != true) {
        cout << "element: " << S.top() << endl;
        S.pop();
    }
    return 0;
}
