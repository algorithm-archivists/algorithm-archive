#include<iostream>
#include<memory>
#include<cassert>

namespace my {
    /**
     * implementation using linked list
     * [value][next] -> [value][next] -> ... -> [value][next]
     *  (front Node)   (intermediat Nodes)     (rear Node)
     */
    template<typename T>
    struct Node {
        /**
        * next: will store right Node address
        */
        T value;
        std::shared_ptr<Node<T>> next;
        Node(const T& V) : value(V) { }
    };

    template<typename T>
    class queue {
    private:
        /**
         * front_pointer:  points to left most node
         * count: keeps track of current number of elements present in queue
         * rear_pointer:  points to most recent Node added into the queue, which is right most Node
         */
        std::shared_ptr<Node<T>> front_pointer;
        std::shared_ptr<Node<T>> rear_pointer;
        size_t count;
    public:
        queue() : count(0ULL) { }

        void enqueue(const T& element) {
            auto new_node = std::make_shared<Node<T>>(element);
            if (count > 0) {
                rear_pointer->next = new_node;
                rear_pointer = new_node;
            } else {
                rear_pointer = front_pointer = new_node;
            }
            count = count + 1;
        }

        void dequeue() {
            if (count > 1) {
                front_pointer = front_pointer->next;
                count = count - 1;
            } else if (count == 1) {
                front_pointer.reset();
                rear_pointer.reset();
                count = count - 1;
            }
        }

        T& front() {
            assert(count > 0 && "calling front on an empty queue");
            return front_pointer->value;
        }

        T const& front() const {
            assert(count > 0 && "calling front on an empty queue");
            return front_pointer->value;
        }

        size_t size() const { return count; }

        bool empty() const { return count == 0; }

        ~queue() {
            while (front_pointer.get() != nullptr) {
                front_pointer = front_pointer->next;
            }
        }
    };
}

int main() {
  my::queue<int> intQueue;
  intQueue.enqueue(4);
  intQueue.enqueue(5);
  intQueue.enqueue(9);

  int frontElement = intQueue.front();
  intQueue.dequeue();
  std::cout << frontElement << '\n';
  std::cout << intQueue.size() << '\n';
  std::cout << intQueue.front() << '\n';
  return 0;
}
