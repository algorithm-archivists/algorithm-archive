#include<iostream>
#include<cassert>
#include<memory>

namespace my {
    /**
     * implementation using linked list
     * [value][next] -> [value][next] -> ... -> [value][next]
     * (top Node)      (intermediat Nodes)
     * left most Node represents top element of stack
     */
    template<typename T>
    struct Node {
        /**
        * next: will store right Node address
        */
        T value;
        std::unique_ptr<Node<T>> next;
        Node(const T& V) : value(V) { }
    };

    template<typename T>
    class stack {
    private:
        /**
         * top_pointer: points to left most node
         * count: keeps track of current number of elements present in stack
         */
        std::unique_ptr<Node<T>> top_pointer;
        size_t count;
    public:
        stack() : count(0ULL) { }

        void push(const T& element) {
            auto new_node = std::make_unique<Node<T>>(element);
            new_node->next = std::move(top_pointer);
            top_pointer = std::move(new_node);
            count = count + 1;
        }

        void pop() {
            if (count > 0) {
                top_pointer = std::move(top_pointer->next);
                count = count - 1;
            }
        }

        T& top() {
            assert(count > 0 and "calling top() on an empty stack");
            return top_pointer->value;
        }
        // returning mutable reference can very be usefull if someone wants to modify top element

        T const& top() const {
            assert(count > 0 and "calling top() on an empty stack");
            return top_pointer->value;
        }

        size_t size() const { return count; }

        bool empty() const { return count == 0; }

        ~stack() {
            while (top_pointer.get() != nullptr) {
                top_pointer = std::move(top_pointer->next);
            }
        }
    };
}

int main() {
  my::stack<int> intStack;

  intStack.push(4);
  intStack.push(5);
  intStack.push(9);

  int topElement = intStack.top();
  intStack.pop();
  std::cout << topElement << '\n';
  std::cout << intStack.size() << '\n';
  std::cout << intStack.top() << '\n';
  return 0;
}
